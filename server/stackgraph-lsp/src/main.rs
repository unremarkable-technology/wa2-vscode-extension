use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use tokio::io::{stdin, stdout};
use tokio::sync::Notify;
use tokio::time::sleep;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct DocumentState {
	text: String,
	dirty: bool,
}

struct Backend {
	client: Client,
	docs: Arc<Mutex<HashMap<Url, DocumentState>>>,
	notify: Arc<Notify>,
	// atomic change marker so analysis can see if a new event arrived
	generation: Arc<AtomicU64>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
	/// being asked to initialize, what capabilities do we have
	async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
		Ok(InitializeResult {
			capabilities: ServerCapabilities {
				text_document_sync: Some(TextDocumentSyncCapability::Kind(
					TextDocumentSyncKind::INCREMENTAL,
				)),
				..ServerCapabilities::default()
			},
			server_info: Some(ServerInfo {
				name: "WA2".to_string(),
				version: None,
			}),
		})
	}

	/// initialized done, spawns the analyser
	async fn initialized(&self, _params: InitializedParams) {
		self.client
			.log_message(MessageType::INFO, "WA2 LSP server initialized")
			.await;

		// clone the arcs protecting resources
		let client = self.client.clone();
		let docs = self.docs.clone();
		let notify = self.notify.clone();
		let generation = self.generation.clone();

		// listen for work via notify
		tokio::spawn(async move {
			analyser_loop(client, docs, notify, generation).await;
		});
	}

	/// graceful shutdown request
	async fn shutdown(&self) -> Result<()> {
		Ok(())
	}

	/// new document opened
	/// capture the event, but do no work on main thread
	async fn did_open(&self, params: DidOpenTextDocumentParams) {
		// capture that a new document has been opened
		let uri = params.text_document.uri;
		let text = params.text_document.text;

		// locking docs to add
		{
			let mut docs = self.docs.lock().unwrap();
			docs.insert(uri.clone(), DocumentState { text, dirty: true });
		}

		// bump generation so analysis can see a new event happened
		self.generation.fetch_add(1, Ordering::Relaxed);

		// tell background analyser there is work todo
		self.notify.notify_one();

		let message = format!("doc_open: {}", uri);
		self.client.log_message(MessageType::INFO, message).await;
	}

	/// document changed
	/// capture the event, but do no work on main thread
	async fn did_change(&self, params: DidChangeTextDocumentParams) {
		let uri = params.text_document.uri;
		let changes = params.content_changes;

		// TECHDEBT: not efficient, ASSUME full text sync
		// take the last change's text as the full document.
		let new_text = changes.last().map(|c| c.text.clone()).unwrap_or_default();

		// locking docs to update
		{
			let mut docs = self.docs.lock().unwrap();
			let entry = docs.entry(uri.clone()).or_insert(DocumentState {
				text: String::new(),
				dirty: false,
			});

			entry.text = new_text;
			entry.dirty = true;
		}

		// bump generation so analysis can see a new event happened
		self.generation.fetch_add(1, Ordering::Relaxed);

		// tell background analyser there is work todo
		self.notify.notify_one();

		let message = format!("doc_change: {}", uri);
		self.client.log_message(MessageType::INFO, message).await;
	}

	/// document saved, re-run analysis on save
	/// capture the event, but do no work on main thread
	async fn did_save(&self, params: DidSaveTextDocumentParams) {
		let uri = params.text_document.uri;

		// locking docs to update
		{
			let mut docs = self.docs.lock().unwrap();
			if let Some(doc) = docs.get_mut(&uri) {
				doc.dirty = true;
			}
		}

		// bump generation so analysis can see a new event happened
		self.generation.fetch_add(1, Ordering::Relaxed);

		// tell background analyser there is work todo
		self.notify.notify_one();

		let message = format!("doc_save: {}", uri);
		self.client.log_message(MessageType::INFO, message).await;
	}

	async fn did_close(&self, _params: DidCloseTextDocumentParams) {
		// no-op for now
	}
}

#[tokio::main]
async fn main() {
	// list of docs and tasl notification
	let docs = Arc::new(Mutex::new(HashMap::new()));
	let notify = Arc::new(Notify::new());
	let generation = Arc::new(AtomicU64::new(0));

	// constructs our instance of the Backend
	let (service, socket) = LspService::new(move |client| Backend {
		client,
		docs: docs.clone(),
		notify: notify.clone(),
		generation: generation.clone(),
	});

	// starts reading JSON RPC messages from stdin, replies to stdout
	Server::new(stdin(), stdout(), socket).serve(service).await;
}

/// listens for work to do via notify, processing any
/// changes that require us to analyse again
async fn analyser_loop(
	client: Client,
	docs: Arc<Mutex<HashMap<Url, DocumentState>>>,
	notify: Arc<Notify>,
	generation: Arc<AtomicU64>,
) {
	const TIME_BUDGET: Duration = Duration::from_millis(50);
	const IDLE_DELAY: Duration = Duration::from_millis(200);

	loop {
		// Wait until something changes
		notify.notified().await;

		// Snapshot the generation at the moment we noticed work.
		let gen_at_start = generation.load(Ordering::Relaxed);

		// Debounce: we use a restartable idle wait – if a new event
		// arrives during this delay, we restart the loop and wait again.
		tokio::select! {
			_ = sleep(IDLE_DELAY) => {
				// idle window elapsed; we'll double-check generation below
			}
			_ = notify.notified() => {
				// another change arrived during debounce; restart the loop
				// so the idle window is effectively reset
				continue;
			}
		}

		// If generation changed during the idle window, a newer event
		// exists; restart and wait for the next idle gap.
		if generation.load(Ordering::Relaxed) != gen_at_start {
			continue;
		}

		let start = Instant::now();

		// Take a snapshot of dirty URIs so we hold the lock briefly
		let dirty_uris: Vec<Url> = {
			let docs_guard = docs.lock().unwrap();
			docs_guard
				.iter()
				.filter_map(
					|(uri, state)| {
						if state.dirty { Some(uri.clone()) } else { None }
					},
				)
				.collect()
		};

		for uri in dirty_uris {
			// cooperative cancellation: stop if we ran out of time,
			// or if a new user event happened since this pass started
			if start.elapsed() > TIME_BUDGET {
				// Out of time for this round; we'll pick up later
				break;
			}
			if generation.load(Ordering::Relaxed) != gen_at_start {
				// New event arrived; stop this pass early and let the
				// next idle cycle handle updated state.
				break;
			}

			// Snapshot the current text and mark clean
			let text = {
				let mut docs_guard = docs.lock().unwrap();
				if let Some(state) = docs_guard.get_mut(&uri) {
					state.dirty = false;
					state.text.clone()
				} else {
					continue;
				}
			};

			analyse_single_document(&client, uri.clone(), text).await;
		}
	}
}

/// requested to analyse a document in isolation
async fn analyse_single_document(client: &Client, uri: Url, _text: String) {
	let message = format!("analyse_document: {}", uri);
	client.log_message(MessageType::INFO, message).await;

	// For now: just publish a simple warning to prove the loop runs.
	let diag = Diagnostic {
		range: Range {
			start: Position {
				line: 0,
				character: 0,
			},
			end: Position {
				line: 0,
				character: 1,
			},
		},
		severity: Some(DiagnosticSeverity::WARNING),
		code: Some(NumberOrString::String("WA2_TEST".into())),
		source: Some("wa2-lsp".into()),
		message: "WA2 analysis stub ran (analyse warning)".into(),
		..Default::default()
	};

	client.publish_diagnostics(uri, vec![diag], None).await;
}
