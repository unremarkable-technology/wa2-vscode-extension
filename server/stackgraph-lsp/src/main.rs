use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use tokio::io::{stdin, stdout};
use tokio::sync::Notify;
use tokio::time::sleep;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod core_engine;
use core_engine::CoreEngine;

/// backend: LSP-facing adapter that holds the engine and schedules work
struct Backend {
	client: Client,
	engine: Arc<Mutex<CoreEngine>>,
	notify: Arc<Notify>,
	// queue of events (generation, uri); multiple entries for the same uri
	// are coalesced by the analyser loop.
	event_queue: Arc<Mutex<VecDeque<Url>>>,
}

impl Backend {
	/// bump generation, enqueue an event for this uri, and notify the analyser
	fn enqueue_event(&self, uri: &Url) {
		// enqueue this event for the analyser
		{
			let mut queue = self.event_queue.lock().unwrap();
			queue.push_back(uri.clone());
		}

		// tell background analyser there is work todo
		self.notify.notify_one();
	}
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
		let engine = self.engine.clone();
		let notify = self.notify.clone();
		let event_queue = self.event_queue.clone();

		// listen for work via notify
		tokio::spawn(async move {
			analyser_loop(client, engine, notify, event_queue).await;
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

		// forward to core engine (under lock)
		{
			let mut engine = self.engine.lock().unwrap();
			engine.on_open(uri.clone(), text);
		}

		// enqueue analysis event
		self.enqueue_event(&uri);

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

		// forward to core engine (under lock)
		{
			let mut engine = self.engine.lock().unwrap();
			engine.on_change(uri.clone(), new_text);
		}

		// enqueue analysis event
		self.enqueue_event(&uri);

		let message = format!("doc_change: {}", uri);
		self.client.log_message(MessageType::INFO, message).await;
	}

	/// document saved, re-run analysis on save
	/// capture the event, but do no work on main thread
	async fn did_save(&self, params: DidSaveTextDocumentParams) {
		let uri = params.text_document.uri;

		// forward to core engine (under lock)
		{
			let mut engine = self.engine.lock().unwrap();
			engine.on_save(&uri);
		}

		// enqueue analysis event
		self.enqueue_event(&uri);

		let message = format!("doc_save: {}", uri);
		self.client.log_message(MessageType::INFO, message).await;
	}

	async fn did_close(&self, _params: DidCloseTextDocumentParams) {
		// no-op for now
	}
}

#[tokio::main]
async fn main() {
	// core engine, task notification and atomic generation counter
	let engine = Arc::new(Mutex::new(CoreEngine::new()));
	let notify = Arc::new(Notify::new());
	let event_queue: Arc<Mutex<VecDeque<Url>>> = Arc::new(Mutex::new(VecDeque::new()));

	// constructs our instance of the Backend
	let (service, socket) = LspService::new(move |client| Backend {
		client,
		engine: engine.clone(),
		notify: notify.clone(),
		event_queue: event_queue.clone(),
	});

	// starts reading JSON RPC messages from stdin, replies to stdout
	Server::new(stdin(), stdout(), socket).serve(service).await;
}

/// listens for work to do via notify, processing any
/// changes that require us to analyse again
async fn analyser_loop(
	client: Client,
	engine: Arc<Mutex<CoreEngine>>,
	notify: Arc<Notify>,
	event_queue: Arc<Mutex<VecDeque<Url>>>,
) {
	const TIME_BUDGET: Duration = Duration::from_millis(50);
	const IDLE_DELAY: Duration = Duration::from_millis(200);

	loop {
		// Wait until something changes
		notify.notified().await;

		// Debounce: wait for an idle gap so that multiple quick edits
		// can be coalesced into a single analysis pass.
		sleep(IDLE_DELAY).await;

		let start = Instant::now();

		loop {
			if start.elapsed() > TIME_BUDGET {
				let message = "analyser_loop: TIME_BUDGET exhausted".to_string();
				client.log_message(MessageType::INFO, message).await;
				break;
			}

			// Pop one event from the front of the queue. If the queue is
			// empty, we're done for this pass.
			let maybe_event = {
				let mut queue = event_queue.lock().unwrap();
				queue.pop_front()
			};

			let uri = match maybe_event {
				Some(ev) => ev,
				None => break, // no more work this pass
			};

			// Coalesce: remove any remaining events for this uri from the
			// queue. We consider them "covered" by this analysis run.
			{
				let mut queue = event_queue.lock().unwrap();
				queue.retain(|u| u != &uri);
			}

			// Analyse this uri using the latest text snapshot. We treat
			// "no text" as a processed event (doc was closed / removed).
			let diagnostics = {
				let engine_guard = engine.lock().unwrap();
				engine_guard.analyse_document_fast(&uri)
			};

			match diagnostics {
				Some(diagnostics) => {
					let message = format!("doc_analyse: {}", uri);
					client.log_message(MessageType::INFO, message).await;

					client
						.publish_diagnostics(uri.clone(), diagnostics, None)
						.await;
				}
				None => {
					let message = format!("doc_analyse: {} (skipped, no text in engine)", uri);
					client.log_message(MessageType::INFO, message).await;
					continue;
				}
			}
		}
	}
}
