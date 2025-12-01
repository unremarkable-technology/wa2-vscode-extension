use tokio::io::{stdin, stdout};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
	#[allow(dead_code)]
	client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
	async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
		Ok(InitializeResult {
			capabilities: ServerCapabilities {
				text_document_sync: Some(TextDocumentSyncCapability::Kind(
					TextDocumentSyncKind::INCREMENTAL,
				)),
				..ServerCapabilities::default()
			},
			server_info: Some(ServerInfo {
				name: "stackgraph-lsp".to_string(),
				version: None,
			}),
		})
	}

	async fn initialized(&self, _params: InitializedParams) {
		self.client
			.log_message(MessageType::INFO, "WA2 LSP initialized")
			.await;
	}

	async fn shutdown(&self) -> Result<()> {
		Ok(())
	}

	async fn did_open(&self, params: DidOpenTextDocumentParams) {
		// Simple test diagnostic: warn on the first character of the file
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
			code: Some(NumberOrString::String("SG000".into())),
			source: Some("WA2".into()),
			message: "WA2 LSP is wired up (test warning)".into(),
			..Default::default()
		};

		self.client
			.publish_diagnostics(params.text_document.uri, vec![diag], None)
			.await;
	}

	async fn did_change(&self, _params: DidChangeTextDocumentParams) {
		// no-op for now
	}

	async fn did_save(&self, _params: DidSaveTextDocumentParams) {
		// no-op for now
	}

	async fn did_close(&self, _params: DidCloseTextDocumentParams) {
		// no-op for now
	}
}

#[tokio::main]
async fn main() {
	let (service, socket) = LspService::new(|client| Backend { client });
	Server::new(stdin(), stdout(), socket).serve(service).await;
}
