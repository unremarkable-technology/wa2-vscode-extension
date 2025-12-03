use std::collections::HashMap;

use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url};

/// per-document state held by the core engine
struct DocumentState {
	text: String,
}

/// core engine: owns all document state and analysis logic
/// this is kept synchronous and independent of tower-lsp so it can be
/// unit-tested without async or JSON-RPC
pub struct CoreEngine {
	docs: HashMap<Url, DocumentState>,
}

impl CoreEngine {
	/// construct a new, empty engine
	pub fn new() -> Self {
		Self {
			docs: HashMap::new(),
		}
	}

	/// event: new document opened with full text
	pub fn on_open(&mut self, uri: Url, text: String) {
		self.docs.insert(uri, DocumentState { text });
	}

	/// event: document text changed (we assume full-text sync for now)
	pub fn on_change(&mut self, uri: Url, new_text: String) {
		let entry = self.docs.entry(uri).or_insert(DocumentState {
			text: String::new(),
		});

		entry.text = new_text;
	}

	/// event: document saved
	pub fn on_save(&mut self, _uri: &Url) {}

	/// requested to analyse a document in isolation (fast path)
	///
	/// for now this is a stub that always returns a single warning,
	/// just to prove that the loop and engine wiring work.
	/// later this becomes the real per-file analysis (parse, CFN checks, etc).
	pub fn analyse_document_fast(&self, uri: &Url) -> Option<Vec<Diagnostic>> {
		let doc = self.docs.get(uri)?;
		let _text = &doc.text;

		let message = format!("WA2 analyse_document_fast: {uri}");

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
			message: format!("{message} (analysis stub)"),
			..Default::default()
		};

		Some(vec![diag])
	}
}
