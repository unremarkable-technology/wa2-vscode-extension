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
		let text = &doc.text;

		// Decide file format based on extension. We treat:
		//  - *.json as JSON
		//  - *.yml / *.yaml as YAML
		//  - everything else: default to YAML first (common for CFN), and
		//    if that ever changes we can revisit.
		let path = uri.path();
		let diags = if path.ends_with(".json") {
			self.analyse_json(uri, text)
		} else if path.ends_with(".yml") || path.ends_with(".yaml") {
			self.analyse_yaml(uri, text)
		} else {
			// default to YAML for now
			self.analyse_yaml(uri, text)
		};

		Some(diags)
	}

	/// Analyse a document as YAML using serde_yaml.
	/// For now:
	///   - on parse error: report a WA2_YAML_PARSE error
	///   - on success: warn if there is no top-level `Resources` key
	fn analyse_yaml(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
		match serde_yaml::from_str::<serde_yaml::Value>(text) {
			Ok(value) => {
				let mut diags = Vec::new();

				// CloudFormation templates are expected to be mappings with a
				// top-level `Resources` key in most real-world cases.
				let has_resources = match value {
					serde_yaml::Value::Mapping(ref map) => map
						.keys()
						.any(|k| matches!(k, serde_yaml::Value::String(s) if s == "Resources")),
					_ => false,
				};

				if !has_resources {
					// We don't know the exact location of "Resources" (it doesn't
					// exist), so we attach this to the very start of the file.
					let range = Range {
						start: Position {
							line: 0,
							character: 0,
						},
						end: Position {
							line: 0,
							character: 1,
						},
					};

					diags.push(Diagnostic {
						range,
						severity: Some(DiagnosticSeverity::WARNING),
						code: Some(NumberOrString::String("WA2_CFN_RESOURCES_MISSING".into())),
						source: Some("wa2-lsp".into()),
						message: format!(
							"YAML template {uri} has no top-level `Resources` section; \
                         most CloudFormation templates define at least one resource."
						),
						..Default::default()
					});
				}

				diags
			}
			Err(err) => {
				// serde_yaml::Error may have a location (1-based line/col).
				let (line, col) = match err.location() {
					Some(loc) => {
						// LSP positions are 0-based
						(loc.line().saturating_sub(1), loc.column().saturating_sub(1))
					}
					None => (0, 0),
				};

				let range = Range {
					start: Position {
						line: line as u32,
						character: col as u32,
					},
					end: Position {
						line: line as u32,
						character: (col + 1) as u32,
					},
				};

				vec![Diagnostic {
					range,
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String("WA2_YAML_PARSE".into())),
					source: Some("wa2-lsp".into()),
					message: format!("YAML parse error in {uri}: {err}"),
					..Default::default()
				}]
			}
		}
	}

	/// Analyse a document as JSON using serde_json.
	/// For now:
	///   - on parse error: report a WA2_JSON_PARSE error
	///   - on success: warn if there is no top-level `Resources` key
	fn analyse_json(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
		match serde_json::from_str::<serde_json::Value>(text) {
			Ok(value) => {
				let mut diags = Vec::new();

				let has_resources = match value {
					serde_json::Value::Object(ref map) => map.contains_key("Resources"),
					_ => false,
				};

				if !has_resources {
					let range = Range {
						start: Position {
							line: 0,
							character: 0,
						},
						end: Position {
							line: 0,
							character: 1,
						},
					};

					diags.push(Diagnostic {
						range,
						severity: Some(DiagnosticSeverity::WARNING),
						code: Some(NumberOrString::String("WA2_CFN_RESOURCES_MISSING".into())),
						source: Some("wa2-lsp".into()),
						message: format!(
							"JSON template {uri} has no top-level `Resources` section; \
                         most CloudFormation templates define at least one resource."
						),
						..Default::default()
					});
				}

				diags
			}
			Err(err) => {
				// serde_json::Error exposes line/column (1-based).
				let line = err.line().saturating_sub(1);
				let col = err.column().saturating_sub(1);

				let range = Range {
					start: Position {
						line: line as u32,
						character: col as u32,
					},
					end: Position {
						line: line as u32,
						character: (col + 1) as u32,
					},
				};

				vec![Diagnostic {
					range,
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String("WA2_JSON_PARSE".into())),
					source: Some("wa2-lsp".into()),
					message: format!("JSON parse error in {uri}: {err}"),
					..Default::default()
				}]
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use tower_lsp::lsp_types::DiagnosticSeverity;

	fn uri(path: &str) -> Url {
		Url::parse(path).expect("valid URI")
	}

	#[test]
	fn yaml_valid_produces_no_diagnostics() {
		let mut engine = CoreEngine::new();
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
"#;

		engine.on_open(uri.clone(), text.to_string());

		let diags = engine
			.analyse_document_fast(&uri)
			.expect("document should exist");

		assert!(
			diags.is_empty(),
			"expected no diagnostics for valid YAML, got: {diags:?}"
		);
	}

	#[test]
	fn yaml_invalid_produces_parse_error() {
		let mut engine = CoreEngine::new();
		let uri = uri("file:///tmp/test.yaml");

		// missing closing quote → invalid YAML
		let text = r#"Name: "abc"#;

		engine.on_open(uri.clone(), text.to_string());

		let diags = engine
			.analyse_document_fast(&uri)
			.expect("document should exist");

		assert_eq!(diags.len(), 1, "expected a single YAML parse error");
		let d = &diags[0];

		assert_eq!(
			d.severity,
			Some(DiagnosticSeverity::ERROR),
			"expected ERROR severity"
		);
		assert_eq!(
			d.code,
			Some(NumberOrString::String("WA2_YAML_PARSE".into())),
			"expected YAML parse error code"
		);
		assert!(
			d.message.contains("YAML parse error"),
			"unexpected message: {}",
			d.message
		);
	}

	#[test]
	fn json_valid_produces_no_diagnostics() {
		let mut engine = CoreEngine::new();
		let uri = uri("file:///tmp/test.json");

		let text = r#"
{
  "Resources": {
    "MyBucket": {
      "Type": "AWS::S3::Bucket"
    }
  }
}
"#;

		engine.on_open(uri.clone(), text.to_string());

		let diags = engine
			.analyse_document_fast(&uri)
			.expect("document should exist");

		assert!(
			diags.is_empty(),
			"expected no diagnostics for valid JSON, got: {diags:?}"
		);
	}

	#[test]
	fn json_invalid_produces_parse_error() {
		let mut engine = CoreEngine::new();
		let uri = uri("file:///tmp/test.json");

		// trailing comma → invalid JSON
		let text = r#"
{
  "Name": "abc",
}
"#;

		engine.on_open(uri.clone(), text.to_string());

		let diags = engine
			.analyse_document_fast(&uri)
			.expect("document should exist");

		assert_eq!(diags.len(), 1, "expected a single JSON parse error");
		let d = &diags[0];

		assert_eq!(
			d.severity,
			Some(DiagnosticSeverity::ERROR),
			"expected ERROR severity"
		);
		assert_eq!(
			d.code,
			Some(NumberOrString::String("WA2_JSON_PARSE".into())),
			"expected JSON parse error code"
		);
		assert!(
			d.message.contains("JSON parse error"),
			"unexpected message: {}",
			d.message
		);
	}

	#[test]
	fn yaml_missing_resources_produces_warning() {
		let mut engine = CoreEngine::new();
		let uri = uri("file:///tmp/no-resources.yaml");

		let text = r#"
Parameters:
  Env:
    Type: String
"#;

		engine.on_open(uri.clone(), text.to_string());

		let diags = engine
			.analyse_document_fast(&uri)
			.expect("document should exist");

		assert_eq!(
			diags.len(),
			1,
			"expected a single warning for missing Resources, got: {diags:?}"
		);
		let d = &diags[0];

		assert_eq!(
			d.severity,
			Some(DiagnosticSeverity::WARNING),
			"expected WARNING severity"
		);
		assert_eq!(
			d.code,
			Some(NumberOrString::String("WA2_CFN_RESOURCES_MISSING".into())),
			"expected WA2_CFN_RESOURCES_MISSING code"
		);
	}

	#[test]
	fn json_missing_resources_produces_warning() {
		let mut engine = CoreEngine::new();
		let uri = uri("file:///tmp/no-resources.json");

		let text = r#"
{
  "Parameters": {
    "Env": { "Type": "String" }
  }
}
"#;

		engine.on_open(uri.clone(), text.to_string());

		let diags = engine
			.analyse_document_fast(&uri)
			.expect("document should exist");

		assert_eq!(
			diags.len(),
			1,
			"expected a single warning for missing Resources, got: {diags:?}"
		);
		let d = &diags[0];

		assert_eq!(
			d.severity,
			Some(DiagnosticSeverity::WARNING),
			"expected WARNING severity"
		);
		assert_eq!(
			d.code,
			Some(NumberOrString::String("WA2_CFN_RESOURCES_MISSING".into())),
			"expected WA2_CFN_RESOURCES_MISSING code"
		);
	}
}
