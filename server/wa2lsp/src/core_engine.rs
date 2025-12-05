use std::{collections::HashMap, sync::Arc};

use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url};

use crate::spec::spec_store::{PropertyName, ResourceTypeId, SpecStore};

/// per-document state held by the core engine
struct DocumentState {
	text: String,
}

/// core engine: owns all document state and analysis logic
/// this is kept synchronous and independent of tower-lsp so it can be
/// unit-tested without async or JSON-RPC
pub struct CoreEngine {
	docs: HashMap<Url, DocumentState>,
	spec: Option<Arc<SpecStore>>,
}

impl Default for CoreEngine {
	fn default() -> Self {
		Self::new()
	}
}

impl CoreEngine {
	/// construct a new, empty engine
	pub fn new() -> Self {
		Self {
			docs: HashMap::new(),
			spec: None,
		}
	}

	/// Inject the global SpecStore once it is loaded.
	pub fn set_spec_store(&mut self, spec: Arc<SpecStore>) {
		self.spec = Some(spec);
	}

	/// Accessor for analysis helpers.
	pub fn spec_store(&self) -> Option<&SpecStore> {
		self.spec.as_deref()
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

				// if we have a loaded SpecStore, do spec-based validation.
				if let Some(spec) = self.spec_store() {
					self.check_cfn_yaml_with_spec(uri, &value, spec, &mut diags);
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

	/// Perform simple CloudFormation-aware checks using the loaded SpecStore:
	///  - unknown resource types
	///  - unknown properties
	///  - missing required properties
	///
	/// For now we don't have per-node spans from serde_yaml, so we attach
	/// diagnostics to the start of the file and include IDs in the message.
	fn check_cfn_yaml_with_spec(
		&self,
		uri: &Url,
		root: &serde_yaml::Value,
		spec: &SpecStore,
		diags: &mut Vec<Diagnostic>,
	) {
		let root_map = match root {
			serde_yaml::Value::Mapping(m) => m,
			_ => return,
		};

		let resources_val = match root_map.get(serde_yaml::Value::String("Resources".into())) {
			Some(v) => v,
			None => return,
		};

		let resources_map = match resources_val {
			serde_yaml::Value::Mapping(m) => m,
			_ => return,
		};

		// Reusable "file start" range for now.
		let file_start = Range {
			start: Position {
				line: 0,
				character: 0,
			},
			end: Position {
				line: 0,
				character: 1,
			},
		};

		for (logical_key, resource_val) in resources_map {
			let logical_id = match logical_key {
				serde_yaml::Value::String(s) => s.as_str(),
				_ => {
					diags.push(Diagnostic {
						range: file_start,
						severity: Some(DiagnosticSeverity::WARNING),
						code: Some(NumberOrString::String(
							"WA2_CFN_RESOURCE_KEY_NOT_STRING".into(),
						)),
						source: Some("wa2-lsp".into()),
						message: format!(
							"YAML template {uri}: resource key is not a string; \
                             CloudFormation logical IDs must be strings."
						),
						..Default::default()
					});
					continue;
				}
			};

			let resource_map = match resource_val {
				serde_yaml::Value::Mapping(m) => m,
				_ => {
					diags.push(Diagnostic {
                        range: file_start,
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(NumberOrString::String(
                            "WA2_CFN_RESOURCE_NOT_MAPPING".into(),
                        )),
                        source: Some("wa2-lsp".into()),
                        message: format!(
                            "YAML template {uri}: resource `{logical_id}` is not a mapping; \
                             CloudFormation resources must be mappings with `Type` and `Properties`."
                        ),
                        ..Default::default()
                    });
					continue;
				}
			};

			// Extract Type
			let type_str = match resource_map.get(serde_yaml::Value::String("Type".into())) {
				Some(serde_yaml::Value::String(s)) => s.as_str(),
				Some(_) => {
					diags.push(Diagnostic {
						range: file_start,
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String(
							"WA2_CFN_RESOURCE_TYPE_NOT_STRING".into(),
						)),
						source: Some("wa2-lsp".into()),
						message: format!(
							"YAML template {uri}: resource `{logical_id}` has a non-string `Type` value."
						),
						..Default::default()
					});
					continue;
				}
				None => {
					diags.push(Diagnostic {
						range: file_start,
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String(
							"WA2_CFN_RESOURCE_TYPE_MISSING".into(),
						)),
						source: Some("wa2-lsp".into()),
						message: format!(
							"YAML template {uri}: resource `{logical_id}` is missing required `Type`."
						),
						..Default::default()
					});
					continue;
				}
			};

			let type_id = ResourceTypeId(type_str.to_string());

			let rt = match spec.resource_types.get(&type_id) {
				Some(rt) => rt,
				None => {
					diags.push(Diagnostic {
						range: file_start,
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String(
							"WA2_CFN_UNKNOWN_RESOURCE_TYPE".into(),
						)),
						source: Some("wa2-lsp".into()),
						message: format!(
							"YAML template {uri}: resource `{logical_id}` uses unknown resource type `{type_str}`."
						),
						..Default::default()
					});
					continue;
				}
			};

			// Extract Properties mapping, if present.
			let props_val = resource_map.get(serde_yaml::Value::String("Properties".into()));

			let props_map = match props_val {
				Some(serde_yaml::Value::Mapping(m)) => Some(m),
				Some(_) => {
					diags.push(Diagnostic {
						range: file_start,
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String(
							"WA2_CFN_PROPERTIES_NOT_MAPPING".into(),
						)),
						source: Some("wa2-lsp".into()),
						message: format!(
							"YAML template {uri}: resource `{logical_id}` has a non-mapping `Properties` value."
						),
						..Default::default()
					});
					None
				}
				None => None,
			};

			// 1) Unknown properties
			if let Some(props) = props_map {
				for (prop_key, _prop_val) in props {
					let prop_name_str = match prop_key {
						serde_yaml::Value::String(s) => s.as_str(),
						_ => {
							diags.push(Diagnostic {
								range: file_start,
								severity: Some(DiagnosticSeverity::WARNING),
								code: Some(NumberOrString::String(
									"WA2_CFN_PROPERTY_KEY_NOT_STRING".into(),
								)),
								source: Some("wa2-lsp".into()),
								message: format!(
									"YAML template {uri}: resource `{logical_id}` has a property key \
                                     that is not a string; property names must be strings."
								),
								..Default::default()
							});
							continue;
						}
					};

					let prop_id = PropertyName(prop_name_str.to_string());

					if !rt.properties.contains_key(&prop_id) {
						diags.push(Diagnostic {
							range: file_start,
							severity: Some(DiagnosticSeverity::WARNING),
							code: Some(NumberOrString::String("WA2_CFN_UNKNOWN_PROPERTY".into())),
							source: Some("wa2-lsp".into()),
							message: format!(
								"YAML template {uri}: resource `{logical_id}` of type `{type_str}` \
                                 has unknown property `{prop_name_str}`."
							),
							..Default::default()
						});
					}
				}
			}

			// 2) Missing required properties
			if let Some(props) = props_map {
				for (pname, _pshape) in rt.properties.iter().filter(|(_, s)| s.required) {
					let required_key = serde_yaml::Value::String(pname.0.clone());
					if !props.contains_key(&required_key) {
						diags.push(Diagnostic {
							range: file_start,
							severity: Some(DiagnosticSeverity::ERROR),
							code: Some(NumberOrString::String(
								"WA2_CFN_REQUIRED_PROPERTY_MISSING".into(),
							)),
							source: Some("wa2-lsp".into()),
							message: format!(
								"YAML template {uri}: resource `{logical_id}` of type `{type_str}` \
                                 is missing required property `{}`.",
								pname.0
							),
							..Default::default()
						});
					}
				}
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
