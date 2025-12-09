use std::{collections::HashMap, sync::Arc};

use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url};

use crate::spec::{
	cfn_ir::CfnTemplate,
	spec_store::SpecStore,
};

/// per-document state held by the core engine
struct DocumentState {
	text: String,
}

/// core engine: owns all document state and analysis logic
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
	pub fn new() -> Self {
		Self {
			docs: HashMap::new(),
			spec: None,
		}
	}

	pub fn set_spec_store(&mut self, spec: Arc<SpecStore>) {
		self.spec = Some(spec);
	}

	pub fn spec_store(&self) -> Option<&SpecStore> {
		self.spec.as_deref()
	}

	pub fn on_open(&mut self, uri: Url, text: String) {
		self.docs.insert(uri, DocumentState { text });
	}

	pub fn on_change(&mut self, uri: Url, new_text: String) {
		let entry = self.docs.entry(uri).or_insert(DocumentState {
			text: String::new(),
		});

		entry.text = new_text;
	}

	pub fn on_save(&mut self, _uri: &Url) {}

	pub fn analyse_document_fast(&self, uri: &Url) -> Option<Vec<Diagnostic>> {
		let doc = self.docs.get(uri)?;
		let text = &doc.text;

		let path = uri.path();
		let diags = if path.ends_with(".json") {
			self.analyse_json(uri, text)
		// don't really need this as else covers it
		// } else if path.ends_with(".yml") || path.ends_with(".yaml") {
		// 	self.analyse_yaml(uri, text)
		} else {
			self.analyse_yaml(uri, text)
		};

		Some(diags)
	}

	/// Analyse a document as YAML using saphyr with IR conversion
	fn analyse_yaml(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
		// Parse to IR
		let template = match CfnTemplate::from_yaml(text, uri) {
			Ok(t) => t,
			Err(diags) => return diags,
		};

		let mut diags = Vec::new();

		// Check for missing Resources section (warning only)
		if template.resources.is_empty() {
			diags.push(Diagnostic {
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
				code: Some(NumberOrString::String("WA2_CFN_RESOURCES_MISSING".into())),
				source: Some("wa2-lsp".into()),
				message: format!(
					"YAML template {uri} has no top-level `Resources` section; \
                 most CloudFormation templates define at least one resource."
				),
				..Default::default()
			});
		}

		// Validate against spec if available
		if let Some(spec) = self.spec_store() {
			diags.extend(template.validate_against_spec(spec, uri));
		}

		diags
	}

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
mod spec_tests {
	use super::*;
	use crate::spec::spec_store::SpecStore;
	use std::sync::Arc;
	use tower_lsp::lsp_types::DiagnosticSeverity;

	fn uri(path: &str) -> Url {
		Url::parse(path).expect("valid URI")
	}

	/// Helper to create a minimal SpecStore for testing using JSON parsing
	fn create_test_spec() -> Arc<SpecStore> {
		let json = r#"{
			"ResourceTypes": {
				"AWS::S3::Bucket": {
					"Properties": {
						"BucketName": {
							"PrimitiveType": "String",
							"Required": false
						},
						"BucketEncryption": {
							"Type": "BucketEncryption",
							"Required": true
						},
						"Tags": {
							"Type": "List",
							"ItemType": "Tag",
							"Required": false
						}
					}
				},
				"AWS::Lambda::Function": {
					"Properties": {
						"FunctionName": {
							"PrimitiveType": "String",
							"Required": false
						},
						"Code": {
							"Type": "Code",
							"Required": true
						},
						"Runtime": {
							"PrimitiveType": "String",
							"Required": true
						}
					}
				}
			}
		}"#;

		Arc::new(SpecStore::from_json_bytes(json.as_bytes()).unwrap())
	}

	#[test]
	fn spec_unknown_resource_type() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyResource:
    Type: AWS::FakeService::FakeResource
    Properties:
      SomeProp: value
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 1);
		let d = &diags[0];
		assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
		assert_eq!(
			d.code,
			Some(NumberOrString::String(
				"WA2_CFN_UNKNOWN_RESOURCE_TYPE".into()
			))
		);
		assert!(d.message.contains("AWS::FakeService::FakeResource"));
		assert!(d.message.contains("MyResource"));
	}

	#[test]
	fn spec_unknown_property() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketEncryption: enabled
      InvalidProperty: value
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 1);
		let d = &diags[0];
		assert_eq!(d.severity, Some(DiagnosticSeverity::WARNING));
		assert_eq!(
			d.code,
			Some(NumberOrString::String("WA2_CFN_UNKNOWN_PROPERTY".into()))
		);
		assert!(d.message.contains("InvalidProperty"));
		assert!(d.message.contains("MyBucket"));
		assert!(d.message.contains("AWS::S3::Bucket"));
	}

	#[test]
	fn spec_missing_required_property() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: my-bucket
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 1);
		let d = &diags[0];
		assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
		assert_eq!(
			d.code,
			Some(NumberOrString::String(
				"WA2_CFN_REQUIRED_PROPERTY_MISSING".into()
			))
		);
		assert!(d.message.contains("BucketEncryption"));
		assert!(d.message.contains("MyBucket"));
	}

	#[test]
	fn spec_multiple_issues_one_resource() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      InvalidProperty: value
      AnotherBadProp: value2
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		// Should have: missing BucketEncryption + 2 unknown properties = 3 diagnostics
		assert_eq!(diags.len(), 3);

		let unknown_count = diags
			.iter()
			.filter(|d| d.code == Some(NumberOrString::String("WA2_CFN_UNKNOWN_PROPERTY".into())))
			.count();
		assert_eq!(unknown_count, 2);

		let missing_count = diags
			.iter()
			.filter(|d| {
				d.code
					== Some(NumberOrString::String(
						"WA2_CFN_REQUIRED_PROPERTY_MISSING".into(),
					))
			})
			.count();
		assert_eq!(missing_count, 1);
	}

	#[test]
	fn spec_multiple_resources_different_issues() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      InvalidProperty: value
  MyFunction:
    Type: AWS::Lambda::Function
    Properties:
      FunctionName: my-func
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		// MyBucket: missing BucketEncryption + unknown property = 2
		// MyFunction: missing Code + missing Runtime = 2
		// Total = 4
		assert_eq!(diags.len(), 4);

		let bucket_diags: Vec<_> = diags
			.iter()
			.filter(|d| d.message.contains("MyBucket"))
			.collect();
		assert_eq!(bucket_diags.len(), 2);

		let function_diags: Vec<_> = diags
			.iter()
			.filter(|d| d.message.contains("MyFunction"))
			.collect();
		assert_eq!(function_diags.len(), 2);
	}

	#[test]
	fn spec_valid_resource_all_required_properties() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketEncryption: enabled
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 0, "valid resource should have no diagnostics");
	}

	#[test]
	fn spec_valid_resource_with_optional_properties() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketEncryption: enabled
      BucketName: my-bucket
      Tags:
        - Key: Environment
          Value: Production
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(
			diags.len(),
			0,
			"valid resource with optional properties should have no diagnostics"
		);
	}

	#[test]
	fn spec_resource_key_not_string() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		// YAML allows non-string keys, but CFN doesn't
		let text = r#"
Resources:
  123:
    Type: AWS::S3::Bucket
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 1);
		let d = &diags[0];
		assert_eq!(d.severity, Some(DiagnosticSeverity::WARNING));
		assert_eq!(
			d.code,
			Some(NumberOrString::String(
				"WA2_CFN_RESOURCE_KEY_NOT_STRING".into()
			))
		);
	}

	#[test]
	fn spec_resource_not_mapping() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket: just-a-string
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 1);
		let d = &diags[0];
		assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
		assert_eq!(
			d.code,
			Some(NumberOrString::String(
				"WA2_CFN_RESOURCE_NOT_MAPPING".into()
			))
		);
		assert!(d.message.contains("MyBucket"));
	}

	#[test]
	fn spec_type_not_string() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: 123
    Properties:
      BucketName: test
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		// IR conversion skips non-string Type, so we see missing Type
		assert_eq!(diags.len(), 1);
		let d = &diags[0];
		assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
		assert_eq!(
			d.code,
			Some(NumberOrString::String(
				"WA2_CFN_RESOURCE_TYPE_MISSING".into()
			))
		);
	}

	#[test]
	fn spec_type_missing() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Properties:
      BucketName: test
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 1);
		let d = &diags[0];
		assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
		assert_eq!(
			d.code,
			Some(NumberOrString::String(
				"WA2_CFN_RESOURCE_TYPE_MISSING".into()
			))
		);
		assert!(d.message.contains("MyBucket"));
	}

	#[test]
	fn spec_properties_not_mapping() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties: just-a-string
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		// IR conversion skips non-mapping Properties, so we just see missing required property
		assert_eq!(diags.len(), 1);
		let d = &diags[0];
		assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
		assert_eq!(
			d.code,
			Some(NumberOrString::String(
				"WA2_CFN_REQUIRED_PROPERTY_MISSING".into()
			))
		);
	}

	#[test]
	fn spec_property_key_not_string() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		// YAML technically allows this but it's invalid for CFN
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketEncryption: enabled
      123: value
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		// IR conversion skips non-string keys, so no diagnostic
		// This is acceptable - the key is just ignored
		assert_eq!(diags.len(), 0);
	}

	#[test]
	fn spec_resource_with_no_properties_section() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		// Some resources might have all optional properties
		// but our S3::Bucket has required BucketEncryption
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		// Now correctly detects missing required property
		// (This is the correct behavior - the old code had a bug)
		assert_eq!(diags.len(), 1);
		assert!(diags[0].message.contains("missing required property"));
		assert!(diags[0].message.contains("BucketEncryption"));
	}

	#[test]
	fn spec_multiple_missing_required_properties() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyFunction:
    Type: AWS::Lambda::Function
    Properties:
      FunctionName: my-func
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		// Missing Code and Runtime
		assert_eq!(diags.len(), 2);
		assert!(
			diags
				.iter()
				.all(|d| d.severity == Some(DiagnosticSeverity::ERROR))
		);
		assert!(diags.iter().all(|d| d.code
			== Some(NumberOrString::String(
				"WA2_CFN_REQUIRED_PROPERTY_MISSING".into()
			))));

		let messages: Vec<_> = diags.iter().map(|d| d.message.as_str()).collect();
		assert!(messages.iter().any(|m| m.contains("Code")));
		assert!(messages.iter().any(|m| m.contains("Runtime")));
	}

	#[test]
	fn spec_all_valid_resources_no_errors() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketEncryption: enabled
      BucketName: test-bucket
  MyFunction:
    Type: AWS::Lambda::Function
    Properties:
      Code: 
        ZipFile: "code"
      Runtime: python3.9
      FunctionName: test-function
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(
			diags.len(),
			0,
			"all valid resources should produce no diagnostics"
		);
	}

	#[test]
	fn spec_mixed_valid_and_invalid_resources() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"
Resources:
  ValidBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketEncryption: enabled
  InvalidFunction:
    Type: AWS::Lambda::Function
    Properties:
      FunctionName: test
      InvalidProp: value
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		// ValidBucket should be fine
		// InvalidFunction: missing Code, missing Runtime, unknown InvalidProp = 3 diagnostics
		assert_eq!(diags.len(), 3);

		let valid_bucket_diags: Vec<_> = diags
			.iter()
			.filter(|d| d.message.contains("ValidBucket"))
			.collect();
		assert_eq!(valid_bucket_diags.len(), 0);

		let invalid_function_diags: Vec<_> = diags
			.iter()
			.filter(|d| d.message.contains("InvalidFunction"))
			.collect();
		assert_eq!(invalid_function_diags.len(), 3);
	}

	#[test]
	fn span_points_to_resource_logical_id() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"Resources:
  MyBucket:
    Type: AWS::FakeType
    Properties:
      BucketName: test
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 1);
		let d = &diags[0];

		// With saphyr markers, we now point to the Type VALUE, not the key
		// "AWS::FakeType" starts at column 10 on line 2
		assert_eq!(
			d.range.start.line, 2,
			"diagnostic points to Type value line"
		);
		assert_eq!(d.range.start.character, 10, "should start at Type value");
	}

	#[test]
	fn span_points_to_type_field() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		let text = r#"Resources:
  MyBucket:
    Type: AWS::FakeType
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 1);
		let d = &diags[0];

		// Points to the Type value on line 2
		assert_eq!(d.range.start.line, 2, "diagnostic should be on line 2");
		assert_eq!(d.range.start.character, 10, "points to Type value");
	}

	#[test]
	fn span_disambiguates_similar_resource_names() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		// Test that "Bucket" and "BucketPolicy" don't confuse the span resolver
		let text = r#"Resources:
  Bucket:
    Type: AWS::FakeType1
  BucketPolicy:
    Type: AWS::FakeType2
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 2);

		// Both diagnostics point to Type values on lines 2 and 4
		let diag_lines: Vec<u32> = diags.iter().map(|d| d.range.start.line).collect();
		assert!(diag_lines.contains(&2), "Should have diagnostic on line 2");
		assert!(diag_lines.contains(&4), "Should have diagnostic on line 4");
	}

	#[test]
	fn span_handles_indented_yaml() {
		let mut engine = CoreEngine::new();
		engine.set_spec_store(create_test_spec());
		let uri = uri("file:///tmp/test.yaml");

		// Varying indentation levels
		let text = r#"Resources:
    MyBucket:
        Type: AWS::FakeType
"#;

		engine.on_open(uri.clone(), text.to_string());
		let diags = engine.analyse_document_fast(&uri).unwrap();

		assert_eq!(diags.len(), 1);
		let d = &diags[0];

		// Points to Type value (line 2) with extra indentation
		// "AWS::FakeType" starts at column 14 (8 spaces + "Type: " = 14)
		assert_eq!(d.range.start.line, 2);
		assert_eq!(
			d.range.start.character, 14,
			"should point to Type value with extra indentation"
		);
	}
}
