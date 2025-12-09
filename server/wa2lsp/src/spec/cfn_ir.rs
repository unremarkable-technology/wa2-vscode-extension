use std::collections::HashMap;
use tower_lsp::lsp_types::Range;

/// CloudFormation template intermediate representation
#[derive(Debug, Clone)]
pub struct CfnTemplate {
	pub resources: HashMap<String, CfnResource>,
	// We'll add Parameters, Outputs, etc. later
}

/// A CloudFormation resource with position tracking
#[derive(Debug, Clone)]
pub struct CfnResource {
	pub logical_id: String,
	pub resource_type: String,
	pub properties: HashMap<String, CfnValue>,

	// Position tracking for diagnostics
	pub logical_id_range: Range,
	pub type_range: Range,
}

/// A value in a CloudFormation template with position tracking
#[derive(Debug, Clone)]
pub enum CfnValue {
	String(String, Range),
	Number(f64, Range),
	Bool(bool, Range),
	Null(Range),
	Array(Vec<CfnValue>, Range),
	Object(HashMap<String, CfnValue>, Range),
	// We'll add Ref, GetAtt etc. later
}

impl CfnValue {
	/// Get the position range of this value
	pub fn range(&self) -> Range {
		match self {
			CfnValue::String(_, r) => *r,
			CfnValue::Number(_, r) => *r,
			CfnValue::Bool(_, r) => *r,
			CfnValue::Null(r) => *r,
			CfnValue::Array(_, r) => *r,
			CfnValue::Object(_, r) => *r,
		}
	}

	/// Try to get this value as a string
	pub fn as_str(&self) -> Option<&str> {
		match self {
			CfnValue::String(s, _) => Some(s.as_str()),
			_ => None,
		}
	}

	/// Try to get this value as an object/mapping
	pub fn as_object(&self) -> Option<&HashMap<String, CfnValue>> {
		match self {
			CfnValue::Object(map, _) => Some(map),
			_ => None,
		}
	}

	/// Try to get this value as an array
	pub fn as_array(&self) -> Option<&[CfnValue]> {
		match self {
			CfnValue::Array(items, _) => Some(items),
			_ => None,
		}
	}
}

use saphyr::{LoadableYamlNode, MarkedYaml, ScanError};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Url};

impl CfnTemplate {
	/// Parse a CloudFormation template from YAML text
	pub fn from_yaml(text: &str, uri: &Url) -> Result<Self, Vec<Diagnostic>> {
		let docs = MarkedYaml::load_from_str(text)
			.map_err(|err| vec![yaml_error_to_diagnostic(err, uri)])?;

		if docs.is_empty() {
			return Ok(CfnTemplate {
				resources: HashMap::new(),
			});
		}

		Self::from_marked_yaml(&docs[0], uri)
	}

	fn from_marked_yaml(root: &MarkedYaml, uri: &Url) -> Result<Self, Vec<Diagnostic>> {
		let mut resources = HashMap::new();

		let root_map = match root.data.as_mapping() {
			Some(m) => m,
			None => {
				return Err(vec![Diagnostic {
					range: marked_yaml_to_range(root),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String("WA2_CFN_INVALID_ROOT".into())),
					source: Some("wa2-lsp".into()),
					message: format!("Template {uri} root must be a mapping"),
					..Default::default()
				}]);
			}
		};

		// Find Resources section
		let resources_node = root_map
			.iter()
			.find(|(k, _)| k.data.as_str() == Some("Resources"))
			.map(|(_, v)| v);

		let resources_node = match resources_node {
			Some(v) => v,
			None => {
				// No Resources section - return empty template (warning handled elsewhere)
				return Ok(CfnTemplate {
					resources: HashMap::new(),
				});
			}
		};

		let resources_map = match resources_node.data.as_mapping() {
			Some(m) => m,
			None => {
				return Err(vec![Diagnostic {
					range: marked_yaml_to_range(resources_node),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String("WA2_CFN_INVALID_RESOURCES".into())),
					source: Some("wa2-lsp".into()),
					message: format!("Template {uri} Resources section must be a mapping"),
					..Default::default()
				}]);
			}
		};

		// Convert each resource
		let mut errors = Vec::new();
		for (logical_key, resource_node) in resources_map {
			let logical_id = match logical_key.data.as_str() {
				Some(s) => s.to_string(),
				None => {
					errors.push(Diagnostic {
						range: marked_yaml_to_range(logical_key),
						severity: Some(DiagnosticSeverity::WARNING),
						code: Some(NumberOrString::String(
							"WA2_CFN_RESOURCE_KEY_NOT_STRING".into(),
						)),
						source: Some("wa2-lsp".into()),
						message: format!(
							"Template {uri}: resource key is not a string; \
                             CloudFormation logical IDs must be strings."
						),
						..Default::default()
					});
					continue;
				}
			};

			match CfnResource::from_marked_yaml(
				logical_id.clone(),
				resource_node,
				marked_yaml_to_range(logical_key),
				uri,
			) {
				Ok(resource) => {
					resources.insert(logical_id, resource);
				}
				Err(mut diags) => {
					errors.append(&mut diags);
				}
			}
		}

		if !errors.is_empty() {
			return Err(errors);
		}

		Ok(CfnTemplate { resources })
	}
}

impl CfnResource {
	fn from_marked_yaml(
		logical_id: String,
		node: &MarkedYaml,
		logical_id_range: Range,
		uri: &Url,
	) -> Result<Self, Vec<Diagnostic>> {
		let resource_map = match node.data.as_mapping() {
			Some(m) => m,
			None => {
				return Err(vec![Diagnostic {
					range: marked_yaml_to_range(node),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_RESOURCE_NOT_MAPPING".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!(
						"Template {uri}: resource `{logical_id}` is not a mapping; \
                         CloudFormation resources must be mappings with `Type` and `Properties`."
					),
					..Default::default()
				}]);
			}
		};

		// Extract Type
		let (type_str, type_range) = resource_map
			.iter()
			.find(|(k, _)| k.data.as_str() == Some("Type"))
			.and_then(|(_, v)| {
				v.data
					.as_str()
					.map(|s| (s.to_string(), marked_yaml_to_range(v)))
			})
			.ok_or_else(|| {
				vec![Diagnostic {
					range: logical_id_range,
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_RESOURCE_TYPE_MISSING".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!(
						"Template {uri}: resource `{logical_id}` is missing required `Type`."
					),
					..Default::default()
				}]
			})?;

		// Extract Properties (optional)
		let properties = resource_map
			.iter()
			.find(|(k, _)| k.data.as_str() == Some("Properties"))
			.map(|(_, v)| CfnValue::from_marked_yaml(v))
			.transpose()?
			.and_then(|v| match v {
				CfnValue::Object(map, _) => Some(map),
				_ => None,
			})
			.unwrap_or_default();

		Ok(CfnResource {
			logical_id,
			resource_type: type_str,
			properties,
			logical_id_range,
			type_range,
		})
	}
}

use saphyr::{Scalar, YamlData};
impl CfnValue {
	fn from_marked_yaml(node: &MarkedYaml) -> Result<Self, Vec<Diagnostic>> {
		let range = marked_yaml_to_range(node);

		Ok(match &node.data {
			// ----- Scalars -----
			YamlData::Value(scalar) => match scalar {
				Scalar::String(s) => CfnValue::String(s.to_string(), range),
				Scalar::Integer(i) => CfnValue::Number(*i as f64, range),
				Scalar::FloatingPoint(f) => {
					// OrderedFloat<f64> - use into_inner() to get f64
					CfnValue::Number(f.into_inner(), range)
				}
				Scalar::Boolean(b) => CfnValue::Bool(*b, range),
				Scalar::Null => CfnValue::Null(range),
			},

			// Handle tagged nodes by recursing on the inner value
			YamlData::Tagged(_tag, inner) => {
				return CfnValue::from_marked_yaml(inner);
			}

			// ----- Collections -----
			YamlData::Sequence(seq) => {
				let items: Result<Vec<_>, _> = seq.iter().map(CfnValue::from_marked_yaml).collect();
				CfnValue::Array(items?, range)
			}

			YamlData::Mapping(map) => {
				let mut obj = HashMap::new();
				for (k, v) in map {
					// Match on key to get string
					let key = match &k.data {
						YamlData::Value(Scalar::String(s)) => s.to_string(),
						_ => continue, // skip non-string keys
					};
					let value = CfnValue::from_marked_yaml(v)?;
					obj.insert(key, value);
				}
				CfnValue::Object(obj, range)
			}

			// Handle less common cases
			YamlData::Representation(_, _, _) => {
				// Shouldn't happen with default parsing, treat as null
				CfnValue::Null(range)
			}

			YamlData::Alias(_) => {
				// YAML aliases - for now treat as null
				// Could resolve these later if needed
				CfnValue::Null(range)
			}

			YamlData::BadValue => {
				// Invalid scalar value - treat as null
				CfnValue::Null(range)
			}
		})
	}
}

/// Convert a MarkedYaml node to an LSP Range
fn marked_yaml_to_range(node: &MarkedYaml) -> Range {
	let start_marker = node.span.start;
	let end_marker = node.span.end;

	let start_line = start_marker.line().saturating_sub(1);
	let start_col = start_marker.col();

	let end_line = end_marker.line().saturating_sub(1);
	let end_col = end_marker.col();

	Range {
		start: Position {
			line: start_line as u32,
			character: start_col as u32,
		},
		end: Position {
			line: end_line as u32,
			character: end_col as u32,
		},
	}
}

/// Convert a saphyr scan error to an LSP diagnostic
fn yaml_error_to_diagnostic(err: ScanError, uri: &Url) -> Diagnostic {
	let marker = err.marker();
	let line = marker.line().saturating_sub(1);
	let col = marker.col();

	Diagnostic {
		range: Range {
			start: Position {
				line: line as u32,
				character: col as u32,
			},
			end: Position {
				line: line as u32,
				character: (col + 1) as u32,
			},
		},
		severity: Some(DiagnosticSeverity::ERROR),
		code: Some(NumberOrString::String("WA2_YAML_PARSE".into())),
		source: Some("wa2-lsp".into()),
		message: format!("YAML parse error in {uri}: {err}"),
		..Default::default()
	}
}

use crate::spec::spec_store::{PropertyName, ResourceTypeId, SpecStore};

impl CfnTemplate {
	/// Validate this template against the CloudFormation spec
	pub fn validate_against_spec(&self, spec: &SpecStore, uri: &Url) -> Vec<Diagnostic> {
		let mut diags = Vec::new();

		for (logical_id, resource) in &self.resources {
			diags.extend(resource.validate_against_spec(logical_id, spec, uri));
		}

		diags
	}
}

impl CfnResource {
	/// Validate this resource against the CloudFormation spec
	fn validate_against_spec(
		&self,
		logical_id: &str,
		spec: &SpecStore,
		uri: &Url,
	) -> Vec<Diagnostic> {
		let mut diags = Vec::new();

		// Check if resource type exists in spec
		let type_id = ResourceTypeId(self.resource_type.clone());
		let rt = match spec.resource_types.get(&type_id) {
			Some(rt) => rt,
			None => {
				diags.push(Diagnostic {
					range: self.type_range,
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_UNKNOWN_RESOURCE_TYPE".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!(
						"YAML template {uri}: resource `{logical_id}` uses unknown resource type `{}`.",
						self.resource_type
					),
					..Default::default()
				});
				return diags; // Can't validate properties without type info
			}
		};

		// Check for unknown properties
		for (prop_name, prop_value) in &self.properties {
			let prop_id = PropertyName(prop_name.clone());
			if !rt.properties.contains_key(&prop_id) {
				diags.push(Diagnostic {
					range: prop_value.range(),
					severity: Some(DiagnosticSeverity::WARNING),
					code: Some(NumberOrString::String("WA2_CFN_UNKNOWN_PROPERTY".into())),
					source: Some("wa2-lsp".into()),
					message: format!(
						"YAML template {uri}: resource `{logical_id}` of type `{}` \
                         has unknown property `{prop_name}`.",
						self.resource_type
					),
					..Default::default()
				});
			}
		}

		// Check for missing required properties
		for (pname, _pshape) in rt.properties.iter().filter(|(_, s)| s.required) {
			if !self.properties.contains_key(&pname.0) {
				diags.push(Diagnostic {
					range: self.logical_id_range,
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_REQUIRED_PROPERTY_MISSING".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!(
						"YAML template {uri}: resource `{logical_id}` of type `{}` \
                         is missing required property `{}`.",
						self.resource_type, pname.0
					),
					..Default::default()
				});
			}
		}

		diags
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn test_uri() -> Url {
		Url::parse("file:///tmp/test.yaml").unwrap()
	}

	#[test]
	fn yaml_converts_simple_resource() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: test-bucket
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		assert_eq!(template.resources.len(), 1);
		assert!(template.resources.contains_key("MyBucket"));

		let bucket = &template.resources["MyBucket"];
		assert_eq!(bucket.logical_id, "MyBucket");
		assert_eq!(bucket.resource_type, "AWS::S3::Bucket");
		assert_eq!(bucket.properties.len(), 1);

		let bucket_name = bucket.properties.get("BucketName").unwrap();
		assert_eq!(bucket_name.as_str(), Some("test-bucket"));
	}

	#[test]
	fn yaml_converts_multiple_resources() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: test
  MyFunction:
    Type: AWS::Lambda::Function
    Properties:
      FunctionName: test-fn
      Runtime: python3.9
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		assert_eq!(template.resources.len(), 2);
		assert!(template.resources.contains_key("MyBucket"));
		assert!(template.resources.contains_key("MyFunction"));

		let function = &template.resources["MyFunction"];
		assert_eq!(function.resource_type, "AWS::Lambda::Function");
		assert_eq!(function.properties.len(), 2);
	}

	#[test]
	fn yaml_resource_without_properties() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];
		assert_eq!(bucket.properties.len(), 0);
	}

	#[test]
	fn yaml_nested_properties() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketEncryption:
        ServerSideEncryptionConfiguration:
          - ServerSideEncryptionByDefault:
              SSEAlgorithm: AES256
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];
		let encryption = bucket.properties.get("BucketEncryption").unwrap();

		// Check it's an object
		assert!(encryption.as_object().is_some());

		let encryption_obj = encryption.as_object().unwrap();
		assert!(encryption_obj.contains_key("ServerSideEncryptionConfiguration"));
	}

	#[test]
	fn yaml_property_with_array() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      Tags:
        - Key: Environment
          Value: Production
        - Key: Owner
          Value: Team
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];
		let tags = bucket.properties.get("Tags").unwrap();

		let tags_array = tags.as_array().unwrap();
		assert_eq!(tags_array.len(), 2);
	}

	#[test]
	fn yaml_missing_type_returns_error() {
		let text = r#"
Resources:
  MyBucket:
    Properties:
      BucketName: test
"#;

		let result = CfnTemplate::from_yaml(text, &test_uri());

		assert!(result.is_err());
		let diags = result.unwrap_err();
		assert_eq!(diags.len(), 1);
		assert!(diags[0].message.contains("missing required `Type`"));
	}

	#[test]
	fn yaml_invalid_yaml_returns_parse_error() {
		let text = r#"
Resources:
  MyBucket:
    Type: "unclosed string
"#;

		let result = CfnTemplate::from_yaml(text, &test_uri());

		assert!(result.is_err());
		let diags = result.unwrap_err();
		assert_eq!(diags.len(), 1);
		assert!(diags[0].message.contains("YAML parse error"));
	}

	#[test]
	fn yaml_empty_resources_section() {
		let text = r#"
Resources: {}
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();
		assert_eq!(template.resources.len(), 0);
	}

	#[test]
	fn yaml_no_resources_section() {
		let text = r#"
Parameters:
  Env:
    Type: String
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();
		assert_eq!(template.resources.len(), 0);
	}

	#[test]
	fn yaml_preserves_position_info() {
		let text = r#"Resources:
  MyBucket:
    Type: AWS::S3::Bucket
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];

		// MyBucket is on line 1 (0-indexed)
		assert_eq!(bucket.logical_id_range.start.line, 1);

		// Type value is on line 2
		assert_eq!(bucket.type_range.start.line, 2);
	}

	use crate::spec::spec_store::{
		CollectionKind, PrimitiveType, PropertyName, PropertyShape, ResourceTypeDescriptor,
		ResourceTypeId, ShapeKind, SpecStore, TypeInfo,
	};

	fn create_test_spec() -> SpecStore {
		let mut resource_types = std::collections::HashMap::new();

		// AWS::S3::Bucket with BucketName (optional) and BucketEncryption (required)
		let mut s3_bucket_props = std::collections::HashMap::new();
		s3_bucket_props.insert(
			PropertyName("BucketName".into()),
			PropertyShape {
				name: PropertyName("BucketName".into()),
				type_info: TypeInfo {
					kind: ShapeKind::Primitive(PrimitiveType::String),
					collection: CollectionKind::Scalar,
				},
				required: false,
				documentation_url: None,
				update_behavior: None,
				duplicates_allowed: false,
			},
		);
		s3_bucket_props.insert(
			PropertyName("BucketEncryption".into()),
			PropertyShape {
				name: PropertyName("BucketEncryption".into()),
				type_info: TypeInfo {
					kind: ShapeKind::Complex(crate::spec::spec_store::PropertyTypeId(
						"BucketEncryption".into(),
					)),
					collection: CollectionKind::Scalar,
				},
				required: true,
				documentation_url: None,
				update_behavior: None,
				duplicates_allowed: false,
			},
		);

		resource_types.insert(
			ResourceTypeId("AWS::S3::Bucket".into()),
			ResourceTypeDescriptor {
				type_id: ResourceTypeId("AWS::S3::Bucket".into()),
				properties: s3_bucket_props,
				attributes: std::collections::HashMap::new(),
				documentation_url: None,
			},
		);

		SpecStore {
			resource_types,
			property_types: std::collections::HashMap::new(),
		}
	}

	#[test]
	fn validation_detects_unknown_resource_type() {
		let text = r#"
Resources:
  MyResource:
    Type: AWS::FakeService::FakeResource
    Properties:
      SomeProp: value
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();
		let spec = create_test_spec();
		let diags = template.validate_against_spec(&spec, &test_uri());

		assert_eq!(diags.len(), 1);
		assert!(diags[0].message.contains("unknown resource type"));
		assert!(diags[0].message.contains("AWS::FakeService::FakeResource"));
	}

	#[test]
	fn validation_detects_unknown_property() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketEncryption: enabled
      InvalidProperty: value
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();
		let spec = create_test_spec();
		let diags = template.validate_against_spec(&spec, &test_uri());

		assert_eq!(diags.len(), 1);
		assert!(diags[0].message.contains("unknown property"));
		assert!(diags[0].message.contains("InvalidProperty"));
	}

	#[test]
	fn validation_detects_missing_required_property() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: test
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();
		let spec = create_test_spec();
		let diags = template.validate_against_spec(&spec, &test_uri());

		assert_eq!(diags.len(), 1);
		assert!(diags[0].message.contains("missing required property"));
		assert!(diags[0].message.contains("BucketEncryption"));
	}

	#[test]
	fn validation_passes_for_valid_resource() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketEncryption: enabled
      BucketName: test
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();
		let spec = create_test_spec();
		let diags = template.validate_against_spec(&spec, &test_uri());

		assert_eq!(diags.len(), 0);
	}
}
