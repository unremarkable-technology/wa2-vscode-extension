use crate::spec::code_utils::names;
use crate::spec::intrinsics::IntrinsicKind;
use crate::spec::spec_store::{AttributeName, PropertyName, ResourceTypeId, SpecStore, TypeInfo};
use crate::spec::symbol_table::SymbolTable;
use crate::spec::{intrinsics, type_resolver};
use std::collections::HashMap;
use tower_lsp::lsp_types::Range;

/// CloudFormation template intermediate representation
// Update CfnTemplate struct
#[derive(Debug, Clone)]
pub struct CfnTemplate {
	pub resources: HashMap<String, CfnResource>,
	pub parameters: HashMap<String, CfnParameter>,
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
	/// !Ref / { "Ref": "LogicalId" }
	Ref {
		target: String,
		range: Range,
	},

	/// !GetAtt / { "Fn::GetAtt": ["LogicalId", "Attribute"] } / "LogicalId.Attribute"
	GetAtt {
		target: String,
		attribute: String,
		range: Range,
	},

	// !Sub / { "Fn::Sub": "template string" } or { "Fn::Sub": ["template", {vars}] }
	Sub {
		template: String,
		variables: Option<HashMap<String, CfnValue>>, // For long form with explicit variables
		range: Range,
	},
}

// Add to cfn_ir.rs after CfnResource struct

/// A CloudFormation parameter declaration
#[derive(Debug, Clone)]
pub struct CfnParameter {
	pub name: String,
	pub parameter_type: String, // "String", "Number", "List<Number>", etc.
	pub default_value: Option<CfnValue>,
	pub description: Option<String>,

	// Position tracking
	pub name_range: Range,
	pub type_range: Range,
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
			CfnValue::Ref { range, .. } => *range,
			CfnValue::GetAtt { range, .. } => *range,
			CfnValue::Sub { range, .. } => *range,
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

// In cfn_ir.rs, create a helper to build diagnostics with suggestions
fn create_diagnostic_with_suggestion(
	range: Range,
	severity: DiagnosticSeverity,
	code: &str,
	message: String,
	suggestion: Option<String>,
) -> Diagnostic {
	let mut diag = Diagnostic {
		range,
		severity: Some(severity),
		code: Some(NumberOrString::String(code.into())),
		source: Some("wa2-lsp".into()),
		message,
		..Default::default()
	};

	// Store suggestion in data field as JSON
	if let Some(suggestion) = suggestion
		&& let Ok(json) = serde_json::to_value(serde_json::json!({
			"suggestion": suggestion
		})) {
		diag.data = Some(json);
	}

	diag
}

impl CfnTemplate {
	/// Parse a CloudFormation template from YAML text
	pub fn from_yaml(text: &str, uri: &Url) -> Result<Self, Vec<Diagnostic>> {
		let docs = MarkedYaml::load_from_str(text)
			.map_err(|err| vec![yaml_error_to_diagnostic(err, uri)])?;

		if docs.is_empty() {
			return Ok(CfnTemplate {
				resources: HashMap::new(),
				parameters: HashMap::new(),
			});
		}

		Self::from_marked_yaml(&docs[0], uri)
	}

	fn from_marked_yaml(root: &MarkedYaml, uri: &Url) -> Result<Self, Vec<Diagnostic>> {
		let mut resources = HashMap::new();
		let mut parameters = HashMap::new();

		let root_map = match root.data.as_mapping() {
			Some(m) => m,
			None => {
				return Err(vec![Diagnostic {
					range: marked_yaml_to_range(root),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String("WA2_CFN_INVALID_ROOT".into())),
					source: Some("wa2-lsp".into()),
					message: "Template root must be a mapping".to_string(),
					..Default::default()
				}]);
			}
		};

		// Parse Parameters section
		let parameters_node = root_map
			.iter()
			.find(|(k, _)| k.data.as_str() == Some("Parameters"))
			.map(|(_, v)| v);

		if let Some(params_node) = parameters_node {
			let params_map = match params_node.data.as_mapping() {
				Some(m) => m,
				None => {
					return Err(vec![Diagnostic {
						range: marked_yaml_to_range(params_node),
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String("WA2_CFN_INVALID_PARAMETERS".into())),
						source: Some("wa2-lsp".into()),
						message: "Template Parameters section must be a mapping".to_string(),
						..Default::default()
					}]);
				}
			};

			let mut errors = Vec::new();
			for (param_key, param_node) in params_map {
				let param_name = match param_key.data.as_str() {
					Some(s) => s.to_string(),
					None => {
						errors.push(Diagnostic {
							range: marked_yaml_to_range(param_key),
							severity: Some(DiagnosticSeverity::WARNING),
							code: Some(NumberOrString::String(
								"WA2_CFN_PARAMETER_KEY_NOT_STRING".into(),
							)),
							source: Some("wa2-lsp".into()),
							message: "Template: parameter key is not a string".to_string(),
							..Default::default()
						});
						continue;
					}
				};

				match CfnParameter::from_marked_yaml(
					param_name.clone(),
					param_node,
					marked_yaml_to_range(param_key),
					uri,
				) {
					Ok(param) => {
						parameters.insert(param_name, param);
					}
					Err(mut diags) => {
						errors.append(&mut diags);
					}
				}
			}

			if !errors.is_empty() {
				return Err(errors);
			}
		}

		// Parse Resources section
		let resources_node = root_map
			.iter()
			.find(|(k, _)| k.data.as_str() == Some("Resources"))
			.map(|(_, v)| v);

		let resources_node = match resources_node {
			Some(v) => v,
			None => {
				return Ok(CfnTemplate {
					resources: HashMap::new(),
					parameters, // Include parsed parameters
				});
			}
		};

		// ... existing Resources parsing ...
		let resources_map = match resources_node.data.as_mapping() {
			Some(m) => m,
			None => {
				return Err(vec![Diagnostic {
					range: marked_yaml_to_range(resources_node),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String("WA2_CFN_INVALID_RESOURCES".into())),
					source: Some("wa2-lsp".into()),
					message: "Template Resources section must be a mapping".to_string(),
					..Default::default()
				}]);
			}
		};

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
						message: "Template: resource key is not a string; \
                             CloudFormation logical IDs must be strings."
							.to_string(),
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

		Ok(CfnTemplate {
			resources,
			parameters,
		})
	}
}

impl CfnResource {
	fn from_marked_yaml(
		logical_id: String,
		node: &MarkedYaml,
		logical_id_range: Range,
		_uri: &Url,
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
						"Template: resource `{logical_id}` is not a mapping; \
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
						"Template: resource `{logical_id}` is missing required `Type`."
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
		// Default range for this node; used by scalars / arrays / plain objects.
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
			YamlData::Tagged(tag, inner) => {
				let range = marked_yaml_to_range(node);

				// Check if this is a known intrinsic
				if let Some(intrinsic) = intrinsics::get_intrinsic_by_tag(&tag.suffix) {
					match intrinsic.kind {
						IntrinsicKind::Ref => {
							if let YamlData::Value(Scalar::String(target)) = &inner.data {
								return Ok(CfnValue::Ref {
									target: target.to_string(),
									range,
								});
							} else {
								return Err(vec![Diagnostic {
									range,
									severity: Some(DiagnosticSeverity::ERROR),
									code: Some(NumberOrString::String(
										"WA2_CFN_MALFORMED_REF".into(),
									)),
									source: Some("wa2-lsp".into()),
									message: "Malformed !Ref: expected a string value".to_string(),
									..Default::default()
								}]);
							}
						}
						IntrinsicKind::GetAtt => {
							if let YamlData::Value(Scalar::String(s)) = &inner.data {
								if let Some((target, attribute)) = s.split_once('.') {
									return Ok(CfnValue::GetAtt {
										target: target.to_string(),
										attribute: attribute.to_string(),
										range,
									});
								} else {
									return Err(vec![Diagnostic {
										range,
										severity: Some(DiagnosticSeverity::ERROR),
										code: Some(NumberOrString::String(
											"WA2_CFN_MALFORMED_GETATT".into(),
										)),
										source: Some("wa2-lsp".into()),
										message: format!(
											"Malformed !GetAtt: expected 'ResourceName.AttributeName', got '{}'",
											s
										),
										..Default::default()
									}]);
								}
							}
							// Array form: !GetAtt [Resource, Attribute]
							if let YamlData::Sequence(seq) = &inner.data
								&& seq.len() == 2 && let (
								YamlData::Value(Scalar::String(target)),
								YamlData::Value(Scalar::String(attribute)),
							) = (&seq[0].data, &seq[1].data)
							{
								return Ok(CfnValue::GetAtt {
									target: target.to_string(),
									attribute: attribute.to_string(),
									range,
								});
							}

							return Err(vec![Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("WA2_CFN_MALFORMED_GETATT".into())),
                source: Some("wa2-lsp".into()),
                message: "Malformed !GetAtt: expected 'ResourceName.AttributeName' or array [ResourceName, AttributeName]".to_string(),
                ..Default::default()
            }]);
						}
						IntrinsicKind::Sub => {
							// String form: !Sub "template with ${Var}"
							if let YamlData::Value(Scalar::String(template)) = &inner.data {
								return Ok(CfnValue::Sub {
									template: template.to_string(),
									variables: None,
									range,
								});
							}

							// Array form: !Sub ["template", {Var1: val1, Var2: val2}]
							if let YamlData::Sequence(seq) = &inner.data
								&& seq.len() == 2 && let YamlData::Value(Scalar::String(template)) =
								&seq[0].data
							{
								let variables = if let YamlData::Mapping(_) = &seq[1].data {
									Some(
										CfnValue::from_marked_yaml(&seq[1])?
											.as_object()
											.cloned()
											.unwrap_or_default(),
									)
								} else {
									None
								};

								return Ok(CfnValue::Sub {
									template: template.to_string(),
									variables,
									range,
								});
							}

							return Err(vec![Diagnostic {
								range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String("WA2_CFN_MALFORMED_SUB".into())),
								source: Some("wa2-lsp".into()),
								message:
									"Malformed !Sub: expected string or array [template, variables]"
										.to_string(),
								..Default::default()
							}]);
						}
					}
				}

				// Unknown tag - fallback
				return CfnValue::from_marked_yaml(inner);
			}

			// ----- Collections -----
			YamlData::Sequence(seq) => {
				let items: Result<Vec<_>, _> = seq.iter().map(CfnValue::from_marked_yaml).collect();
				CfnValue::Array(items?, range)
			}

			YamlData::Mapping(map) => {
				// Intrinsic long-form detection:
				//
				//   SomeProp:
				//     Ref: MyBucket
				//
				//   SomeProp:
				//     Fn::GetAtt: [MyBucket, Arn]
				//
				if map.len() == 1 {
					let (k, v) = map.iter().next().unwrap();

					// key must be a simple string
					if let YamlData::Value(Scalar::String(name)) = &k.data {
						// For intrinsics, we want the range of the *value node*
						// (the Ref / Fn::GetAtt payload), not the outer mapping.
						let inner_range = marked_yaml_to_range(v);

						match name.as_ref() {
							"Ref" => {
								// Ref target must be a string scalar
								if let YamlData::Value(Scalar::String(target)) = &v.data {
									return Ok(CfnValue::Ref {
										target: target.to_string(),
										range: inner_range,
									});
								}
							}
							"Fn::GetAtt" => {
								// GetAtt can be ["LogicalId", "Attribute"]
								// or "LogicalId.Attribute"
								match &v.data {
									YamlData::Sequence(seq) if seq.len() == 2 => {
										let res = &seq[0];
										let attr = &seq[1];

										if let (
											YamlData::Value(Scalar::String(target)),
											YamlData::Value(Scalar::String(attribute)),
										) = (&res.data, &attr.data)
										{
											return Ok(CfnValue::GetAtt {
												target: target.to_string(),
												attribute: attribute.to_string(),
												range: inner_range,
											});
										}
									}
									YamlData::Value(Scalar::String(s)) => {
										if let Some((target, attribute)) = s.split_once('.') {
											return Ok(CfnValue::GetAtt {
												target: target.to_string(),
												attribute: attribute.to_string(),
												range: inner_range,
											});
										}
									}
									_ => {}
								}
							}
							_ => {}
						}
					}
				}

				// Fallback: normal object mapping
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

// CfnParameter::from_marked_yaml implementation
impl CfnParameter {
	fn from_marked_yaml(
		name: String,
		node: &MarkedYaml,
		name_range: Range,
		_uri: &Url,
	) -> Result<Self, Vec<Diagnostic>> {
		let param_map = match node.data.as_mapping() {
			Some(m) => m,
			None => {
				return Err(vec![Diagnostic {
					range: marked_yaml_to_range(node),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_PARAMETER_NOT_MAPPING".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!(
						"Template: parameter `{name}` is not a mapping; \
                         CloudFormation parameters must be mappings with `Type`."
					),
					..Default::default()
				}]);
			}
		};

		// Extract Type (required)
		let (type_str, type_range) = param_map
			.iter()
			.find(|(k, _)| k.data.as_str() == Some("Type"))
			.and_then(|(_, v)| {
				v.data
					.as_str()
					.map(|s| (s.to_string(), marked_yaml_to_range(v)))
			})
			.ok_or_else(|| {
				vec![Diagnostic {
					range: name_range,
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_PARAMETER_TYPE_MISSING".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!("Template: parameter `{name}` is missing required `Type`."),
					..Default::default()
				}]
			})?;

		// Extract Default (optional)
		let default_value = param_map
			.iter()
			.find(|(k, _)| k.data.as_str() == Some("Default"))
			.map(|(_, v)| CfnValue::from_marked_yaml(v))
			.transpose()?;

		// Extract Description (optional)
		let description = param_map
			.iter()
			.find(|(k, _)| k.data.as_str() == Some("Description"))
			.and_then(|(_, v)| v.data.as_str().map(|s| s.to_string()));

		Ok(CfnParameter {
			name,
			parameter_type: type_str,
			default_value,
			description,
			name_range,
			type_range,
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

impl CfnTemplate {
	/// Validate this template against the CloudFormation spec
	pub fn validate_against_spec(&self, spec_store: &SpecStore, _uri: &Url) -> Vec<Diagnostic> {
		let mut diagnostics = Vec::new();

		// Build symbol table for intrinsic validation
		let symbols = SymbolTable::from_template(self);

		// Validate each resource
		for (logical_id, resource) in &self.resources {
			let type_id = ResourceTypeId(resource.resource_type.clone());

			// Check if resource type exists in spec
			if !spec_store.resource_types.contains_key(&type_id) {
				diagnostics.push(Diagnostic {
					range: resource.type_range,
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_UNKNOWN_RESOURCE_TYPE".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!(
						"Unknown CloudFormation resource type: {} (in resource `{}`)",
						resource.resource_type, logical_id
					),
					..Default::default()
				});
				continue;
			}

			// Get resource spec for property validation
			let resource_spec = match spec_store.resource_types.get(&type_id) {
				Some(spec) => spec,
				None => continue,
			};

			// Check for required properties
			for (prop_name, prop_spec) in &resource_spec.properties {
				if prop_spec.required && !resource.properties.contains_key(&prop_name.0) {
					diagnostics.push(Diagnostic {
						range: resource.logical_id_range,
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String(
							"WA2_CFN_REQUIRED_PROPERTY_MISSING".into(),
						)),
						source: Some("wa2-lsp".into()),
						message: format!(
							"Resource `{}` is missing required property `{}`",
							logical_id, prop_name.0
						),
						..Default::default()
					});
				}
			}

			// Check for unknown properties and validate types
			for (prop_name, prop_value) in &resource.properties {
				let prop_id = PropertyName(prop_name.clone());

				match resource_spec.properties.get(&prop_id) {
					Some(prop_spec) => {
						// Property exists - validate type
						Self::validate_property_type(
							prop_value,
							&prop_spec.type_info,
							&symbols,
							spec_store,
							logical_id,
							prop_name,
							&mut diagnostics,
						);
					}
					// For unknown properties
					None => {
						let candidates = resource_spec.properties.keys().map(|k| k.0.as_str());
						let suggestion_data = names::find_closest(prop_name, candidates);

						let (message, suggestion) = if let Some((suggested, _)) = suggestion_data {
							(
								format!(
									"Unknown property `{}` for resource type `{}` (in resource `{}`). Did you mean `{}`?",
									prop_name, resource.resource_type, logical_id, suggested
								),
								Some(suggested.to_string()),
							)
						} else {
							(
								format!(
									"Unknown property `{}` for resource type `{}` (in resource `{}`)",
									prop_name, resource.resource_type, logical_id
								),
								None,
							)
						};

						diagnostics.push(create_diagnostic_with_suggestion(
							prop_value.range(),
							DiagnosticSeverity::WARNING,
							"WA2_CFN_UNKNOWN_PROPERTY",
							message,
							suggestion,
						));
					}
				}

				// Validate intrinsic functions (existence checks)
				Self::validate_intrinsics(prop_value, &symbols, &mut diagnostics, spec_store); // Pass spec_store
			}
		}

		diagnostics
	}

	// Validate that a property value's type matches what the spec expects
	fn validate_property_type(
		value: &CfnValue,
		expected: &TypeInfo,
		symbols: &SymbolTable,
		spec: &SpecStore,
		resource_id: &str,
		prop_name: &str,
		diagnostics: &mut Vec<Diagnostic>,
	) {
		// Resolve the actual type of the value
		let actual = match type_resolver::resolve_type(value, symbols, spec) {
			Some(t) => t,
			None => return, // Can't resolve type (e.g., null, unknown intrinsic)
		};

		// Check if types are compatible
		if !Self::types_compatible(&actual, expected) {
			diagnostics.push(Diagnostic {
				range: value.range(),
				severity: Some(DiagnosticSeverity::ERROR),
				code: Some(NumberOrString::String("WA2_CFN_TYPE_MISMATCH".into())),
				source: Some("wa2-lsp".into()),
				message: format!(
					"Type mismatch in resource `{}` property `{}`: expected {}, got {}",
					resource_id,
					prop_name,
					Self::format_type(expected),
					Self::format_type(&actual)
				),
				..Default::default()
			});
		}
	}

	// Check if two types are compatible
	fn types_compatible(actual: &TypeInfo, expected: &TypeInfo) -> bool {
		use crate::spec::spec_store::{CollectionKind, PrimitiveType, ShapeKind};

		// Check collection compatibility first
		match (&actual.collection, &expected.collection) {
			(CollectionKind::Scalar, CollectionKind::Scalar) => {}
			(CollectionKind::List, CollectionKind::List) => {}
			(CollectionKind::Map, CollectionKind::Map) => {}
			_ => return false, // Collection mismatch
		}

		// Check shape compatibility
		match (&actual.kind, &expected.kind) {
			// Exact match
			(ShapeKind::Primitive(a), ShapeKind::Primitive(e)) if a == e => true,

			// Any accepts anything
			(_, ShapeKind::Any) => true,
			(ShapeKind::Any, _) => true,

			// Number type compatibility - be lenient like CloudFormation
			// Integer/Long/Double can all be used interchangeably
			(
				ShapeKind::Primitive(PrimitiveType::Integer),
				ShapeKind::Primitive(PrimitiveType::Double),
			) => true,
			(
				ShapeKind::Primitive(PrimitiveType::Integer),
				ShapeKind::Primitive(PrimitiveType::Long),
			) => true,
			(
				ShapeKind::Primitive(PrimitiveType::Long),
				ShapeKind::Primitive(PrimitiveType::Double),
			) => true,
			(
				ShapeKind::Primitive(PrimitiveType::Long),
				ShapeKind::Primitive(PrimitiveType::Integer),
			) => true,
			(
				ShapeKind::Primitive(PrimitiveType::Double),
				ShapeKind::Primitive(PrimitiveType::Integer),
			) => true,
			(
				ShapeKind::Primitive(PrimitiveType::Double),
				ShapeKind::Primitive(PrimitiveType::Long),
			) => true,

			// Complex types must match exactly (for now)
			(ShapeKind::Complex(a), ShapeKind::Complex(e)) => a == e,

			_ => false,
		}
	}

	// Format a type for display in error messages
	fn format_type(type_info: &TypeInfo) -> String {
		use crate::spec::spec_store::{CollectionKind, PrimitiveType, ShapeKind};

		let base = match &type_info.kind {
			ShapeKind::Primitive(p) => match p {
				PrimitiveType::String => "String",
				PrimitiveType::Integer => "Integer",
				PrimitiveType::Long => "Long",
				PrimitiveType::Double => "Double",
				PrimitiveType::Boolean => "Boolean",
				PrimitiveType::Timestamp => "Timestamp",
				PrimitiveType::Json => "Json",
				PrimitiveType::Other(s) => s.as_str(),
			},
			ShapeKind::Complex(id) => &id.0,
			ShapeKind::Any => "Any",
		};

		match type_info.collection {
			CollectionKind::Scalar => base.to_string(),
			CollectionKind::List => format!("List<{}>", base),
			CollectionKind::Map => format!("Map<{}>", base),
		}
	}

	// Add this function before validate_intrinsics:
	/// Extract ${Variable} references from a Sub template string
	fn extract_sub_variables(template: &str) -> Vec<String> {
		let mut variables = Vec::new();
		let mut chars = template.chars().peekable();

		while let Some(ch) = chars.next() {
			if ch == '$' && chars.peek() == Some(&'{') {
				chars.next(); // consume '{'
				let mut var_name = String::new();

				while let Some(&ch) = chars.peek() {
					if ch == '}' {
						chars.next(); // consume '}'
						if !var_name.is_empty() {
							variables.push(var_name);
						}
						break;
					}
					var_name.push(ch);
					chars.next();
				}
			}
		}

		variables
	}

	fn validate_intrinsics(
		value: &CfnValue,
		symbols: &SymbolTable,
		diagnostics: &mut Vec<Diagnostic>,
		spec: &SpecStore, // NEW parameter
	) {
		match value {
			CfnValue::Ref { target, range } => {
				// For invalid Ref
				if !symbols.has_ref_target(target) {
					let mut candidates: Vec<&str> = Vec::new();
					candidates.extend(symbols.resources.keys().map(|s| s.as_str()));
					candidates.extend(symbols.parameters.keys().map(|s| s.as_str()));
					candidates.extend(symbols.pseudo_parameters.keys().map(|s| s.as_str()));

					let suggestion_data = names::find_closest(target, candidates.into_iter());

					let (message, suggestion) = if let Some((suggested, _)) = suggestion_data {
						(
							format!(
								"!Ref target `{}` does not exist. Did you mean `{}`?",
								target, suggested
							),
							Some(suggested.to_string()),
						)
					} else {
						(
							format!(
								"!Ref target `{}` does not exist. Must reference a resource, parameter, or pseudo-parameter.",
								target
							),
							None,
						)
					};

					diagnostics.push(create_diagnostic_with_suggestion(
						*range,
						DiagnosticSeverity::ERROR,
						"WA2_CFN_INVALID_REF",
						message,
						suggestion,
					));
				}
			}
			CfnValue::GetAtt {
				target,
				attribute,
				range,
			} => {
				// Check if the resource exists
				if !symbols.has_resource(target) {
					diagnostics.push(Diagnostic {
						range: *range,
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String(
							"WA2_CFN_INVALID_GETATT_RESOURCE".into(),
						)),
						source: Some("wa2-lsp".into()),
						message: format!("!GetAtt target resource `{}` does not exist.", target),
						..Default::default()
					});
					return; // Can't validate attribute if resource doesn't exist
				}

				// Validate attribute exists for the resource type
				if let Some(resource_entry) = symbols.resources.get(target) {
					let type_id = ResourceTypeId(resource_entry.resource_type.clone());
					if let Some(resource_spec) = spec.resource_types.get(&type_id) {
						let attr_name = AttributeName(attribute.clone());
						// For GetAtt attributes
						if !resource_spec.attributes.contains_key(&attr_name) {
							let candidates = resource_spec.attributes.keys().map(|k| k.0.as_str());
							let suggestion_data = names::find_closest(attribute, candidates);

							let (message, suggestion) = if let Some((suggested, _)) =
								suggestion_data
							{
								(
									format!(
										"!GetAtt attribute `{}` does not exist on resource type `{}`. Did you mean `{}`?",
										attribute, resource_entry.resource_type, suggested
									),
									Some(serde_json::json!({
										"kind": "getatt",
										"target": target,
										"attribute": suggested
									})),
								)
							} else {
								(
									format!(
										"!GetAtt attribute `{}` does not exist on resource type `{}`. This resource type has no attributes.",
										attribute, resource_entry.resource_type
									),
									None,
								)
							};

							let mut diag = Diagnostic {
								range: *range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String(
									"WA2_CFN_INVALID_GETATT_ATTRIBUTE".into(),
								)),
								source: Some("wa2-lsp".into()),
								message,
								..Default::default()
							};

							if let Some(sugg) = suggestion {
								diag.data = Some(sugg);
							}

							diagnostics.push(diag);
						}
					}
				}
			}
			// In validate_intrinsics() method, add before the Array/Object cases:
			CfnValue::Sub {
				template,
				variables,
				range,
			} => {
				// Extract ${Variable} references from template
				let var_refs = Self::extract_sub_variables(template);

				for var_ref in var_refs {
					// Check if variable exists in explicit variables map
					if let Some(vars) = variables {
						if vars.contains_key(&var_ref) {
							continue; // Found in explicit variables
						}
					}

					// Check if it's a valid Ref target (resource/parameter/pseudo-param)
					if !symbols.has_ref_target(&var_ref) {
						let mut candidates: Vec<&str> = Vec::new();
						candidates.extend(symbols.resources.keys().map(|s| s.as_str()));
						candidates.extend(symbols.parameters.keys().map(|s| s.as_str()));
						candidates.extend(symbols.pseudo_parameters.keys().map(|s| s.as_str()));

						let suggestion_data = crate::spec::code_utils::names::find_closest(
							&var_ref,
							candidates.into_iter(),
						);

						let message = if let Some((suggested, _)) = suggestion_data {
							format!(
								"!Sub variable `${{{}}}` does not exist. Did you mean `{}`?",
								var_ref, suggested
							)
						} else {
							format!(
								"!Sub variable `${{{}}}` does not exist. Must reference a resource, parameter, or pseudo-parameter.",
								var_ref
							)
						};

						diagnostics.push(Diagnostic {
							range: *range,
							severity: Some(DiagnosticSeverity::ERROR),
							code: Some(NumberOrString::String(
								"WA2_CFN_INVALID_SUB_VARIABLE".into(),
							)),
							source: Some("wa2-lsp".into()),
							message,
							..Default::default()
						});
					}
				}

				// Recursively validate variable values if present
				if let Some(vars) = variables {
					for val in vars.values() {
						Self::validate_intrinsics(val, symbols, diagnostics, spec);
					}
				}
			}
			CfnValue::Array(items, _) => {
				for item in items {
					Self::validate_intrinsics(item, symbols, diagnostics, spec); // Pass spec
				}
			}
			CfnValue::Object(map, _) => {
				for val in map.values() {
					Self::validate_intrinsics(val, symbols, diagnostics, spec); // Pass spec
				}
			}
			CfnValue::String(..)
			| CfnValue::Number(..)
			| CfnValue::Bool(..)
			| CfnValue::Null(..) => {}
		}
	}
}

use jsonc_parser::ast::Value;
use jsonc_parser::common::Ranged;
use jsonc_parser::{ParseOptions, errors::ParseError, parse_to_ast};

impl CfnTemplate {
	/// Parse a CloudFormation template from JSON text
	pub fn from_json(text: &str, uri: &Url) -> Result<Self, Vec<Diagnostic>> {
		let parse_options = ParseOptions {
			allow_comments: true,
			allow_trailing_commas: true,
			allow_loose_object_property_names: false,
			allow_hexadecimal_numbers: false,
			allow_single_quoted_strings: false,
			allow_unary_plus_numbers: false,
		};

		let parse_result = parse_to_ast(text, &Default::default(), &parse_options)
			.map_err(|err| vec![json_error_to_diagnostic(err, uri)])?;

		// Get the value from ParseResult
		let root = parse_result.value.ok_or_else(|| {
			vec![Diagnostic {
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
				severity: Some(DiagnosticSeverity::ERROR),
				code: Some(NumberOrString::String("WA2_JSON_EMPTY".into())),
				source: Some("wa2-lsp".into()),
				message: "Template is empty".to_string(),
				..Default::default()
			}]
		})?;

		Self::from_json_ast(&root, text, uri)
	}

	fn from_json_ast(root: &Value, text: &str, uri: &Url) -> Result<Self, Vec<Diagnostic>> {
		let mut resources = HashMap::new();
		let mut parameters = HashMap::new();

		let root_obj = match root.as_object() {
			Some(obj) => obj,
			None => {
				return Err(vec![Diagnostic {
					range: ranged_to_range(root, text),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String("WA2_CFN_INVALID_ROOT".into())),
					source: Some("wa2-lsp".into()),
					message: "Template root must be an object".to_string(),
					..Default::default()
				}]);
			}
		};

		// Parse Parameters section
		if let Some(params_prop) = root_obj.get("Parameters") {
			let params_obj = match params_prop.value.as_object() {
				Some(obj) => obj,
				None => {
					return Err(vec![Diagnostic {
						range: ranged_to_range(&params_prop.value, text),
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String("WA2_CFN_INVALID_PARAMETERS".into())),
						source: Some("wa2-lsp".into()),
						message: "Template Parameters section must be an object".to_string(),
						..Default::default()
					}]);
				}
			};

			let mut errors = Vec::new();
			for prop in params_obj.properties.iter() {
				let param_name = prop.name.clone().into_string();
				let name_range = ranged_to_range(&prop.name, text);
				let param_value = prop.value.clone();

				match CfnParameter::from_json_ast(
					param_name.clone(),
					&param_value,
					name_range,
					text,
					uri,
				) {
					Ok(param) => {
						parameters.insert(param_name, param);
					}
					Err(mut diags) => {
						errors.append(&mut diags);
					}
				}
			}

			if !errors.is_empty() {
				return Err(errors);
			}
		}

		// Find Resources section
		let resources_value = root_obj.get("Resources").map(|prop| prop.value.clone());
		let resources_value = match resources_value {
			Some(v) => v,
			None => {
				// No Resources section - return empty template
				return Ok(CfnTemplate {
					resources: HashMap::new(),
					parameters: HashMap::new(),
				});
			}
		};

		let resources_obj = match resources_value.as_object() {
			Some(obj) => obj,
			None => {
				return Err(vec![Diagnostic {
					range: ranged_to_range(&resources_value, text),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String("WA2_CFN_INVALID_RESOURCES".into())),
					source: Some("wa2-lsp".into()),
					message: "Template Resources section must be an object".to_string(),
					..Default::default()
				}]);
			}
		};

		// Convert each resource - iterate over properties
		let mut errors = Vec::new();
		for prop in resources_obj.properties.iter() {
			let logical_id = prop.name.clone().into_string();
			let logical_id_range = ranged_to_range(&prop.name, text);
			let resource_value = prop.value.clone();

			match CfnResource::from_json_ast(
				logical_id.clone(),
				&resource_value,
				logical_id_range,
				text,
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

		Ok(CfnTemplate {
			resources,
			parameters,
		})
	}
}

/// Convert anything with a range to an LSP Range
fn ranged_to_range<T: Ranged>(node: &T, text: &str) -> Range {
	let json_range = node.range();

	Range {
		start: byte_offset_to_position(text, json_range.start),
		end: byte_offset_to_position(text, json_range.end),
	}
}

impl CfnResource {
	fn from_json_ast(
		logical_id: String,
		node: &Value,
		logical_id_range: Range,
		text: &str,
		_uri: &Url,
	) -> Result<Self, Vec<Diagnostic>> {
		let resource_obj = match node.as_object() {
			Some(obj) => obj,
			None => {
				return Err(vec![Diagnostic {
					range: json_ast_to_range(node, text),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_RESOURCE_NOT_MAPPING".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!(
						"Template: resource `{logical_id}` is not an object; \
                         CloudFormation resources must be objects with `Type` and `Properties`."
					),
					..Default::default()
				}]);
			}
		};

		// Extract Type
		let type_prop = resource_obj.get("Type").map(|p| p.value.clone());
		let (type_str, type_range) = match type_prop {
			Some(v) => match v.as_string_lit() {
				Some(s) => (s.value.to_string(), ranged_to_range(&v, text)),
				None => {
					return Err(vec![Diagnostic {
						range: logical_id_range,
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String(
							"WA2_CFN_RESOURCE_TYPE_MISSING".into(),
						)),
						source: Some("wa2-lsp".into()),
						message: format!(
							"Template: resource `{logical_id}` is missing required `Type`."
						),
						..Default::default()
					}]);
				}
			},
			None => {
				return Err(vec![Diagnostic {
					range: logical_id_range,
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_RESOURCE_TYPE_MISSING".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!(
						"Template: resource `{logical_id}` is missing required `Type`."
					),
					..Default::default()
				}]);
			}
		};

		// Extract Properties (optional)
		let properties = resource_obj
			.get("Properties")
			.map(|p| p.value.clone())
			.as_ref()
			.map(|v| CfnValue::from_json_ast(v, text))
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

impl CfnValue {
	fn from_json_ast(node: &Value, text: &str) -> Result<Self, Vec<Diagnostic>> {
		// Default range for this node
		let range = json_ast_to_range(node, text);

		// Use as_string_lit(), as_number_lit(), as_boolean_lit()
		if let Some(s) = node.as_string_lit() {
			return Ok(CfnValue::String(s.value.to_string(), range));
		}

		if let Some(n) = node.as_number_lit() {
			return Ok(CfnValue::Number(
				n.value.parse::<f64>().unwrap_or(0.0),
				range,
			));
		}

		if let Some(b) = node.as_boolean_lit() {
			return Ok(CfnValue::Bool(b.value, range));
		}

		// Check for null - try to match against known null patterns
		// If none of the above matched and it's not array/object, assume null
		if node.as_array().is_none() && node.as_object().is_none() {
			return Ok(CfnValue::Null(range));
		}

		if let Some(arr) = node.as_array() {
			let items: Result<Vec<_>, _> = arr
				.elements
				.iter()
				.map(|item| CfnValue::from_json_ast(item, text))
				.collect();
			return Ok(CfnValue::Array(items?, range));
		}

		if let Some(obj) = node.as_object() {
			// Intrinsic-detection: objects with a single key "Ref" or "Fn::GetAtt"
			if obj.properties.len() == 1 {
				let prop = &obj.properties[0];
				let key = prop.name.clone().into_string();
				let value = &prop.value;

				// Check if this is a known intrinsic
				if let Some(intrinsic) = intrinsics::get_intrinsic_by_json_key(&key) {
					let inner_range = json_ast_to_range(value, text);

					match intrinsic.kind {
						IntrinsicKind::Ref => {
							if let Some(s) = value.as_string_lit() {
								return Ok(CfnValue::Ref {
									target: s.value.to_string(),
									range: inner_range,
								});
							}
						}
						IntrinsicKind::GetAtt => {
							// Array form: ["LogicalId", "Attribute"]
							if let Some(arr) = value.as_array()
								&& arr.elements.len() == 2
								&& let (Some(target), Some(attribute)) = (
									arr.elements[0].as_string_lit(),
									arr.elements[1].as_string_lit(),
								) {
								return Ok(CfnValue::GetAtt {
									target: target.value.to_string(),
									attribute: attribute.value.to_string(),
									range: inner_range,
								});
							}
							// String form: "LogicalId.Attribute"
							if let Some(s) = value.as_string_lit()
								&& let Some((target, attribute)) = s.value.split_once('.')
							{
								return Ok(CfnValue::GetAtt {
									target: target.to_string(),
									attribute: attribute.to_string(),
									range: inner_range,
								});
							}
						}
						IntrinsicKind::Sub => {
							// String form: {"Fn::Sub": "template"}
							if let Some(s) = value.as_string_lit() {
								return Ok(CfnValue::Sub {
									template: s.value.to_string(),
									variables: None,
									range: inner_range,
								});
							}

							// Array form: {"Fn::Sub": ["template", {vars}]}
							if let Some(arr) = value.as_array()
								&& arr.elements.len() == 2
								&& let Some(template) = arr.elements[0].as_string_lit()
							{
								let variables = if arr.elements[1].as_object().is_some() {
									Some(
										CfnValue::from_json_ast(&arr.elements[1], text)?
											.as_object()
											.cloned()
											.unwrap_or_default(),
									)
								} else {
									None
								};

								return Ok(CfnValue::Sub {
									template: template.value.to_string(),
									variables,
									range: inner_range,
								});
							}
						}
					}
				}
			}

			// Fallback: plain object → CfnValue::Object
			let mut map = HashMap::new();
			for prop in obj.properties.iter() {
				let key = prop.name.clone().into_string();
				let value = CfnValue::from_json_ast(&prop.value, text)?;
				map.insert(key, value);
			}
			return Ok(CfnValue::Object(map, range));
		}

		// Fallback - treat as null
		Ok(CfnValue::Null(range))
	}
}

impl CfnParameter {
	fn from_json_ast(
		name: String,
		node: &Value,
		name_range: Range,
		text: &str,
		_uri: &Url,
	) -> Result<Self, Vec<Diagnostic>> {
		let param_obj = match node.as_object() {
			Some(o) => o,
			None => {
				return Err(vec![Diagnostic {
					range: ranged_to_range(node, text),
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_PARAMETER_NOT_OBJECT".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!(
						"Template: parameter `{name}` is not an object; \
                         CloudFormation parameters must be objects with `Type`."
					),
					..Default::default()
				}]);
			}
		};

		// Extract Type (required)
		let (type_str, type_range) = param_obj
			.get("Type")
			.and_then(|prop| {
				prop.value
					.as_string_lit()
					.map(|s| (s.value.to_string(), ranged_to_range(&prop.value, text)))
			})
			.ok_or_else(|| {
				vec![Diagnostic {
					range: name_range,
					severity: Some(DiagnosticSeverity::ERROR),
					code: Some(NumberOrString::String(
						"WA2_CFN_PARAMETER_TYPE_MISSING".into(),
					)),
					source: Some("wa2-lsp".into()),
					message: format!("Template: parameter `{name}` is missing required `Type`."),
					..Default::default()
				}]
			})?;

		// Extract Default (optional)
		let default_value = param_obj
			.get("Default")
			.map(|prop| CfnValue::from_json_ast(&prop.value, text))
			.transpose()?;

		// Extract Description (optional)
		let description = param_obj
			.get("Description")
			.and_then(|prop| prop.value.as_string_lit().map(|s| s.value.to_string()));

		Ok(CfnParameter {
			name,
			parameter_type: type_str,
			default_value,
			description,
			name_range,
			type_range,
		})
	}
}

/// Convert byte offset to line/column position
fn byte_offset_to_position(text: &str, offset: usize) -> Position {
	let mut line = 0u32;
	let mut character = 0u32;

	for (byte_idx, ch) in text.char_indices() {
		if byte_idx >= offset {
			break;
		}
		if ch == '\n' {
			line += 1;
			character = 0;
		} else {
			character += 1;
		}
	}

	Position { line, character }
}

/// Convert a jsonc-parser AST Value to an LSP Range
fn json_ast_to_range(node: &Value, text: &str) -> Range {
	ranged_to_range(node, text)
}

/// Convert a jsonc-parser error to an LSP diagnostic
fn json_error_to_diagnostic(err: ParseError, uri: &Url) -> Diagnostic {
	// ParseError doesn't have text, so use simple range
	let json_range = err.range();

	let range = Range {
		start: Position {
			line: 0,
			character: json_range.start as u32,
		},
		end: Position {
			line: 0,
			character: json_range.end as u32,
		},
	};

	Diagnostic {
		range,
		severity: Some(DiagnosticSeverity::ERROR),
		code: Some(NumberOrString::String("WA2_JSON_PARSE".into())),
		source: Some("wa2-lsp".into()),
		message: format!("JSON parse error in {uri}: {}", err),
		..Default::default()
	}
}

use std::fmt;

impl fmt::Display for CfnTemplate {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "Template:")?;

		// Display Parameters section
		if !self.parameters.is_empty() {
			writeln!(f, "{}Parameters:", " ".repeat(2))?;
			let mut params: Vec<_> = self.parameters.iter().collect();
			params.sort_by_key(|(name, _)| *name);
			for (_, param) in params {
				for line in param.to_string().lines() {
					writeln!(f, "{}{}", " ".repeat(4), line)?;
				}
			}
		}

		// Display Resources section
		if !self.resources.is_empty() {
			writeln!(f, "{}Resources:", " ".repeat(2))?;
			for (logical_id, resource) in &self.resources {
				writeln!(f, "{}{}:", " ".repeat(4), logical_id)?;
				for line in resource.to_string().lines() {
					writeln!(f, "{}{}", " ".repeat(2), line)?;
				}
			}
		}

		Ok(())
	}
}

impl fmt::Display for CfnResource {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "{}Type: {}", " ".repeat(4), self.resource_type)?;
		if !self.properties.is_empty() {
			writeln!(f, "    Properties:")?;
			// Sort properties by name for deterministic output
			let mut props: Vec<_> = self.properties.iter().collect();
			props.sort_by_key(|(name, _)| *name);

			for (name, value) in props {
				writeln!(f, "{}{}: {}", " ".repeat(6), name, value)?;
			}
		}
		Ok(())
	}
}

impl fmt::Display for CfnValue {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			CfnValue::String(s, _) => write!(f, "\"{}\"", s),
			CfnValue::Number(n, _) => write!(f, "{}", n),
			CfnValue::Bool(b, _) => write!(f, "{}", b),
			CfnValue::Null(_) => write!(f, "null"),
			CfnValue::Ref { target, .. } => write!(f, "!Ref {}", target),
			CfnValue::GetAtt {
				target, attribute, ..
			} => {
				write!(f, "!GetAtt {}.{}", target, attribute)
			}
			// In Display for CfnValue:
			CfnValue::Sub {
				template,
				variables,
				..
			} => {
				if let Some(_vars) = variables {
					write!(f, "!Sub [{}, {{...}}]", template)
				} else {
					write!(f, "!Sub \"{}\"", template)
				}
			}
			CfnValue::Array(items, _) => {
				write!(f, "[")?;
				for (i, item) in items.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", item)?;
				}
				write!(f, "]")
			}
			CfnValue::Object(map, _) => {
				write!(f, "{{")?;
				for (i, (k, v)) in map.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}: {}", k, v)?;
				}
				write!(f, "}}")
			}
		}
	}
}

impl fmt::Display for CfnParameter {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "{}:", self.name)?;
		writeln!(f, "  Type: {}", self.parameter_type)?;
		if let Some(ref desc) = self.description {
			writeln!(f, "  Description: {}", desc)?;
		}
		if let Some(ref default) = self.default_value {
			writeln!(f, "  Default: {}", default)?;
		}
		Ok(())
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

	// In cfn_ir.rs, update create_test_spec()
	fn create_test_spec() -> SpecStore {
		let mut resource_types = std::collections::HashMap::new();

		// AWS::S3::Bucket with BucketName and BucketEncryption as simple strings (not complex)
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
					kind: ShapeKind::Primitive(PrimitiveType::String), // Changed from Complex to String
					collection: CollectionKind::Scalar,
				},
				required: true,
				documentation_url: None,
				update_behavior: None,
				duplicates_allowed: false,
			},
		);
		s3_bucket_props.insert(
			PropertyName("Tags".into()),
			PropertyShape {
				name: PropertyName("Tags".into()),
				type_info: TypeInfo {
					kind: ShapeKind::Primitive(PrimitiveType::String),
					collection: CollectionKind::List, // List of strings for simplicity
				},
				required: false,
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

		// AWS::Lambda::Function with required properties as strings
		let mut lambda_props = std::collections::HashMap::new();
		lambda_props.insert(
			PropertyName("FunctionName".into()),
			PropertyShape {
				name: PropertyName("FunctionName".into()),
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
		lambda_props.insert(
			PropertyName("Code".into()),
			PropertyShape {
				name: PropertyName("Code".into()),
				type_info: TypeInfo {
					kind: ShapeKind::Primitive(PrimitiveType::String), // Simplified for testing
					collection: CollectionKind::Scalar,
				},
				required: true,
				documentation_url: None,
				update_behavior: None,
				duplicates_allowed: false,
			},
		);
		lambda_props.insert(
			PropertyName("Runtime".into()),
			PropertyShape {
				name: PropertyName("Runtime".into()),
				type_info: TypeInfo {
					kind: ShapeKind::Primitive(PrimitiveType::String),
					collection: CollectionKind::Scalar,
				},
				required: true,
				documentation_url: None,
				update_behavior: None,
				duplicates_allowed: false,
			},
		);
		lambda_props.insert(
			PropertyName("RoleArn".into()),
			PropertyShape {
				name: PropertyName("RoleArn".into()),
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

		resource_types.insert(
			ResourceTypeId("AWS::Lambda::Function".into()),
			ResourceTypeDescriptor {
				type_id: ResourceTypeId("AWS::Lambda::Function".into()),
				properties: lambda_props,
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
		assert!(
			diags[0]
				.message
				.contains("Unknown CloudFormation resource type")
		); // Changed capitalization
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
		assert!(diags[0].message.contains("Unknown property")); // Changed capitalization
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

	#[test]
	fn yaml_intrinsic_ref_long_form_converts_to_ref_value() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName:
        Ref: OtherBucket
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];
		let bucket_name = bucket
			.properties
			.get("BucketName")
			.expect("BucketName property must exist");

		match bucket_name {
			CfnValue::Ref { target, .. } => {
				assert_eq!(target, "OtherBucket");
			}
			other => panic!("expected Ref CfnValue, got {:?}", other),
		}
	}

	#[test]
	fn yaml_intrinsic_getatt_array_form_converts_to_getatt_value() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      Arn:
        Fn::GetAtt: [MyBucket, Arn]
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];
		let arn = bucket
			.properties
			.get("Arn")
			.expect("Arn property must exist");

		match arn {
			CfnValue::GetAtt {
				target, attribute, ..
			} => {
				assert_eq!(target, "MyBucket");
				assert_eq!(attribute, "Arn");
			}
			other => panic!("expected GetAtt CfnValue, got {:?}", other),
		}
	}

	#[test]
	fn yaml_intrinsic_getatt_string_form_converts_to_getatt_value() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      Arn:
        Fn::GetAtt: MyBucket.Arn
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];
		let arn = bucket
			.properties
			.get("Arn")
			.expect("Arn property must exist");

		match arn {
			CfnValue::GetAtt {
				target, attribute, ..
			} => {
				assert_eq!(target, "MyBucket");
				assert_eq!(attribute, "Arn");
			}
			other => panic!("expected GetAtt CfnValue, got {:?}", other),
		}
	}

	#[test]
	fn json_intrinsic_ref_object_converts_to_ref_value() {
		let text = r#"
{
  "Resources": {
    "MyBucket": {
      "Type": "AWS::S3::Bucket",
      "Properties": {
        "BucketName": { "Ref": "OtherBucket" }
      }
    }
  }
}
"#;

		let template = CfnTemplate::from_json(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];
		let bucket_name = bucket
			.properties
			.get("BucketName")
			.expect("BucketName property must exist");

		match bucket_name {
			CfnValue::Ref { target, .. } => {
				assert_eq!(target, "OtherBucket");
			}
			other => panic!("expected Ref CfnValue, got {:?}", other),
		}
	}

	#[test]
	fn json_intrinsic_getatt_forms_convert_to_getatt_value() {
		// Array form
		let text_array = r#"
{
  "Resources": {
    "MyBucket": {
      "Type": "AWS::S3::Bucket",
      "Properties": {
        "Arn": { "Fn::GetAtt": ["MyBucket", "Arn"] }
      }
    }
  }
}
"#;

		let template_array = CfnTemplate::from_json(text_array, &test_uri()).unwrap();
		let bucket_array = &template_array.resources["MyBucket"];
		let arn_array = bucket_array
			.properties
			.get("Arn")
			.expect("Arn property must exist");

		match arn_array {
			CfnValue::GetAtt {
				target, attribute, ..
			} => {
				assert_eq!(target, "MyBucket");
				assert_eq!(attribute, "Arn");
			}
			other => panic!("expected GetAtt CfnValue (array form), got {:?}", other),
		}

		// String form
		let text_string = r#"
{
  "Resources": {
    "MyBucket": {
      "Type": "AWS::S3::Bucket",
      "Properties": {
        "Arn": { "Fn::GetAtt": "MyBucket.Arn" }
      }
    }
  }
}
"#;

		let template_string = CfnTemplate::from_json(text_string, &test_uri()).unwrap();
		let bucket_string = &template_string.resources["MyBucket"];
		let arn_string = bucket_string
			.properties
			.get("Arn")
			.expect("Arn property must exist");

		match arn_string {
			CfnValue::GetAtt {
				target, attribute, ..
			} => {
				assert_eq!(target, "MyBucket");
				assert_eq!(attribute, "Arn");
			}
			other => panic!("expected GetAtt CfnValue (string form), got {:?}", other),
		}
	}

	#[test]
	fn yaml_short_form_ref_converts_to_ref_value() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Ref OtherBucket
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];
		let bucket_name = bucket
			.properties
			.get("BucketName")
			.expect("BucketName property must exist");

		match bucket_name {
			CfnValue::Ref { target, .. } => {
				assert_eq!(target, "OtherBucket");
			}
			other => panic!("expected Ref CfnValue, got {:?}", other),
		}
	}

	#[test]
	fn yaml_short_form_getatt_converts_to_getatt_value() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      Arn: !GetAtt MyBucket.Arn
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		let bucket = &template.resources["MyBucket"];
		let arn = bucket
			.properties
			.get("Arn")
			.expect("Arn property must exist");

		match arn {
			CfnValue::GetAtt {
				target, attribute, ..
			} => {
				assert_eq!(target, "MyBucket");
				assert_eq!(attribute, "Arn");
			}
			other => panic!("expected GetAtt CfnValue, got {:?}", other),
		}
	}

	#[test]
	fn yaml_short_form_intrinsics_complete_check() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Ref OtherBucket
      Arn: !GetAtt MyBucket.Arn
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();
		let output = template.to_string();

		let expected = r#"Template:
  Resources:
    MyBucket:
      Type: AWS::S3::Bucket
      Properties:
        Arn: !GetAtt MyBucket.Arn
        BucketName: !Ref OtherBucket
"#;

		assert_eq!(output, expected);
	}

	#[test]
	fn yaml_parses_parameters_section() {
		let text = r#"
Parameters:
  Environment:
    Type: String
    Default: dev
    Description: Deployment environment
  InstanceCount:
    Type: Number
    Default: 2

Resources:
  MyBucket:
    Type: AWS::S3::Bucket
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();

		assert_eq!(template.parameters.len(), 2);
		assert!(template.parameters.contains_key("Environment"));
		assert!(template.parameters.contains_key("InstanceCount"));

		let env_param = &template.parameters["Environment"];
		assert_eq!(env_param.parameter_type, "String");
		assert_eq!(
			env_param.description,
			Some("Deployment environment".to_string())
		);
		assert!(env_param.default_value.is_some());

		let count_param = &template.parameters["InstanceCount"];
		assert_eq!(count_param.parameter_type, "Number");
	}

	#[test]
	fn json_parses_parameters_section() {
		let text = r#"{
  "Parameters": {
    "Environment": {
      "Type": "String",
      "Default": "dev",
      "Description": "Deployment environment"
    },
    "InstanceCount": {
      "Type": "Number",
      "Default": 2
    }
  },
  "Resources": {
    "MyBucket": {
      "Type": "AWS::S3::Bucket"
    }
  }
}"#;

		let template = CfnTemplate::from_json(text, &test_uri()).unwrap();

		assert_eq!(template.parameters.len(), 2);
		assert!(template.parameters.contains_key("Environment"));
		assert!(template.parameters.contains_key("InstanceCount"));

		let env_param = &template.parameters["Environment"];
		assert_eq!(env_param.parameter_type, "String");
		assert_eq!(
			env_param.description,
			Some("Deployment environment".to_string())
		);
	}

	#[test]
	fn test_invalid_ref_target() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Ref NonExistentResource
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let spec_store = create_test_spec(); // Changed from new_empty()
		let diagnostics = template.validate_against_spec(&spec_store, &uri);

		assert!(diagnostics.iter().any(|d| {
			d.code == Some(NumberOrString::String("WA2_CFN_INVALID_REF".into()))
				&& d.message.contains("NonExistentResource")
		}));
	}

	#[test]
	fn test_valid_ref_to_resource() {
		let text = r#"
Resources:
  SourceBucket:
    Type: AWS::S3::Bucket
  DestBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Ref SourceBucket
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let spec_store = create_test_spec(); // Changed
		let diagnostics = template.validate_against_spec(&spec_store, &uri);

		assert!(
			!diagnostics
				.iter()
				.any(|d| { d.code == Some(NumberOrString::String("WA2_CFN_INVALID_REF".into())) })
		);
	}

	#[test]
	fn test_valid_ref_to_parameter() {
		let text = r#"
Parameters:
  Environment:
    Type: String

Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Ref Environment
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let spec_store = create_test_spec(); // Changed
		let diagnostics = template.validate_against_spec(&spec_store, &uri);

		assert!(
			!diagnostics
				.iter()
				.any(|d| { d.code == Some(NumberOrString::String("WA2_CFN_INVALID_REF".into())) })
		);
	}

	#[test]
	fn test_valid_ref_to_pseudo_parameter() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Ref AWS::Region
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let spec_store = create_test_spec(); // Changed
		let diagnostics = template.validate_against_spec(&spec_store, &uri);

		assert!(
			!diagnostics
				.iter()
				.any(|d| { d.code == Some(NumberOrString::String("WA2_CFN_INVALID_REF".into())) })
		);
	}

	#[test]
	fn test_invalid_getatt_resource() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      Arn: !GetAtt NonExistentBucket.Arn
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let spec_store = create_test_spec(); // Changed
		let diagnostics = template.validate_against_spec(&spec_store, &uri);

		assert!(diagnostics.iter().any(|d| {
			d.code
				== Some(NumberOrString::String(
					"WA2_CFN_INVALID_GETATT_RESOURCE".into(),
				)) && d.message.contains("NonExistentBucket")
		}));
	}

	#[test]
	fn test_type_mismatch_ref_returns_string() {
		use crate::spec::spec_store::{
			CollectionKind, PrimitiveType, PropertyName, PropertyShape, ResourceTypeDescriptor,
			ResourceTypeId, ShapeKind, SpecStore, TypeInfo,
		};

		// Create spec with S3::Bucket and Lambda::Function
		let mut resource_types = std::collections::HashMap::new();

		// Add S3::Bucket (empty properties, just needs to exist)
		resource_types.insert(
			ResourceTypeId("AWS::S3::Bucket".into()),
			ResourceTypeDescriptor {
				type_id: ResourceTypeId("AWS::S3::Bucket".into()),
				properties: std::collections::HashMap::new(),
				attributes: std::collections::HashMap::new(),
				documentation_url: None,
			},
		);

		// Add Lambda::Function with Timeout property that expects Integer
		let mut lambda_props = std::collections::HashMap::new();
		lambda_props.insert(
			PropertyName("Timeout".into()),
			PropertyShape {
				name: PropertyName("Timeout".into()),
				type_info: TypeInfo {
					kind: ShapeKind::Primitive(PrimitiveType::Integer),
					collection: CollectionKind::Scalar,
				},
				required: false,
				documentation_url: None,
				update_behavior: None,
				duplicates_allowed: false,
			},
		);

		resource_types.insert(
			ResourceTypeId("AWS::Lambda::Function".into()),
			ResourceTypeDescriptor {
				type_id: ResourceTypeId("AWS::Lambda::Function".into()),
				properties: lambda_props,
				attributes: std::collections::HashMap::new(),
				documentation_url: None,
			},
		);

		let spec = SpecStore {
			resource_types,
			property_types: std::collections::HashMap::new(),
		};

		// Ref to a resource returns String, but Timeout needs Integer
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
  MyFunction:
    Type: AWS::Lambda::Function
    Properties:
      Timeout: !Ref MyBucket
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let diagnostics = template.validate_against_spec(&spec, &uri);

		assert!(
			diagnostics.iter().any(|d| {
				d.code == Some(NumberOrString::String("WA2_CFN_TYPE_MISMATCH".into()))
					&& d.message.contains("expected Integer")
					&& d.message.contains("got String")
			}),
			"Expected type mismatch diagnostic, got: {:?}",
			diagnostics
		);
	}

	#[test]
	fn test_type_match_string_property() {
		let text = r#"
Parameters:
  BucketName:
    Type: String

Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Ref BucketName
      BucketEncryption: enabled
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let spec = create_test_spec();
		let diagnostics = template.validate_against_spec(&spec, &uri);

		// Should not have type mismatch - String parameter to String property is valid
		assert!(
			!diagnostics.iter().any(|d| {
				d.code == Some(NumberOrString::String("WA2_CFN_TYPE_MISMATCH".into()))
			}),
			"Should not have type mismatch, got: {:?}",
			diagnostics
		);
	}

	use crate::spec::spec_store::{AttributeName, AttributeShape};
	#[test]
	fn test_invalid_getatt_attribute() {
		// Create spec with S3::Bucket that has only "Arn" attribute
		let mut resource_types = std::collections::HashMap::new();
		let mut s3_attributes = std::collections::HashMap::new();

		s3_attributes.insert(
			AttributeName("Arn".into()),
			AttributeShape {
				name: AttributeName("Arn".into()),
				type_info: Some(TypeInfo {
					kind: ShapeKind::Primitive(PrimitiveType::String),
					collection: CollectionKind::Scalar,
				}),
				documentation_url: None,
			},
		);

		resource_types.insert(
			ResourceTypeId("AWS::S3::Bucket".into()),
			ResourceTypeDescriptor {
				type_id: ResourceTypeId("AWS::S3::Bucket".into()),
				properties: std::collections::HashMap::new(),
				attributes: s3_attributes,
				documentation_url: None,
			},
		);

		let spec = SpecStore {
			resource_types,
			property_types: std::collections::HashMap::new(),
		};

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
  AnotherBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !GetAtt MyBucket.InvalidAttribute
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let diagnostics = template.validate_against_spec(&spec, &uri);

		assert!(
			diagnostics.iter().any(|d| {
				d.code
					== Some(NumberOrString::String(
						"WA2_CFN_INVALID_GETATT_ATTRIBUTE".into(),
					)) && d.message.contains("InvalidAttribute")
			}),
			"Expected invalid attribute diagnostic, got: {:?}",
			diagnostics
		);
	}

	#[test]
	fn test_valid_getatt_attribute() {
		// Create spec with S3::Bucket that has "Arn" attribute
		let mut resource_types = std::collections::HashMap::new();
		let mut s3_attributes = std::collections::HashMap::new();

		s3_attributes.insert(
			AttributeName("Arn".into()),
			AttributeShape {
				name: AttributeName("Arn".into()),
				type_info: Some(TypeInfo {
					kind: ShapeKind::Primitive(PrimitiveType::String),
					collection: CollectionKind::Scalar,
				}),
				documentation_url: None,
			},
		);

		resource_types.insert(
			ResourceTypeId("AWS::S3::Bucket".into()),
			ResourceTypeDescriptor {
				type_id: ResourceTypeId("AWS::S3::Bucket".into()),
				properties: std::collections::HashMap::new(),
				attributes: s3_attributes,
				documentation_url: None,
			},
		);

		let spec = SpecStore {
			resource_types,
			property_types: std::collections::HashMap::new(),
		};

		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
  AnotherBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !GetAtt MyBucket.Arn
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let diagnostics = template.validate_against_spec(&spec, &uri);

		// Should not have invalid attribute error
		assert!(
			!diagnostics.iter().any(|d| {
				d.code
					== Some(NumberOrString::String(
						"WA2_CFN_INVALID_GETATT_ATTRIBUTE".into(),
					))
			}),
			"Should not have invalid attribute error, got: {:?}",
			diagnostics
		);
	}

	#[test]
	fn yaml_sub_string_form() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Sub "my-bucket-${AWS::Region}"
"#;

		let template = CfnTemplate::from_yaml(text, &test_uri()).unwrap();
		let bucket = &template.resources["MyBucket"];
		let bucket_name = bucket.properties.get("BucketName").unwrap();

		match bucket_name {
			CfnValue::Sub {
				template,
				variables,
				..
			} => {
				assert_eq!(template, "my-bucket-${AWS::Region}");
				assert!(variables.is_none());
			}
			other => panic!("expected Sub CfnValue, got {:?}", other),
		}
	}

	#[test]
	fn test_invalid_sub_variable() {
		let text = r#"
Resources:
  MyBucket:
    Type: AWS::S3::Bucket
    Properties:
      BucketName: !Sub "my-bucket-${NonExistent}"
"#;

		let uri = test_uri();
		let template = CfnTemplate::from_yaml(text, &uri).unwrap();
		let spec_store = create_test_spec();
		let diagnostics = template.validate_against_spec(&spec_store, &uri);

		assert!(diagnostics.iter().any(|d| {
			d.code
				== Some(NumberOrString::String(
					"WA2_CFN_INVALID_SUB_VARIABLE".into(),
				)) && d.message.contains("NonExistent")
		}));
	}
}
