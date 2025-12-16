use std::collections::HashMap;

use jsonc_parser::ParseOptions;
use jsonc_parser::ast::Value;
use jsonc_parser::common::Ranged;
use jsonc_parser::errors::ParseError;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};
use url::Url;

use crate::spec::cfn_ir::types::{CfnCondition, CfnParameter, CfnResource, CfnTemplate, CfnValue};
use crate::spec::intrinsics::{self, IntrinsicKind};

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

		let parse_result = jsonc_parser::parse_to_ast(text, &Default::default(), &parse_options)
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
		let mut conditions = HashMap::new();

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

		// Parse Conditions section
		if let Some(conditions_prop) = root_obj.get("Conditions") {
			let conditions_obj = match conditions_prop.value.as_object() {
				Some(obj) => obj,
				None => {
					return Err(vec![Diagnostic {
						range: ranged_to_range(&conditions_prop.value, text),
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String("WA2_CFN_INVALID_CONDITIONS".into())),
						source: Some("wa2-lsp".into()),
						message: "Template Conditions section must be an object".to_string(),
						..Default::default()
					}]);
				}
			};

			let mut errors = Vec::new();
			for prop in conditions_obj.properties.iter() {
				let condition_name = prop.name.clone().into_string();
				let name_range = ranged_to_range(&prop.name, text);
				let condition_value = prop.value.clone();

				match CfnCondition::from_json_ast(
					condition_name.clone(),
					&condition_value,
					name_range,
					text,
					uri,
				) {
					Ok(condition) => {
						conditions.insert(condition_name, condition);
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
					conditions: HashMap::new(),
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
			conditions,
		})
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

impl CfnCondition {
	fn from_json_ast(
		name: String,
		node: &Value,
		name_range: Range,
		text: &str,
		_uri: &Url,
	) -> Result<Self, Vec<Diagnostic>> {
		// A condition is just a value expression
		let expression = CfnValue::from_json_ast(node, text)?;

		Ok(CfnCondition {
			name,
			expression,
			name_range,
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
						IntrinsicKind::GetAZs => {
							let region_value = CfnValue::from_json_ast(value, text)?;
							return Ok(CfnValue::GetAZs {
								region: Box::new(region_value),
								range: inner_range,
							});
						}
						IntrinsicKind::Join => {
							// Join: {"Fn::Join": [delimiter, [values]]}
							if let Some(arr) = value.as_array()
								&& arr.elements.len() == 2
								&& let Some(delimiter) = arr.elements[0].as_string_lit()
							{
								let values_node = &arr.elements[1];
								let values_cfn = CfnValue::from_json_ast(values_node, text)?;

								let values = match values_cfn {
									CfnValue::Array(items, _) => items,
									_ => {
										return Err(vec![Diagnostic {
                    range: json_ast_to_range(values_node, text),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("WA2_CFN_MALFORMED_JOIN".into())),
                    source: Some("wa2-lsp".into()),
                    message: "Malformed Fn::Join: second argument must be an array of values".to_string(),
                    ..Default::default()
                }]);
									}
								};

								return Ok(CfnValue::Join {
									delimiter: delimiter.value.to_string(),
									values,
									range: inner_range,
								});
							}
						}
						IntrinsicKind::Select => {
							// Select: {"Fn::Select": [index, list]}
							if let Some(arr) = value.as_array()
								&& arr.elements.len() == 2
							{
								let index_value = CfnValue::from_json_ast(&arr.elements[0], text)?;
								let list_value = CfnValue::from_json_ast(&arr.elements[1], text)?;

								return Ok(CfnValue::Select {
									index: Box::new(index_value),
									list: Box::new(list_value),
									range: inner_range,
								});
							}
						}
						IntrinsicKind::If => {
							// If: {"Fn::If": [condition_name, value_if_true, value_if_false]}
							if let Some(arr) = value.as_array()
								&& arr.elements.len() == 3
								&& let Some(condition_name) = arr.elements[0].as_string_lit()
							{
								let value_if_true =
									CfnValue::from_json_ast(&arr.elements[1], text)?;
								let value_if_false =
									CfnValue::from_json_ast(&arr.elements[2], text)?;

								return Ok(CfnValue::If {
									condition_name: condition_name.value.to_string(),
									value_if_true: Box::new(value_if_true),
									value_if_false: Box::new(value_if_false),
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

/// Convert anything with a range to an LSP Range
fn ranged_to_range<T: Ranged>(node: &T, text: &str) -> Range {
	let json_range = node.range();

	Range {
		start: byte_offset_to_position(text, json_range.start),
		end: byte_offset_to_position(text, json_range.end),
	}
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

