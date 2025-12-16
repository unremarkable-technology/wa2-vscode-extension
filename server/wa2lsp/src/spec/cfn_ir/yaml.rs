use std::collections::HashMap;

use saphyr::{LoadableYamlNode, MarkedYaml, Scalar, ScanError, YamlData};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};
use url::Url;

use crate::spec::cfn_ir::types::{CfnCondition, CfnParameter, CfnResource, CfnTemplate, CfnValue};
use crate::spec::intrinsics::{self, IntrinsicKind};

impl CfnTemplate {
	/// Parse a CloudFormation template from YAML text
	pub fn from_yaml(text: &str, uri: &Url) -> Result<Self, Vec<Diagnostic>> {
		let docs = MarkedYaml::load_from_str(text)
			.map_err(|err| vec![yaml_error_to_diagnostic(err, uri)])?;

		if docs.is_empty() {
			return Ok(CfnTemplate {
				resources: HashMap::new(),
				parameters: HashMap::new(),
				conditions: HashMap::new(),
			});
		}

		Self::from_marked_yaml(&docs[0], uri)
	}

	fn from_marked_yaml(root: &MarkedYaml, uri: &Url) -> Result<Self, Vec<Diagnostic>> {
		let mut resources = HashMap::new();
		let mut parameters = HashMap::new();
		let mut conditions = HashMap::new();

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

		// Parse Conditions section
		let conditions_node = root_map
			.iter()
			.find(|(k, _)| k.data.as_str() == Some("Conditions"))
			.map(|(_, v)| v);

		if let Some(conditions_node) = conditions_node {
			let conditions_map = match conditions_node.data.as_mapping() {
				Some(m) => m,
				None => {
					return Err(vec![Diagnostic {
						range: marked_yaml_to_range(conditions_node),
						severity: Some(DiagnosticSeverity::ERROR),
						code: Some(NumberOrString::String("WA2_CFN_INVALID_CONDITIONS".into())),
						source: Some("wa2-lsp".into()),
						message: "Template Conditions section must be a mapping".to_string(),
						..Default::default()
					}]);
				}
			};

			let mut errors = Vec::new();
			for (condition_key, condition_node) in conditions_map {
				let condition_name = match condition_key.data.as_str() {
					Some(s) => s.to_string(),
					None => {
						errors.push(Diagnostic {
							range: marked_yaml_to_range(condition_key),
							severity: Some(DiagnosticSeverity::WARNING),
							code: Some(NumberOrString::String(
								"WA2_CFN_CONDITION_KEY_NOT_STRING".into(),
							)),
							source: Some("wa2-lsp".into()),
							message: "Template: condition key is not a string".to_string(),
							..Default::default()
						});
						continue;
					}
				};

				match CfnCondition::from_marked_yaml(
					condition_name.clone(),
					condition_node,
					marked_yaml_to_range(condition_key),
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
					parameters,
					conditions,
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
			conditions,
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

impl CfnCondition {
	fn from_marked_yaml(
		name: String,
		node: &MarkedYaml,
		name_range: Range,
		_uri: &Url,
	) -> Result<Self, Vec<Diagnostic>> {
		// A condition is just a value expression (usually an intrinsic function)
		let expression = CfnValue::from_marked_yaml(node)?;

		Ok(CfnCondition {
			name,
			expression,
			name_range,
		})
	}
}

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
						IntrinsicKind::GetAZs => {
							// GetAZs accepts either empty string "" or a region reference
							let region_value = CfnValue::from_marked_yaml(inner)?;
							return Ok(CfnValue::GetAZs {
								region: Box::new(region_value),
								range,
							});
						}
						IntrinsicKind::Join => {
							// Join must be array form: !Join [delimiter, [values]]
							if let YamlData::Sequence(seq) = &inner.data
								&& seq.len() == 2 && let YamlData::Value(Scalar::String(delimiter)) =
								&seq[0].data
							{
								// Second element should be an array of values
								let values_node = &seq[1];
								let values_cfn = CfnValue::from_marked_yaml(values_node)?;

								let values = match values_cfn {
									CfnValue::Array(items, _) => items,
									_ => {
										return Err(vec![Diagnostic {
											range: marked_yaml_to_range(values_node),
											severity: Some(DiagnosticSeverity::ERROR),
											code: Some(NumberOrString::String("WA2_CFN_MALFORMED_JOIN".into())),
											source: Some("wa2-lsp".into()),
											message: "Malformed !Join: second argument must be an array of values".to_string(),
											..Default::default()
										}]);
									}
								};

								return Ok(CfnValue::Join {
									delimiter: delimiter.to_string(),
									values,
									range,
								});
							}

							return Err(vec![Diagnostic {
								range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String("WA2_CFN_MALFORMED_JOIN".into())),
								source: Some("wa2-lsp".into()),
								message: "Malformed !Join: expected array [delimiter, [values]]"
									.to_string(),
								..Default::default()
							}]);
						}
						IntrinsicKind::Select => {
							// Select must be array form: !Select [index, list]
							if let YamlData::Sequence(seq) = &inner.data
								&& seq.len() == 2
							{
								let index_value = CfnValue::from_marked_yaml(&seq[0])?;
								let list_value = CfnValue::from_marked_yaml(&seq[1])?;

								return Ok(CfnValue::Select {
									index: Box::new(index_value),
									list: Box::new(list_value),
									range,
								});
							}

							return Err(vec![Diagnostic {
								range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String(
									"WA2_CFN_MALFORMED_SELECT".into(),
								)),
								source: Some("wa2-lsp".into()),
								message: "Malformed !Select: expected array [index, list]"
									.to_string(),
								..Default::default()
							}]);
						}
						IntrinsicKind::If => {
							// If must be array form: !If [condition_name, value_if_true, value_if_false]
							if let YamlData::Sequence(seq) = &inner.data
								&& seq.len() == 3 && let YamlData::Value(Scalar::String(
								condition_name,
							)) = &seq[0].data
							{
								let value_if_true = CfnValue::from_marked_yaml(&seq[1])?;
								let value_if_false = CfnValue::from_marked_yaml(&seq[2])?;

								return Ok(CfnValue::If {
									condition_name: condition_name.to_string(),
									value_if_true: Box::new(value_if_true),
									value_if_false: Box::new(value_if_false),
									range,
								});
							}

							return Err(vec![Diagnostic {
								range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String("WA2_CFN_MALFORMED_IF".into())),
								source: Some("wa2-lsp".into()),
								message: "Malformed !If: expected array [condition_name, value_if_true, value_if_false]".to_string(),
								..Default::default()
							}]);
						}
						IntrinsicKind::Equals => {
							// Equals must be array form: !Equals [value1, value2]
							if let YamlData::Sequence(seq) = &inner.data
								&& seq.len() == 2
							{
								let left = CfnValue::from_marked_yaml(&seq[0])?;
								let right = CfnValue::from_marked_yaml(&seq[1])?;

								return Ok(CfnValue::Equals {
									left: Box::new(left),
									right: Box::new(right),
									range,
								});
							}

							return Err(vec![Diagnostic {
								range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String(
									"WA2_CFN_MALFORMED_EQUALS".into(),
								)),
								source: Some("wa2-lsp".into()),
								message: "Malformed !Equals: expected array [value1, value2]"
									.to_string(),
								..Default::default()
							}]);
						}
						IntrinsicKind::Not => {
							// Not must be array form with single element: !Not [condition]
							if let YamlData::Sequence(seq) = &inner.data
								&& seq.len() == 1
							{
								let condition = CfnValue::from_marked_yaml(&seq[0])?;

								return Ok(CfnValue::Not {
									condition: Box::new(condition),
									range,
								});
							}

							return Err(vec![Diagnostic {
								range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String("WA2_CFN_MALFORMED_NOT".into())),
								source: Some("wa2-lsp".into()),
								message: "Malformed !Not: expected array with single condition [condition]".to_string(),
								..Default::default()
							}]);
						}
						IntrinsicKind::And => {
							// And must be array form with 2-10 conditions: !And [cond1, cond2, ...]
							if let YamlData::Sequence(seq) = &inner.data {
								if seq.len() < 2 || seq.len() > 10 {
									return Err(vec![Diagnostic {
										range,
										severity: Some(DiagnosticSeverity::ERROR),
										code: Some(NumberOrString::String(
											"WA2_CFN_MALFORMED_AND".into(),
										)),
										source: Some("wa2-lsp".into()),
										message: format!(
											"Malformed !And: expected 2-10 conditions, got {}",
											seq.len()
										),
										..Default::default()
									}]);
								}

								let conditions: Result<Vec<_>, _> =
									seq.iter().map(CfnValue::from_marked_yaml).collect();

								return Ok(CfnValue::And {
									conditions: conditions?,
									range,
								});
							}

							return Err(vec![Diagnostic {
								range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String("WA2_CFN_MALFORMED_AND".into())),
								source: Some("wa2-lsp".into()),
								message: "Malformed !And: expected array of conditions".to_string(),
								..Default::default()
							}]);
						}
						IntrinsicKind::Or => {
							// Or must be array form with 2-10 conditions: !Or [cond1, cond2, ...]
							if let YamlData::Sequence(seq) = &inner.data {
								if seq.len() < 2 || seq.len() > 10 {
									return Err(vec![Diagnostic {
										range,
										severity: Some(DiagnosticSeverity::ERROR),
										code: Some(NumberOrString::String(
											"WA2_CFN_MALFORMED_OR".into(),
										)),
										source: Some("wa2-lsp".into()),
										message: format!(
											"Malformed !Or: expected 2-10 conditions, got {}",
											seq.len()
										),
										..Default::default()
									}]);
								}

								let conditions: Result<Vec<_>, _> =
									seq.iter().map(CfnValue::from_marked_yaml).collect();

								return Ok(CfnValue::Or {
									conditions: conditions?,
									range,
								});
							}

							return Err(vec![Diagnostic {
								range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String("WA2_CFN_MALFORMED_OR".into())),
								source: Some("wa2-lsp".into()),
								message: "Malformed !Or: expected array of conditions".to_string(),
								..Default::default()
							}]);
						}
						IntrinsicKind::Condition => {
							// Condition takes a string: !Condition ConditionName
							if let YamlData::Value(Scalar::String(condition_name)) = &inner.data {
								return Ok(CfnValue::Condition {
									condition_name: condition_name.to_string(),
									range,
								});
							}

							return Err(vec![Diagnostic {
								range,
								severity: Some(DiagnosticSeverity::ERROR),
								code: Some(NumberOrString::String(
									"WA2_CFN_MALFORMED_CONDITION".into(),
								)),
								source: Some("wa2-lsp".into()),
								message: "Malformed !Condition: expected condition name string"
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
