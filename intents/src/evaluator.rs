use crate::{
	node::{Annotation, NodeKind, TaggedValue, VendorReference},
	system::{FocusTaxonomy, NodeError, PrettySystem, System},
};
use wa2lsp::spec::cfn_ir::types::{CfnTemplate, CfnValue};

use std::str::FromStr;

pub fn evaluate_system_ok(template: &CfnTemplate) -> Result<(), NodeError> {
	// PROJECT: vendor template into wa2 system
	let system = project_vendor_aws(template)?;

	println!("{}", PrettySystem { system: &system });

	// GUIDANCE: is guidance requuired?
	let guidance = system.guidance();

	println!("{:?}", guidance);

	// For now: treat any required guidance as failing the check.
	if !guidance.is_empty() {
		return Err(NodeError::GuidanceRequired);
	}

	Ok(())
}

/// takes a Cfn template and create a system based on it
fn project_vendor_aws(template: &CfnTemplate) -> Result<System, NodeError> {
	let mut system = System::default();

	for resource in template.resources.values() {
		// MAP: resources into primitives (s3/bucket -> store)
		let kind = match resource.resource_type.parse::<NodeKind>() {
			Ok(k) => k,
			Err(_) => continue, // unknown resource type -> ignore
		};

		let name = resource.logical_id.clone();
		let node = system.add_node(&name, kind);

		system.annotate(
			node,
			Annotation::Vendor(VendorReference {
				name: resource.resource_type.clone(),
			}),
		)?;

		// TAGS: extract any evidence held in tags
		// TODO: map tags into taxonomy via configuration not hard coded!
		if let Some((tag_val, _tag_range)) = resource.properties.get("Tags") {
			let tags = tag_val.as_array().expect("valid object");
			for tag in tags {
				let tag = tag.as_object().expect("valid object");
				let key = tag["Key"].0.as_str().expect("key is a string");
				let value = tag["Value"].0.as_str().expect("value is a string");

				// does it have a data tagging?
				match key {
					"DataSensitivity" => {
						system.annotate(
							node,
							Annotation::Tagged(TaggedValue {
								tag: FocusTaxonomy::DataSensitivity,
								value: Some(value.to_owned()),
							}),
						)?;
					}
					"DataCriticality" => {
						system.annotate(
							node,
							Annotation::Tagged(TaggedValue {
								tag: FocusTaxonomy::DataCriticality,
								value: Some(value.to_owned()),
							}),
						)?;
					}
					_ => {}
				}
			}
		}

		// Evidence: what resiliance is in place?
		// TODO: map methods resiliance
		if let Some((repl_val, _repl_range)) = resource.properties.get("ReplicationConfiguration") {
			let repl_obj = repl_val.as_object().expect("valid object");
			let field = &repl_obj["Status"].0;
			if let CfnValue::String(value, _) = field
				&& value == "Enabled"
			{
				system.annotate(
					node,
					Annotation::Evidence(FocusTaxonomy::DataResiliance),
				)?;
			}
		}
	}

	Ok(system)
}

impl FromStr for NodeKind {
	type Err = ();

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		Ok(match s {
			"AWS::EC2::Instance" | "AWS::Lambda::Function" => NodeKind::Run,
			"AWS::S3::Bucket" | "AWS::EC2::Volume" | "AWS::EFS::FileSystem" => NodeKind::Store,
			"AWS::SQS::Queue" | "AWS::Kinesis::Stream" => NodeKind::Move,
			_ => return Err(()),
		})
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::test_support::use_latest_cfn_spec;
	use std::{env, path::Path};
	use url::Url;

	#[allow(clippy::large_enum_variant)]
	#[allow(dead_code)]
	enum ParseResult {
		Error,
		Parsed { template: CfnTemplate },
	}

	fn parse_and_validate(path: &Path) -> Result<ParseResult, Box<dyn std::error::Error>> {
		let content = std::fs::read_to_string(path)?;
		let abs_path = path.canonicalize()?;
		let uri = Url::from_file_path(&abs_path)
			.map_err(|_| format!("Invalid path: {}", path.display()))?;

		match CfnTemplate::from_yaml(&content, &uri) {
			Err(parse_diags) => Ok(ParseResult::Error),
			Ok(template) => {
				let spec = use_latest_cfn_spec();
				let diags = template.validate_against_spec(&spec, &uri);
				Ok(ParseResult::Parsed { template })
			}
		}
	}

	#[test]
	fn tutorial_step_0() {
		println!("{}", env::current_dir().unwrap().display());
		let path = Path::new("../examples/tutorial/0.data.yaml");
		let parse_result = parse_and_validate(path).expect("Failed to read/parse file");

		if let ParseResult::Parsed { template } = parse_result {
			evaluate_system_ok(&template).expect("system should evaluate ok");
		} else {
			panic!("Expected parsed template");
		}
	}
}
