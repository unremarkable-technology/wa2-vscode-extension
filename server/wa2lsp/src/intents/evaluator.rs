use super::model::{Cmp, EntityId, Model, ModelError, Query, Value};

use crate::{
	intents::model::Axis,
	spec::cfn_ir::types::{CfnTemplate, CfnValue},
};

pub fn project_vendor_aws(template: &CfnTemplate) -> Result<Model, ModelError> {
	let mut model = Model::bootstrap();
	model.ensure_entity("aws");

	let root = model.ensure_entity("deployment");
	model.apply_to(root, "wa2:type", "wa2:Deployment")?;
	model.set_root(root);

	let mut entities = Vec::new();

	for resource in template.resources.values() {
		let entity = model.ensure_entity(&resource.logical_id);
		model.apply_to(
			entity,
			"aws:type",
			&format!("\"{}\"", resource.resource_type),
		)?;
		model.apply_to(
			entity,
			"aws:logicalId",
			&format!("\"{}\"", resource.logical_id),
		)?;
		model.apply_entity(root, "wa2:contains", entity)?;

		for (prop_name, (prop_value, _)) in &resource.properties {
			project_value(&mut model, entity, prop_name, prop_value)?;
		}

		entities.push(entity);
	}

	// Derive phase - queries the model
	for entity in entities {
		derive_wa2_type(&mut model, entity)?;
		derive_evidence(&mut model, entity)?;
	}

	Ok(model)
}

fn project_value(
	model: &mut Model,
	parent: EntityId,
	property_name: &str,
	value: &CfnValue,
) -> Result<(), ModelError> {
	let pred = format!("aws:{}", property_name);

	match value {
		CfnValue::String(s, _) => {
			model.apply_to(parent, &pred, &format!("\"{}\"", s))?;
		}
		CfnValue::Number(n, _) => {
			model.apply_to(parent, &pred, &format!("\"{}\"", n))?;
		}
		CfnValue::Bool(b, _) => {
			model.apply_to(parent, &pred, &format!("\"{}\"", b))?;
		}
		CfnValue::Null(_) => {
			// Skip nulls
		}
		CfnValue::Array(items, _) => {
			let container = model.blank();
			model.apply_entity(parent, &pred, container)?;
			for item in items {
				project_array_item(model, container, item)?;
			}
		}
		CfnValue::Object(map, _) => {
			let node = model.blank();
			model.apply_entity(parent, &pred, node)?;
			for (key, (val, _)) in map {
				project_value(model, node, key, val)?;
			}
		}
		// Intrinsic functions - store reference info where useful
		CfnValue::Ref { target, .. } => {
			model.apply_to(parent, &pred, &format!("\"!Ref {}\"", target))?;
		}
		CfnValue::GetAtt {
			target, attribute, ..
		} => {
			model.apply_to(
				parent,
				&pred,
				&format!("\"!GetAtt {}.{}\"", target, attribute),
			)?;
		}
		_ => {
			// Other intrinsics - skip or serialize as needed
		}
	}

	Ok(())
}

fn project_array_item(
	model: &mut Model,
	container: EntityId,
	value: &CfnValue,
) -> Result<(), ModelError> {
	match value {
		CfnValue::Object(map, _) => {
			let item = model.blank();
			model.apply_entity(container, "wa2:contains", item)?;
			for (key, (val, _)) in map {
				project_value(model, item, key, val)?;
			}
		}
		CfnValue::String(s, _) => {
			// Primitive array items - store as literal children
			let item = model.blank();
			model.apply_entity(container, "wa2:contains", item)?;
			model.apply_to(item, "aws:value", &format!("\"{}\"", s))?;
		}
		CfnValue::Array(items, _) => {
			// Nested array
			let nested = model.blank();
			model.apply_entity(container, "wa2:contains", nested)?;
			for sub_item in items {
				project_array_item(model, nested, sub_item)?;
			}
		}
		_ => {
			// Other types - skip for now
		}
	}
	Ok(())
}

fn derive_wa2_type(model: &mut Model, entity: EntityId) -> Result<(), ModelError> {
	let Some(aws_type) = model.get_literal(entity, "aws:type") else {
		return Ok(());
	};

	let wa2_kind = match aws_type.as_str() {
		"AWS::EC2::Instance" | "AWS::Lambda::Function" => Some("wa2:Run"),
		"AWS::S3::Bucket" | "AWS::EC2::Volume" | "AWS::EFS::FileSystem" => Some("wa2:Store"),
		"AWS::SQS::Queue" | "AWS::Kinesis::Stream" => Some("wa2:Move"),
		_ => None,
	};

	if let Some(kind) = wa2_kind {
		model.apply_to(entity, "wa2:type", kind)?;
	}

	Ok(())
}

fn derive_evidence(model: &mut Model, entity: EntityId) -> Result<(), ModelError> {
	let query = Query::follow("aws:ReplicationConfiguration")
		.then_follow("aws:Rules")
		.then(Axis::Child, None)
		.filter("aws:Status", Cmp::Eq, Value::Literal("Enabled".to_string()));

	if !model.query_from(&[entity], &query).is_empty() {
		let evidence = model.blank();
		model.apply_to(evidence, "wa2:type", "wa2:Evidence")?;
		model.apply_to(evidence, "wa2:value", "\"DataResiliance\"")?;
		model.apply_entity(entity, "wa2:contains", evidence)?;
	}

	Ok(())
}
// ─── Guidance Types ───

#[derive(Debug)]
pub struct Guide {
	pub entity: EntityId,
	pub logical_id: String,
	pub level: GuideLevel,
	pub focus: FocusTaxonomy,
	pub tldr: String,
	pub message: String,
	pub why: String,
}

#[derive(Debug)]
pub enum GuideLevel {
	Required,
	Action,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FocusTaxonomy {
	DataSensitivity,
	DataCriticality,
	DataResiliance,
}

// ─── Query Helpers ───

/// Check if a resource has a tag with the given key
fn has_tag(model: &Model, resource: EntityId, tag_key: &str) -> bool {
	get_tag_value(model, resource, tag_key).is_some()
}

/// Get the value of a tag on a resource
fn get_tag_value(model: &Model, resource: EntityId, tag_key: &str) -> Option<String> {
	let query = Query::follow("aws:Tags").then(Axis::Child, None).filter(
		"aws:Key",
		Cmp::Eq,
		Value::Literal(tag_key.to_string()),
	);

	let matches = model.query_from(&[resource], &query);

	matches
		.first()
		.and_then(|&tag| model.get_literal(tag, "aws:Value"))
}

/// Check if a resource has evidence of a given type
fn has_evidence(model: &Model, resource: EntityId, evidence_name: &str) -> bool {
	let query = Query::descendant("wa2:Evidence").filter(
		"wa2:value",
		Cmp::Eq,
		Value::Literal(evidence_name.to_string()),
	);

	!model.query_from(&[resource], &query).is_empty()
}

// ─── Guidance ───

/// Compute guidance for a model
pub fn guidance(model: &Model) -> Vec<Guide> {
	let mut guides = Vec::new();

	// Find all stores
	let stores = model.query(&Query::descendant("wa2:Store"));

	for id in stores {
		let logical_id = model
			.get_literal(id, "aws:logicalId")
			.unwrap_or_else(|| model.qualified_name(id));

		// Check for mandatory tags
		if !has_tag(model, id, "DataSensitivity") {
			guides.push(Guide {
				entity: id,
				logical_id: logical_id.clone(),
				tldr: "Tag this resource for DataSensitivity".to_string(),
				message: "All stores of information should have data sensitivity.".to_string(),
				why: "Tagging store with sensitivity speeds up design decisions, \
                    we can apply the same designs for all data of the same class. \
                    For example website assets don't need encryption, whilst \
                    a healthcare record needs encryption and access restricted."
					.to_string(),
				level: GuideLevel::Required,
				focus: FocusTaxonomy::DataSensitivity,
			});
		}

		if !has_tag(model, id, "DataCriticality") {
			guides.push(Guide {
				entity: id,
				logical_id: logical_id.clone(),
				tldr: "Tag this resource for DataCriticality".to_string(),
				message: "All stores of information should have a data criticality.".to_string(),
				why: "Tagging store with criticality speeds up design decisions, \
                    we can apply the same designs for all data of the same class. \
                    For example easily recreated data does not need backing up, whilst \
                    employment records need protection against loss."
					.to_string(),
				level: GuideLevel::Required,
				focus: FocusTaxonomy::DataCriticality,
			});
		}

		// Check critical data is backed up
		if let Some(criticality) = get_tag_value(model, id, "DataCriticality") {
			if (criticality == "MissionCritical" || criticality == "BusinessCritical")
				&& !has_evidence(model, id, "DataResiliance")
			{
				guides.push(Guide {
					entity: id,
					logical_id,
					tldr: "Backup this resource".to_string(),
					message: "All critical stores of information should be backed up.".to_string(),
					why: "This data's criticality indicates we don't want to lose it, \
                        so we need to apply a mechanism to ensure its backed up. \
                        Backup, Snapshots or Replication are common solutions."
						.to_string(),
					level: GuideLevel::Action,
					focus: FocusTaxonomy::DataResiliance,
				});
			}
		}
	}

	guides
}

// ─── Tests ───

#[cfg(test)]
mod tests {
	use url::Url;

	use super::*;
	use crate::intents::{model::print_model_as_tree, test_support::use_latest_cfn_spec};
	use std::{env, path::Path};

	/// Simplified resource for unit testing (mirrors CfnResource structure)
	#[derive(Debug, Clone)]
	struct TestResource {
		logical_id: String,
		resource_type: String,
		tags: Vec<(String, String)>,
		has_replication: bool,
	}

	impl TestResource {
		fn new(logical_id: &str, resource_type: &str) -> Self {
			Self {
				logical_id: logical_id.to_string(),
				resource_type: resource_type.to_string(),
				tags: Vec::new(),
				has_replication: false,
			}
		}

		fn with_tag(mut self, key: &str, value: &str) -> Self {
			self.tags.push((key.to_string(), value.to_string()));
			self
		}

		fn with_replication(mut self) -> Self {
			self.has_replication = true;
			self
		}
	}

	#[derive(Debug)]
	enum TemplateError {
		Parse(Vec<tower_lsp::lsp_types::Diagnostic>),
		Validation(Vec<tower_lsp::lsp_types::Diagnostic>),
	}

	impl std::fmt::Display for TemplateError {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			write!(f, "{self:?}")
		}
	}
	impl std::error::Error for TemplateError {}

	fn parse_text_and_validate(content: &str) -> Result<CfnTemplate, TemplateError> {
		let template =
			CfnTemplate::from_yaml(content, &test_uri()).map_err(TemplateError::Parse)?;

		let spec = use_latest_cfn_spec();
		let diags = template.validate_against_spec(&spec);

		if !diags.is_empty() {
			return Err(TemplateError::Validation(diags));
		}

		Ok(template)
	}

	fn test_uri() -> Url {
		Url::parse("file:///tmp/test.yaml").unwrap()
	}

	fn parse_and_validate(path: &Path) -> Result<CfnTemplate, TemplateError> {
		let content = std::fs::read_to_string(path).unwrap();
		let abs_path = path.canonicalize().unwrap();

		parse_text_and_validate(&content)
	}

	#[test]
	fn test_non_critical_data_no_backup_required() {
		// NonCritical data doesn't need backup
		let cfn_text = r#"
Resources:
  TempBucket:
    Type: AWS::S3::Bucket

    Properties:
      Tags:
        - Key: DataSensitivity
          Value: Confidential
        - Key: DataCriticality
          Value: NonCritical
"#;

		let template = parse_text_and_validate(cfn_text).expect("Failed to read/parse text");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);
		eprintln!("\nModel:\n===\n{}", print_model_as_tree(&model));

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);

		assert!(guides.is_empty());
	}

	#[test]
	fn test_mission_critical_needs_backup() {
		let cfn_text = r#"
Resources:
  CriticalBucket:
    Type: AWS::S3::Bucket
    Properties:
      Tags:
        - Key: DataSensitivity
          Value: Confidential
        - Key: DataCriticality
          Value: MissionCritical
"#;

		let template = parse_text_and_validate(cfn_text).expect("Failed to read/parse text");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);
		eprintln!("\nModel:\n===\n{}", print_model_as_tree(&model));

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);

		assert_eq!(guides.len(), 1);
		assert!(matches!(guides[0].focus, FocusTaxonomy::DataResiliance));
	}

	#[test]
	fn test_non_store_resources_no_guidance() {
		let cfn_text = r#"
Resources:
  MyLambda:
    Type: AWS::Lambda::Function
    Properties:
      Role: arn::iam::123456789012
      Runtime: python3.9
      Handler: index.handler
      Code:
        ZipFile: |
          def handler(event, context):
            return "hello"
  MyRole:
    Type: AWS::IAM::Role
    Properties:
      AssumeRolePolicyDocument:
        Version: "2012-10-17"
        Statement: []
  MyQueue:
    Type: AWS::SQS::Queue
"#;

		let template = parse_text_and_validate(cfn_text).expect("Failed to read/parse text");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);
		eprintln!("\nModel:\n===\n{}", print_model_as_tree(&model));

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);

		// Lambda is Run, Queue is Move - neither are Store
		// Only Store requires DataSensitivity/DataCriticality tags
		let stores = model.query(&Query::descendant("wa2:Store"));
		assert!(stores.is_empty());
		assert!(guides.is_empty());
	}

	#[test]
	fn test_query_finds_stores() {
		let cfn_text = r#"
Resources:
  Bucket1:
    Type: AWS::S3::Bucket
  Bucket2:
    Type: AWS::S3::Bucket
  Lambda1:
    Type: AWS::Lambda::Function
    Properties:
      Role: arn::iam::123456789012
      Runtime: python3.9
      Handler: index.handler
      Code:
        ZipFile: "def handler(e,c): pass"
"#;

		let template = parse_text_and_validate(cfn_text).expect("Failed to read/parse text");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);
		eprintln!("\nModel:\n===\n{}", print_model_as_tree(&model));

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);

		let stores = model.query(&Query::descendant("wa2:Store"));
		assert_eq!(stores.len(), 2);

		let runs = model.query(&Query::descendant("wa2:Run"));
		assert_eq!(runs.len(), 1);
	}

	#[test]
	fn test_evidence_query() {
		let cfn_text = r#"
Resources:
  Bucket1:
    Type: AWS::S3::Bucket
    Properties:
      Tags:
        - Key: DataSensitivity
          Value: Confidential
        - Key: DataCriticality
          Value: BusinessCritical
      ReplicationConfiguration:
        Role: !GetAtt ReplicationRole.Arn
        Rules:
          - Status: Enabled
            Destination:
              Bucket: arn:aws:s3:::destination-bucket
  ReplicationRole:
    Type: AWS::IAM::Role
    Properties:
      AssumeRolePolicyDocument:
        Version: "2012-10-17"
        Statement: []
"#;

		let template = parse_text_and_validate(cfn_text).expect("Failed to read/parse text");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);
		eprintln!("\nModel:\n===\n{}", print_model_as_tree(&model));

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);

		let bucket = model.resolve("Bucket1").expect("bucket should exist");
		assert!(has_evidence(&model, bucket, "DataResiliance"));
		assert!(!has_evidence(&model, bucket, "SomethingElse"));
	}

	#[test]
	fn test_model_display() {
		let cfn_text = r#"
Resources:
  TestBucket:
    Type: AWS::S3::Bucket
    Properties:
      Tags:
        - Key: Environment
          Value: Test
"#;

		let template = parse_text_and_validate(cfn_text).expect("Failed to read/parse text");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);
		let display = format!("{}", model);

		assert!(display.contains("TestBucket"));
		assert!(display.contains("wa2:Store"));
	}

	#[test]
	fn tutorial_step_0() {
		println!("{}", env::current_dir().unwrap().display());
		let path = Path::new("../../examples/tutorial/0.naive.yaml");
		let template = parse_and_validate(path).expect("Failed to read/parse file");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);
		eprintln!("\nModel:\n===\n{}", print_model_as_tree(&model));

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);
		assert_eq!(guides.len(), 2, "should fail");
		assert!(matches!(guides[0].focus, FocusTaxonomy::DataSensitivity));
		assert!(matches!(guides[1].focus, FocusTaxonomy::DataCriticality));
	}

	#[test]
	fn tutorial_step_1() {
		println!("{}", env::current_dir().unwrap().display());
		let path = Path::new("../../examples/tutorial/1.calm.yaml");
		let template = parse_and_validate(path).expect("Failed to read/parse file");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);
		eprintln!("\nModel:\n===\n{}", print_model_as_tree(&model));

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);
		assert_eq!(guides.len(), 4, "should fail: not tagged");
		assert!(matches!(guides[0].focus, FocusTaxonomy::DataSensitivity));
		assert!(matches!(guides[1].focus, FocusTaxonomy::DataCriticality));
	}

	#[test]
	fn tutorial_step_2() {
		println!("{}", env::current_dir().unwrap().display());
		let path = Path::new("../../examples/tutorial/2.wa2tags.yaml");
		let template = parse_and_validate(path).expect("Failed to read/parse file");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);
		eprintln!("\nModel:\n===\n{}", print_model_as_tree(&model));

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);
		assert_eq!(guides.len(), 1, "should fail: not tagged");
		assert!(matches!(guides[0].focus, FocusTaxonomy::DataResiliance));
	}

	#[test]
	fn tutorial_step_3() {
		println!("{}", env::current_dir().unwrap().display());
		let path = Path::new("../../examples/tutorial/3.with-replication.yaml");
		let template = parse_and_validate(path).expect("Failed to read/parse file");

		let model = project_vendor_aws(&template).expect("projection");
		eprintln!("\nModel:\n===\n{}", &model);
		eprintln!("\nModel:\n===\n{}", print_model_as_tree(&model));

		// GUIDANCE: is guidance required?
		let guides = guidance(&model);
		eprintln!("Guidance:\n===\n{:?}", guides);
		assert!(guides.is_empty(), "all good");
		panic!();
	}
}
