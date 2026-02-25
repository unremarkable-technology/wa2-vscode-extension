use super::model::{Axis, Cmp, EntityId, Model, Query, Value};

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
	DataResilience,
}
// ─── Query Helpers ───

/// Get the cfn:Resource linked to a core:Node
fn get_source(model: &Model, node: EntityId) -> Option<EntityId> {
	let query = Query::follow("core:source");
	model.query_from(&[node], &query).first().copied()
}

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

/// Check if a core:Node has evidence of a given type
pub fn has_evidence(model: &Model, node: EntityId, evidence_name: &str) -> bool {
	let query = Query::descendant("core:Evidence").filter(
		"core:value",
		Cmp::Eq,
		Value::Literal(evidence_name.to_string()),
	);

	!model.query_from(&[node], &query).is_empty()
}

// ─── Guidance ───

/// Compute guidance for a model
pub fn guidance(model: &Model) -> Vec<Guide> {
	let mut guides = Vec::new();

	// Find all stores (core:Store nodes)
	let stores = model.query(&Query::descendant("core:Store"));

	for node in stores {
		// Get the underlying cfn:Resource to check tags
		let Some(cfn_resource) = get_source(model, node) else {
			continue;
		};

		let logical_id = model
			.get_literal(cfn_resource, "aws:logicalId")
			.unwrap_or_else(|| model.qualified_name(cfn_resource));

		// Check for mandatory tags (on the cfn:Resource)
		if !has_tag(model, cfn_resource, "DataSensitivity") {
			guides.push(Guide {
				entity: cfn_resource,
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

		if !has_tag(model, cfn_resource, "DataCriticality") {
			guides.push(Guide {
				entity: cfn_resource,
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

		// Check critical data is backed up (evidence on the core:Node)
		if let Some(criticality) = get_tag_value(model, cfn_resource, "DataCriticality") {
			if (criticality == "MissionCritical" || criticality == "BusinessCritical")
				&& !has_evidence(model, node, "DataResilience")
			{
				guides.push(Guide {
					entity: cfn_resource,
					logical_id,
					tldr: "Backup this resource".to_string(),
					message: "All critical stores of information should be backed up.".to_string(),
					why: "This data's criticality indicates we don't want to lose it, \
                        so we need to apply a mechanism to ensure its backed up. \
                        Backup, Snapshots or Replication are common solutions."
						.to_string(),
					level: GuideLevel::Action,
					focus: FocusTaxonomy::DataResilience,
				});
			}
		}
	}

	guides
}
