use id_arena::Arena;

use super::node::{Annotation, Node, NodeId, NodeKind};

/// A declared node
#[derive(Debug, Default)]
pub struct System {
	/// Nodes in this system
	pub nodes: Arena<Node>,
}

#[derive(Debug)]
pub enum NodeError {
	InvalidNode,
	GuidanceRequired,
}

impl System {
	pub fn add_node(&mut self, name: &str, kind: NodeKind) -> NodeId {
		self.nodes.alloc(Node {
			name: name.to_owned(),
			kind,
			annotations: vec![],
		})
	}

	pub fn node(&self, id: NodeId) -> Result<&Node, NodeError> {
		self.nodes.get(id).ok_or(NodeError::InvalidNode)
	}

	pub fn node_mut(&mut self, id: NodeId) -> Result<&mut Node, NodeError> {
		self.nodes.get_mut(id).ok_or(NodeError::InvalidNode)
	}

	pub fn annotate(&mut self, id: NodeId, annotation: Annotation) -> Result<(), NodeError> {
		self.node_mut(id)?.annotations.push(annotation);
		Ok(())
	}

	pub fn guidance(&self) -> Vec<Guide> {
		let mut guides = Vec::<Guide>::default();

		for (id, node) in self.nodes.iter() {
			if matches!(node.kind, NodeKind::Store) {
				// check for mandatory tags
				if !node.has_tag(FocusTaxonomy::DataSensitivity) {
					guides.push(Guide {
						node: id,
						tldr: "Tag this resource for DataSensitivity".to_string(),
						message: "All stores of information should have data sensitivity."
							.to_string(),
						why: "Tagging store with sensitivity speeds up design decisions, \
						we can apply the same designs for all data of the same class. \
						For example website assets don't need encryption, whilst \
						a healthcare record needs encryption and access restricted."
							.to_string(),
						level: GuideLevel::Required,
						focus: FocusTaxonomy::DataSensitivity,
					});
				}

				if !node.has_tag(FocusTaxonomy::DataCriticality) {
					guides.push(Guide {
						node: id,
						tldr: "Tag this resource for DataCriticality".to_string(),
						message: "All stores of information should have a data criticality."
							.to_string(),
						why: "Tagging store with criticality speeds up design decisions, \
						we can apply the same designs for all data of the same class. \
						For example easily recreated data does not need backing up, whilst \
						employment records need protection against loss."
							.to_string(),
						level: GuideLevel::Required,
						focus: FocusTaxonomy::DataCriticality,
					});
				}

				// check any criticl data is backed up
				if node.has_tag_value(
					FocusTaxonomy::DataCriticality,
					[Some("MissionCritical"), Some("BusinessCritical")],
				) && !node.has_evidence(FocusTaxonomy::DataResiliance)
				{
					guides.push(Guide {
						node: id,
						tldr: "Backup this resource".to_string(),
						message: "All critical stores of information should be backed up."
							.to_string(),
						why: "This data's criticality indicates we don't want to lose it, \
					so we need to apply a mechanism to ensure its backed up. \
					Backup, Snapshots or Replication are common solutions."
							.to_string(),
						level: GuideLevel::Action,
						focus: FocusTaxonomy::DataCriticality,
					});
				}
			}
		}

		guides
	}
}

#[derive(Debug)]
pub struct Guide {
	pub node: NodeId,
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

use std::fmt;

pub struct PrettySystem<'a> {
	pub system: &'a System,
}

impl<'a> fmt::Display for PrettySystem<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for (id, node) in self.system.nodes.iter() {
			writeln!(f, "{} {} ({})", node.kind.glyph(), node.name, id.index(),)?;

			for ann in &node.annotations {
				writeln!(f, "  - {:?}", ann)?;
			}
		}
		Ok(())
	}
}
