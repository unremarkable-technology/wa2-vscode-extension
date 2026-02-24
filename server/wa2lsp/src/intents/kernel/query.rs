//! Query execution for XPath-like expressions

use crate::intents::kernel::ast::*;
use crate::intents::kernel::rules::RuleError;
use crate::intents::model::{DOMAIN, EntityId, Model, RANGE, Value};

pub struct QueryEngine;

impl QueryEngine {
	pub fn new() -> Self {
		Self
	}

	pub fn execute(&self, model: &Model, path: &QueryPath) -> Result<Vec<EntityId>, RuleError> {
		if path.steps.is_empty() {
			return Ok(Vec::new());
		}

		let (first_step, rest) = match path.steps.split_first() {
			Some(v) => v,
			None => return Ok(Vec::new()),
		};

		// First step with Descendant axis = global scan
		let mut current = if matches!(first_step.axis, Axis::Descendant | Axis::DescendantOrSelf) {
			// All entities as candidates
			let all: Vec<EntityId> = (0..model.entity_count())
				.map(|i| EntityId(i as u32))
				.collect();
			// Apply type/predicate filters from first step
			self.apply_filters(model, all, first_step)?
		} else {
			let root = match model.root() {
				Some(r) => vec![r],
				None => return Ok(Vec::new()),
			};
			self.execute_step(model, &root, first_step)?
		};

		// Remaining steps
		for step in rest {
			current = self.execute_step(model, &current, step)?;
		}

		Ok(current)
	}

	fn apply_filters(
		&self,
		model: &Model,
		candidates: Vec<EntityId>,
		step: &QueryStep,
	) -> Result<Vec<EntityId>, RuleError> {
		let mut results = Vec::new();

		for candidate in candidates {
			// Node test (type or name)
			if let Some(ref type_name) = step.node_test {
				let qname = type_name.to_string();
				if let Some(resolved) = model.resolve(&qname) {
					if self.is_type(model, resolved) {
						if !model.has_type(candidate, resolved) {
							continue;
						}
					} else {
						if candidate != resolved {
							continue;
						}
					}
				} else {
					continue;
				}
			}

			// Predicates
			if !self.check_predicates(model, candidate, &step.predicates)? {
				continue;
			}

			if !results.contains(&candidate) {
				results.push(candidate);
			}
		}

		Ok(results)
	}

	fn reachable(&self, model: &Model, node: EntityId) -> Vec<EntityId> {
		let mut result = Vec::new();
		let mut stack = vec![node];
		let mut visited = std::collections::HashSet::new();
		visited.insert(node);

		while let Some(current) = stack.pop() {
			for stmt_id in model.outgoing(current) {
				let stmt = model.statement(stmt_id);
				if let Value::Entity(child) = &stmt.object {
					if !visited.contains(child) {
						visited.insert(*child);
						result.push(*child);
						stack.push(*child);
					}
				}
			}
		}

		result
	}

	fn execute_step(
		&self,
		model: &Model,
		input: &[EntityId],
		step: &QueryStep,
	) -> Result<Vec<EntityId>, RuleError> {
		let mut results = Vec::new();

		for &node in input {
			let candidates = match step.axis {
				Axis::Child => {
					if let Some(ref node_test) = step.node_test {
						// Check if it's a type or predicate traversal
						self.traverse(model, node, node_test)?
					} else {
						// Wildcard - get children
						model.children(node)
					}
				}
				Axis::Descendant | Axis::DescendantOrSelf => {
					// For global scan at start, this is handled in execute()
					// For mid-path, traverse reachable entities
					self.reachable(model, node)
				}
			};

			for candidate in candidates {
				// Type test (for Descendant axis, node_test is a type filter)
				if matches!(step.axis, Axis::Descendant | Axis::DescendantOrSelf) {
					if let Some(ref type_name) = step.node_test {
						let qname = type_name.to_string();
						if let Some(type_id) = model.resolve(&qname) {
							if self.is_type(model, type_id) {
								if !model.has_type(candidate, type_id) {
									continue;
								}
							}
						}
					}
				}

				// Predicates
				if !self.check_predicates(model, candidate, &step.predicates)? {
					continue;
				}

				if !results.contains(&candidate) {
					results.push(candidate);
				}
			}
		}

		Ok(results)
	}

	/// Traverse from node via named type or predicate
	fn traverse(
		&self,
		model: &Model,
		node: EntityId,
		name: &QualifiedName,
	) -> Result<Vec<EntityId>, RuleError> {
		let qname = name.to_string();
		let Some(target_id) = model.resolve(&qname) else {
			return Ok(Vec::new());
		};

		if self.is_type(model, target_id) {
			// Type traversal - find related entities of this type
			self.traverse_to_type(model, node, target_id)
		} else {
			// Predicate traversal - follow edge with inferred direction
			self.traverse_predicate(model, node, target_id)
		}
	}

	/// Check if entity is a type (has wa2:type = wa2:Type)
	fn is_type(&self, model: &Model, entity: EntityId) -> bool {
		let Some(wa2_type) = model.resolve("wa2:Type") else {
			return false;
		};
		model.has_type(entity, wa2_type)
	}

	/// Traverse to find related entities of a given type
	fn traverse_to_type(
		&self,
		model: &Model,
		node: EntityId,
		target_type: EntityId,
	) -> Result<Vec<EntityId>, RuleError> {
		let mut results = Vec::new();

		// Check outgoing edges - find entities we point to that have target_type
		for stmt_id in model.outgoing(node) {
			let stmt = model.statement(stmt_id);
			if let Value::Entity(obj) = &stmt.object {
				if model.has_type(*obj, target_type) && !results.contains(obj) {
					results.push(*obj);
				}
			}
		}

		// Check incoming edges - find entities that point to us that have target_type
		for stmt_id in model.incoming(node) {
			let stmt = model.statement(stmt_id);
			if model.has_type(stmt.subject, target_type) && !results.contains(&stmt.subject) {
				results.push(stmt.subject);
			}
		}

		Ok(results)
	}

	/// Traverse a predicate with direction inferred from domain/range
	fn traverse_predicate(
		&self,
		model: &Model,
		node: EntityId,
		predicate: EntityId,
	) -> Result<Vec<EntityId>, RuleError> {
		let mut results = Vec::new();

		// Get predicate's domain and range
		let domain = model.get(predicate, DOMAIN).and_then(|v| v.as_entity());
		let range = model.get(predicate, RANGE).and_then(|v| v.as_entity());

		// Get node's types
		let node_types = model.types(node);

		// Check if we should go forward (node is in domain)
		let go_forward = domain.map_or(true, |d| {
			node_types
				.iter()
				.any(|&t| t == d || self.is_subtype_of(model, t, d))
		});

		// Check if we should go backward (node is in range)
		let go_backward = range.map_or(true, |r| {
			node_types
				.iter()
				.any(|&t| t == r || self.is_subtype_of(model, t, r))
		});

		// Forward: node --predicate--> ?
		if go_forward {
			for value in model.get_all(node, predicate) {
				if let Value::Entity(obj) = value {
					if !results.contains(&obj) {
						results.push(obj);
					}
				}
			}
		}

		// Backward: ? --predicate--> node
		if go_backward {
			for stmt_id in model.incoming(node) {
				let stmt = model.statement(stmt_id);
				if stmt.predicate == predicate && !results.contains(&stmt.subject) {
					results.push(stmt.subject);
				}
			}
		}

		Ok(results)
	}

	/// Check if type_a is a subtype of type_b
	fn is_subtype_of(&self, model: &Model, type_a: EntityId, type_b: EntityId) -> bool {
		if type_a == type_b {
			return true;
		}
		let Some(sub_type_of) = model.resolve("wa2:subTypeOf") else {
			return false;
		};
		// Check direct subtype relationship
		model.has(type_a, sub_type_of, &Value::Entity(type_b))
	}

	fn check_predicates(
		&self,
		model: &Model,
		node: EntityId,
		predicates: &[QueryPredicate],
	) -> Result<bool, RuleError> {
		for pred in predicates {
			if !self.check_predicate(model, node, pred)? {
				return Ok(false);
			}
		}
		Ok(true)
	}

	fn check_predicate(
		&self,
		model: &Model,
		node: EntityId,
		predicate: &QueryPredicate,
	) -> Result<bool, RuleError> {
		match predicate {
			QueryPredicate::Eq(path, literal) => {
				let values = self.follow_path(model, node, path)?;
				let target = self.literal_to_string(literal);
				Ok(values.iter().any(|v| v == &target))
			}

			QueryPredicate::In(path, literals) => {
				let values = self.follow_path(model, node, path)?;
				let targets: Vec<String> =
					literals.iter().map(|l| self.literal_to_string(l)).collect();
				Ok(values.iter().any(|v| targets.contains(v)))
			}

			QueryPredicate::Exists(path) => self.path_has_value(model, node, path),
		}
	}

	/// Check if a path leads to any value (entity or literal)
	fn path_has_value(
		&self,
		model: &Model,
		start: EntityId,
		path: &QueryPath,
	) -> Result<bool, RuleError> {
		let steps = &path.steps;

		let Some((last_step, prefix_steps)) = steps.split_last() else {
			return Ok(false);
		};

		let mut current = vec![start];

		for step in prefix_steps {
			let mut next = Vec::new();
			for &node in &current {
				if let Some(ref name) = step.node_test {
					next.extend(self.traverse(model, node, name)?);
				} else {
					next.extend(model.children(node));
				}
			}
			current = next;
		}

		if let Some(ref name) = last_step.node_test {
			let qname = name.to_string();
			if let Some(resolved) = model.resolve(&qname) {
				if self.is_type(model, resolved) {
					for node in current {
						if !self.traverse_to_type(model, node, resolved)?.is_empty() {
							return Ok(true);
						}
					}
				} else {
					for node in current {
						if !model.get_all(node, resolved).is_empty() {
							return Ok(true);
						}
					}
				}
			}
		}

		Ok(false)
	}

	fn follow_path(
		&self,
		model: &Model,
		start: EntityId,
		path: &QueryPath,
	) -> Result<Vec<String>, RuleError> {
		let steps = &path.steps;

		let Some((last_step, prefix_steps)) = steps.split_last() else {
			return Ok(Vec::new());
		};

		let mut current = vec![start];

		// Follow all steps except the last one
		for step in prefix_steps {
			let mut next = Vec::new();

			for node in &current {
				if let Some(ref name) = step.node_test {
					let entities = self.traverse(model, *node, name)?;
					for e in entities {
						if !next.contains(&e) {
							next.push(e);
						}
					}
				} else {
					for child in model.children(*node) {
						if !next.contains(&child) {
							next.push(child);
						}
					}
				}
			}

			current = next;
		}

		// Last step - extract literal values
		let mut results = Vec::new();

		if let Some(ref name) = last_step.node_test {
			let pred_name = name.to_string();
			if let Some(pred_id) = model.resolve(&pred_name) {
				if !self.is_type(model, pred_id) {
					for node in current {
						for value in model.get_all(node, pred_id) {
							match value {
								Value::Literal(s) => results.push(s),
								Value::Number(n) => results.push(n.to_string()),
								_ => {}
							}
						}
					}
				}
			}
		}

		Ok(results)
	}

	fn literal_to_string(&self, literal: &Literal) -> String {
		match literal {
			Literal::String(s) => s.clone(),
			Literal::Number(n) => n.to_string(),
			Literal::Bool(b) => b.to_string(),
		}
	}
}
