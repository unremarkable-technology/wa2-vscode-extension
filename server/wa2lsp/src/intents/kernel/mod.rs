//! Kernel - loads core types and orchestrates projection + guidance

mod ast;
mod lexer;
mod lower;
mod parser;
mod query;
mod rules;

use tower_lsp::lsp_types::Diagnostic;
use url::Url;

use crate::intents::kernel::ast::Rule;
use crate::intents::model::Model;
use crate::intents::vendor::{DocumentFormat, Method, Vendor, get_projector};

use lexer::Wa2Source;
use lower::Lower;
use rules::RuleEngine;

/// Result of analyzing a document
pub struct AnalysisResult {
	pub model: Model,
	pub failures: Vec<AssertFailure>,
}

/// An assertion failure from rule execution
#[derive(Debug)]
pub struct AssertFailure {
	pub entity: crate::intents::model::EntityId,
	pub assertion: String,
}

/// Kernel - the WA2 analysis engine
pub struct Kernel {
	bootstrap_source: String,
	model: Model,
	rules: Vec<Rule>,
}

impl Default for Kernel {
	fn default() -> Self {
		Self::new()
	}
}

impl Kernel {
	pub fn new() -> Self {
		Kernel::bootstrap()
	}

	fn bootstrap() -> Self {
		// Load bootstrap.wa2 from embedded or file
		let bootstrap_source =
			include_str!("../../../../../wa2/core/v0.1/bootstrap.wa2").to_string();

		// 1. Bootstrap model with minimal Rust primitives
		let mut model = Model::bootstrap();

		// 2. Parse bootstrap.wa2
		let source = Wa2Source::from_str(&bootstrap_source);
		let ast = parser::parse(source.lexer())
			.map_err(|e| vec![Kernel::parse_error_to_diagnostic(&e)])
			.unwrap();

		// 3. Lower AST to model (types, predicates, instances)
		let mut lowerer = Lower::new(&mut model, "core")
			.map_err(|e| vec![Kernel::lower_error_to_diagnostic(&e)])
			.unwrap();
		let rules = lowerer
			.lower(&ast)
			.map_err(|e| vec![Kernel::lower_error_to_diagnostic(&e)])
			.unwrap();

		Self {
			bootstrap_source,
			model,
			rules,
		}
	}

	/// Analyze a document, returning model and failures
	pub fn analyse(
		&self,
		text: &str,
		uri: &Url,
		format: DocumentFormat,
		vendor: Vendor,
		method: Method,
	) -> Result<AnalysisResult, Vec<Diagnostic>> {
		// keeps original model, rules clean
		let mut model = self.model.clone();
		let rules = self.rules.clone();

		// 4. Project vendor IaC into the same model
		let projector = get_projector(vendor, method);
		projector.project_into(&mut model, text, uri, format)?;

		// 5. Run rules to fixed-point
		let mut engine = RuleEngine::new();
		engine
			.run(&mut model, &rules)
			.map_err(|e| vec![Kernel::rule_error_to_diagnostic(&e)])?;

		// 6. Query for assertion failures
		let failures = self.collect_failures(&model);

		Ok(AnalysisResult { model, failures })
	}

	fn collect_failures(&self, model: &Model) -> Vec<AssertFailure> {
		let mut failures = Vec::new();

		if let Some(failure_type) = model.resolve("core:AssertFailure") {
			for i in 0..model.entity_count() {
				let entity = crate::intents::model::EntityId(i as u32);
				if model.has_type(entity, failure_type) {
					let assertion = model
						.get_literal(entity, "core:assertion")
						.unwrap_or_default();
					failures.push(AssertFailure { entity, assertion });
				}
			}
		}

		failures
	}

	fn parse_error_to_diagnostic(err: &parser::ParseError) -> Diagnostic {
		Diagnostic {
			range: tower_lsp::lsp_types::Range::default(),
			severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
			message: format!("Parse error: {}", err.message),
			..Default::default()
		}
	}

	fn lower_error_to_diagnostic(err: &lower::LowerError) -> Diagnostic {
		Diagnostic {
			range: tower_lsp::lsp_types::Range::default(),
			severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
			message: format!("Lower error: {}", err.message),
			..Default::default()
		}
	}

	fn rule_error_to_diagnostic(err: &rules::RuleError) -> Diagnostic {
		Diagnostic {
			range: tower_lsp::lsp_types::Range::default(),
			severity: Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR),
			message: format!("Rule error: {}", err.message),
			..Default::default()
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::intents::kernel::{Kernel, rules::RuleEngine};

	#[test]
	fn test_derive_stores_rule() {
		// 1. Bootstrap and load DSL
		let kernel = Kernel::new();
		// keeps original model, rules clean
		let mut model = kernel.model.clone();
		let rules = kernel.rules.clone();

		eprintln!("Loaded {} rules", rules.len());
		for rule in &rules {
			eprintln!("  - {}", rule.name);
		}

		// 2. Manually create CFN structure (simulating projector without derive phase)
		let workload = model.ensure_entity("core:workload").unwrap();
		model
			.apply_to(workload, "wa2:type", "core:Workload")
			.unwrap();
		model.set_root(workload);

		let template = model.blank();
		model
			.apply_to(template, "wa2:type", "cfn:Template")
			.unwrap();
		model
			.apply_entity(workload, "core:source", template)
			.unwrap();

		let resources = model.blank();
		model
			.apply_entity(template, "cfn:resources", resources)
			.unwrap();

		let bucket = model.ensure_raw("MyBucket");
		model.apply_to(bucket, "wa2:type", "cfn:Resource").unwrap();
		model
			.apply_to(bucket, "aws:type", "\"AWS::S3::Bucket\"")
			.unwrap();
		model
			.apply_entity(resources, "wa2:contains", bucket)
			.unwrap();

		//eprintln!("Before rules:\n{}", model);

		// 3. Run rules
		let mut engine = RuleEngine::new();
		engine.run(&mut model, &rules).expect("run rules");

		//eprintln!("After rules:\n{}", model);

		// 4. Verify core:Store was created
		let store_type = model
			.resolve("core:Store")
			.expect("core:Store should exist");

		// Find entities with type core:Store
		let stores: Vec<_> = (0..model.entity_count())
			.map(|i| crate::intents::model::EntityId(i as u32))
			.filter(|&id| model.has_type(id, store_type))
			.collect();

		eprintln!("Found {} Store nodes", stores.len());
		assert_eq!(stores.len(), 1, "Should have created one Store node");

		// Verify it's attached to workload
		let children = model.children(workload);
		let store_in_children = children.iter().any(|&c| model.has_type(c, store_type));
		assert!(store_in_children, "Store should be child of workload");
	}

	#[test]
	fn test_derive_replication_evidence() {
		let kernel = Kernel::new();
		let mut model = kernel.model.clone();
		let rules = kernel.rules.clone();

		// Create workload and template structure
		let workload = model.ensure_entity("core:workload").unwrap();
		model
			.apply_to(workload, "wa2:type", "core:Workload")
			.unwrap();
		model.set_root(workload);

		let template = model.blank();
		model
			.apply_to(template, "wa2:type", "cfn:Template")
			.unwrap();
		model
			.apply_entity(workload, "core:source", template)
			.unwrap();

		let resources = model.blank();
		model
			.apply_entity(template, "cfn:resources", resources)
			.unwrap();

		// SourceBucket with replication configuration
		let source_bucket = model.ensure_raw("SourceBucket");
		model
			.apply_to(source_bucket, "wa2:type", "cfn:Resource")
			.unwrap();
		model
			.apply_to(source_bucket, "aws:type", "\"AWS::S3::Bucket\"")
			.unwrap();
		model
			.apply_entity(resources, "wa2:contains", source_bucket)
			.unwrap();

		// VersioningConfiguration.Status = Enabled
		let versioning = model.blank();
		model
			.apply_entity(source_bucket, "aws:VersioningConfiguration", versioning)
			.unwrap();
		model
			.apply_to(versioning, "aws:Status", "\"Enabled\"")
			.unwrap();

		// ReplicationConfiguration
		let replication = model.blank();
		model
			.apply_entity(source_bucket, "aws:ReplicationConfiguration", replication)
			.unwrap();

		// ReplicationConfiguration.Role (via GetAtt, but we just need it to exist)
		let role_ref = model.blank();
		model.apply_to(role_ref, "wa2:type", "cfn:GetAtt").unwrap();
		model
			.apply_entity(replication, "aws:Role", role_ref)
			.unwrap();

		// ReplicationConfiguration.Rules[]
		let rules_container = model.blank();
		model
			.apply_entity(replication, "aws:Rules", rules_container)
			.unwrap();

		let rule1 = model.blank();
		model
			.apply_entity(rules_container, "wa2:contains", rule1)
			.unwrap();
		model.apply_to(rule1, "aws:Id", "\"ReplicateAll\"").unwrap();
		model.apply_to(rule1, "aws:Status", "\"Enabled\"").unwrap();
		model.apply_to(rule1, "aws:Priority", "\"1\"").unwrap();

		// Run derive_stores first to create core:Store
		let mut engine = RuleEngine::new();
		engine.run(&mut model, &rules).expect("run rules");

		//eprintln!("After rules:\n{}", model);

		// Verify core:Store was created for SourceBucket
		let store_type = model
			.resolve("core:Store")
			.expect("core:Store should exist");
		let stores: Vec<_> = (0..model.entity_count())
			.map(|i| crate::intents::model::EntityId(i as u32))
			.filter(|&id| model.has_type(id, store_type))
			.collect();

		assert_eq!(
			stores.len(),
			1,
			"Should have one Store node for SourceBucket"
		);
		let store = stores[0];

		// Verify Evidence was attached
		let evidence_type = model
			.resolve("core:Evidence")
			.expect("core:Evidence should exist");
		let store_children = model.children(store);
		let evidence_nodes: Vec<_> = store_children
			.iter()
			.filter(|&&c| model.has_type(c, evidence_type))
			.collect();

		eprintln!(
			"Found {} Evidence nodes attached to Store",
			evidence_nodes.len()
		);
		assert_eq!(evidence_nodes.len(), 1, "Should have one Evidence node");

		// Verify evidence value
		let evidence = evidence_nodes[0];
		let value = model.get_literal(*evidence, "core:value");
		assert_eq!(
			value,
			Some("DataResilience".to_string()),
			"Evidence should be DataResilience"
		);
      //panic!();
	}
}
