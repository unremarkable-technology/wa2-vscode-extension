//! Kernel - loads core types and orchestrates projection + guidance

mod ast;
mod lexer;
mod loader;
mod lower;
mod parser;
mod query;
mod resolver;
mod rules;

use std::collections::HashMap;
use std::path::Path;

use tower_lsp::lsp_types::Diagnostic;
use url::Url;

use crate::intents::kernel::ast::{Modal, Policy, Rule};
use crate::intents::kernel::loader::Loader;
use crate::intents::model::{Model, NAMESPACE_TYPE};
use crate::intents::vendor::{DocumentFormat, Method, Vendor, get_projector};
use crate::wa2_config::Wa2Config;

use lexer::Wa2Source;
use lower::Lower;
use parser::Resolver;
use rules::RuleEngine;

pub use loader::{LoadError, LoadedFile};
pub use resolver::FileResolver;

macro_rules! include_wa2 {
	($file:literal) => {
		include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../../wa2/", $file))
	};
}

const EMBEDDED_BOOTSTRAP: &str = include_wa2!("bootstrap.wa2");
const EMBEDDED_CORE: &str = include_wa2!("core/core.wa2");
const EMBEDDED_AWS: &str = include_wa2!("aws/aws.wa2");
const EMBEDDED_AWS_CFN: &str = include_wa2!("aws/cfn/cfn.wa2");
const EMBEDDED_DATA: &str = include_wa2!("data/data.wa2");
const EMBEDDED_QUICKSTART: &str = include_wa2!("examples/quickstart.wa2");

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
	pub severity: String,
	pub subject: Option<crate::intents::model::EntityId>,
	pub area: Option<crate::intents::model::EntityId>,
	pub message: Option<String>,
}

/// Kernel - the WA2 analysis engine
pub struct Kernel {
	source_path: Option<std::path::PathBuf>,
	model: Model,
	rules: Vec<Rule>,
	policies: Vec<Policy>,
}

impl Default for Kernel {
	fn default() -> Self {
		Self::new()
	}
}

/// Create a resolver from a model that checks if a name is a namespace
fn model_resolver(model: &Model) -> Resolver<'_> {
	Box::new(move |name: &str| {
		if let Some(id) = model.resolve(name) {
			model.has_type(id, NAMESPACE_TYPE)
		} else {
			false
		}
	})
}

impl Kernel {
	pub fn new() -> Self {
		eprintln!("WA2: Kernel starting");
		// Check current directory for wa2.toml
		if let Ok(cwd) = std::env::current_dir() {
			eprintln!("WA2: Kernel checking for config in {:?}", cwd);
			if let Some(config) = Wa2Config::load(&cwd) {
				eprintln!("WA2: Kernel found config");

				// Determine framework root
				let framework_root = config
					.framework_path(&cwd)
					.and_then(|p| p.canonicalize().ok());

				if let Some(entry_path) = config.entry_path(&cwd) {
					if entry_path.exists() {
						eprintln!("WA2: Kernel loading from {:?}", entry_path);
						match Self::from_file_with_framework(&entry_path, framework_root.as_deref())
						{
							Ok(kernel) => return kernel,
							Err(e) => {
								eprintln!("WA2: Failed to load {:?}: {}", entry_path, e);
								eprintln!("WA2: Falling back to embedded bootstrap");
							}
						}
					} else {
						eprintln!(
							"WA2: Entry file {:?} not found, using embedded bootstrap",
							entry_path
						);
					}
				} else {
					eprintln!("WA2: No entry specified in config, using embedded bootstrap");
				}
			}
		}
		// Fall back to embedded bootstrap
		Self::bootstrap_embedded()
	}

	/// Load kernel from a wa2 file (simple case, no framework)
	pub fn from_file(path: &Path) -> Result<Self, String> {
		Self::from_file_with_framework(path, None)
	}

	/// Load kernel from a wa2 file with optional framework path
	pub fn from_file_with_framework(
		entry_path: &Path,
		framework_root: Option<&Path>,
	) -> Result<Self, String> {
		// Bootstrap model with minimal Rust primitives
		let mut model = Model::bootstrap();
		let mut all_rules = Vec::new();
		let mut all_policies = Vec::new();

		// Load minimal bootstrap first (just _internal namespace)
		Self::load_source_into(
			&mut model,
			EMBEDDED_BOOTSTRAP,
			"_internal",
			&mut all_rules,
			&mut all_policies,
		)?;

		// Determine loader root (framework root or entry's parent)
		let loader_root = framework_root
			.map(|p| p.to_path_buf())
			.unwrap_or_else(|| entry_path.parent().unwrap_or(Path::new(".")).to_path_buf());

		// Load entry and all dependencies
		let mut loader = Loader::new(&loader_root);
		let files = loader
			.load_entry(entry_path, &model)
			.map_err(|e| format!("Load error: {}", e))?;

		// Lower each file in dependency order
		for file in files {
			let namespace_context = file.inferred_namespace.as_deref().unwrap_or("core");

			let mut lowerer = Lower::new(&mut model, namespace_context)
				.map_err(|e| format!("Lower error: {}", e.message))?;

			let result = lowerer
				.lower(&file.ast)
				.map_err(|e| format!("Lower error in {:?}: {}", file.path, e.message))?;

			all_rules.extend(result.rules);
			all_policies.extend(result.policies);
		}

		Ok(Self {
			source_path: Some(entry_path.to_path_buf()),
			model,
			rules: all_rules,
			policies: all_policies,
		})
	}

	/// Bootstrap using embedded framework files
	fn bootstrap_embedded() -> Self {
		eprintln!("WA2: Using embedded framework");

		let mut model = Model::bootstrap();
		let mut all_rules = Vec::new();
		let mut all_policies = Vec::new();

		// Load in dependency order
		let sources = [
			(EMBEDDED_BOOTSTRAP, "_internal"),
			(EMBEDDED_CORE, "core"),
			(EMBEDDED_AWS, "aws"),
			(EMBEDDED_AWS_CFN, "aws:cfn"),
			(EMBEDDED_DATA, "data"),
			(EMBEDDED_QUICKSTART, "my"), // quickstart declares namespace my {}
		];

		for (source, namespace) in sources {
			if let Err(e) = Self::load_source_into(
				&mut model,
				source,
				namespace,
				&mut all_rules,
				&mut all_policies,
			) {
				eprintln!("WA2: Failed to load embedded {}: {}", namespace, e);
				panic!("Embedded framework should always load");
			}
			eprintln!("WA2: Loaded embedded namespace '{}'", namespace);
		}

		Self {
			source_path: None,
			model,
			rules: all_rules,
			policies: all_policies,
		}
	}

	/// Load a source string into the model
	/// Load a source string into the model
	fn load_source_into(
		model: &mut Model,
		source: &str,
		namespace_context: &str,
		rules: &mut Vec<Rule>,
		policies: &mut Vec<Policy>,
	) -> Result<(), String> {
		// Ensure namespace exists BEFORE parsing so resolver knows about it
		model
			.ensure_namespace(namespace_context)
			.map_err(|e| format!("Failed to create namespace {}: {}", namespace_context, e))?;

		let wa2_source = Wa2Source::from_str(source);
		let resolver = model_resolver(model);
		let ast = parser::parse_with_resolver(wa2_source.lexer(), resolver)
			.map_err(|e| format!("Parse error at {:?}: {}", e.span, e.message))?;

		let mut lowerer = Lower::new(model, namespace_context)
			.map_err(|e| format!("Lower error: {}", e.message))?;
		let result = lowerer
			.lower(&ast)
			.map_err(|e| format!("Lower error: {}", e.message))?;

		rules.extend(result.rules);
		policies.extend(result.policies);
		Ok(())
	}

	/// Build a map from rule name to its modal (from policy bindings)
	fn build_rule_modals(&self) -> HashMap<String, Modal> {
		let mut modals = HashMap::new();
		for policy in &self.policies {
			for binding in &policy.bindings {
				let rule_name = binding.rule_name.to_string();
				// If rule appears in multiple policies, use strictest modal
				let new_modal = binding.modal;
				modals
					.entry(rule_name)
					.and_modify(|existing| {
						*existing = Self::stricter_modal(*existing, new_modal);
					})
					.or_insert(new_modal);
			}
		}
		modals
	}

	fn stricter_modal(a: Modal, b: Modal) -> Modal {
		match (a, b) {
			(Modal::Must, _) | (_, Modal::Must) => Modal::Must,
			(Modal::Should, _) | (_, Modal::Should) => Modal::Should,
			_ => Modal::May,
		}
	}

	pub fn analyse(
		&self,
		text: &str,
		uri: &Url,
		format: DocumentFormat,
		vendor: Vendor,
		method: Method,
	) -> Result<AnalysisResult, Vec<Diagnostic>> {
		let mut model = self.model.clone();
		let rules = self.rules.clone();

		let projector = get_projector(vendor, method);
		projector.project_into(&mut model, text, uri, format)?;

		// Build rule→modal map from policies
		let rule_modals = self.build_rule_modals();

		let mut engine = RuleEngine::new();
		engine
			.run_with_modals(&mut model, &rules, &rule_modals)
			.map_err(|e| vec![Kernel::rule_error_to_diagnostic(&e)])?;

		let failures = self.collect_failures(&model);

		Ok(AnalysisResult { model, failures })
	}

	fn collect_failures(&self, model: &Model) -> Vec<AssertFailure> {
		let mut failures = Vec::new();

		if let Some(failure_type) = model.resolve("core:AssertFailure") {
			let subject_pred = model.resolve("core:subject");
			let area_pred = model.resolve("core:area");
			let severity_pred = model.resolve("core:severity");

			for i in 0..model.entity_count() {
				let entity = crate::intents::model::EntityId(i as u32);
				if model.has_type(entity, failure_type) {
					let assertion = model
						.get_literal(entity, "core:assertion")
						.unwrap_or_default();

					let severity = severity_pred
						.and_then(|p| model.get(entity, p))
						.and_then(|v| v.as_entity())
						.map(|e| model.qualified_name(e))
						.unwrap_or_else(|| "error".to_string());

					let subject = subject_pred
						.and_then(|p| model.get(entity, p))
						.and_then(|v| v.as_entity());

					let area = area_pred
						.and_then(|p| model.get(entity, p))
						.and_then(|v| v.as_entity());

					let message = model.get_literal(entity, "core:message");

					failures.push(AssertFailure {
						entity,
						assertion,
						severity,
						subject,
						area,
						message,
					});
				}
			}
		}

		failures
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
	fn test_embedded_bootstrap_loads() {
		let kernel = Kernel::bootstrap_embedded();

		// Should have loaded core types
		assert!(kernel.model.resolve("core:Store").is_some());
		assert!(kernel.model.resolve("core:Workload").is_some());
		assert!(kernel.model.resolve("aws:cfn:Resource").is_some());
		assert!(kernel.model.resolve("data:Criticality").is_some());

		// Should have rules
		assert!(!kernel.rules.is_empty());

		// Should have policies
		assert!(!kernel.policies.is_empty());
	}

	#[test]
	fn test_derive_stores_rule() {
		let kernel = Kernel::bootstrap_embedded();
		let mut model = kernel.model.clone();
		let rules = kernel.rules.clone();

		eprintln!("Loaded {} rules", rules.len());
		for rule in &rules {
			eprintln!("  - {}", rule.name);
		}

		let workload = model.ensure_entity("core:workload").unwrap();
		model
			.apply_to(workload, "wa2:type", "core:Workload")
			.unwrap();
		model.set_root(workload);

		let template = model.blank();
		model
			.apply_to(template, "wa2:type", "aws:cfn:Template")
			.unwrap();
		model
			.apply_entity(workload, "core:source", template)
			.unwrap();

		let resources = model.blank();
		model
			.apply_entity(template, "aws:cfn:resources", resources)
			.unwrap();

		let bucket = model.ensure_raw("MyBucket");
		model
			.apply_to(bucket, "wa2:type", "aws:cfn:Resource")
			.unwrap();
		model
			.apply_to(bucket, "aws:type", "\"AWS::S3::Bucket\"")
			.unwrap();
		model
			.apply_entity(resources, "wa2:contains", bucket)
			.unwrap();

		let mut engine = RuleEngine::new();
		engine.run(&mut model, &rules).expect("run rules");

		let store_type = model
			.resolve("core:Store")
			.expect("core:Store should exist");

		let stores: Vec<_> = (0..model.entity_count())
			.map(|i| crate::intents::model::EntityId(i as u32))
			.filter(|&id| model.has_type(id, store_type))
			.collect();

		eprintln!("Found {} Store nodes", stores.len());
		assert_eq!(stores.len(), 1, "Should have created one Store node");

		let children = model.children(workload);
		let store_in_children = children.iter().any(|&c| model.has_type(c, store_type));
		assert!(store_in_children, "Store should be child of workload");
	}
}
