//! Multi-file loader for WA2 sources

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use crate::intents::kernel::ast::{Ast, Item};
use crate::intents::kernel::lexer::{Token, Wa2Source};
use crate::intents::kernel::parser::{self, ParseError, Resolver};
use crate::intents::kernel::resolver::FileResolver;
use crate::intents::model::{Model, NAMESPACE_TYPE};

/// Multi-file loader
pub struct Loader {
	root: PathBuf,
	resolver: FileResolver,
	loaded_namespaces: HashSet<String>,
}

#[derive(Debug)]
pub enum LoadError {
	Parse(ParseError, PathBuf), // Add path
	Resolve(String, PathBuf),
	Io(PathBuf, std::io::Error),
	Circular(Vec<String>),
}

impl std::fmt::Display for LoadError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LoadError::Parse(e, path) => write!(
				f,
				"parse error in {:?} at offset {}: {}",
				path, e.span.start, e.message
			),
			LoadError::Resolve(ns, path) => write!(f, "namespace '{}' not found at {:?}", ns, path),
			LoadError::Io(path, e) => write!(f, "failed to read {:?}: {}", path, e),
			LoadError::Circular(chain) => write!(f, "circular dependency: {}", chain.join(" -> ")),
		}
	}
}

impl Loader {
	pub fn new(root: impl Into<PathBuf>) -> Self {
		let root = root.into();
		Self {
			resolver: FileResolver::new(&root),
			root,
			loaded_namespaces: HashSet::new(),
		}
	}

	/// Load an entry file and all its dependencies
	pub fn load_entry(
		&mut self,
		entry_path: &Path,
		model: &Model,
	) -> Result<Vec<LoadedFile>, LoadError> {
		let mut result = Vec::new();
		let mut loading_stack = Vec::new();

		self.load_file_recursive(entry_path, None, model, &mut result, &mut loading_stack)?;

		Ok(result)
	}

	/// Load a file and recursively load its dependencies
	fn load_file_recursive(
		&mut self,
		path: &Path,
		inferred_namespace: Option<String>,
		model: &Model,
		result: &mut Vec<LoadedFile>,
		loading_stack: &mut Vec<String>,
	) -> Result<(), LoadError> {
		// Skip if this namespace already loaded
		if let Some(ref ns) = inferred_namespace {
			if self.loaded_namespaces.contains(ns) {
				return Ok(());
			}
		}

		let path_str = path.to_string_lossy().to_string();

		// Check for circular dependency
		if loading_stack.contains(&path_str) {
			loading_stack.push(path_str);
			return Err(LoadError::Circular(loading_stack.clone()));
		}

		loading_stack.push(path_str.clone());

		// Read source
		let source = fs::read_to_string(path).map_err(|e| LoadError::Io(path.to_path_buf(), e))?;

		// Extract uses from AST after a preliminary parse (won't fail on namespace issues)
		// Actually, we need to extract uses BEFORE full parse
		// Use lexer to extract use statements
		let uses = extract_uses_from_source(&source);

		// Load dependencies first (this populates loaded_namespaces)
		for use_ns in &uses {
			let ns_path = self.resolver.resolve(use_ns).ok_or_else(|| {
				let expected = self
					.resolver
					.resolve_or_err(use_ns)
					.unwrap_err()
					.expected_path;
				LoadError::Resolve(use_ns.clone(), expected)
			})?;

			let inferred = Some(use_ns.clone());
			self.load_file_recursive(&ns_path, inferred, model, result, loading_stack)?;
		}

		// Register this namespace BEFORE parsing so self-references work
		if let Some(ref ns) = inferred_namespace {
			self.loaded_namespaces.insert(ns.clone());
		}

		// Now parse with resolver that knows all loaded namespaces
		let wa2_source = Wa2Source::from_string(source.clone());
		let resolver = self.namespace_resolver(model);
		let ast = parser::parse_with_resolver(wa2_source.lexer(), resolver)
			.map_err(|e| LoadError::Parse(e, path.to_path_buf()))?;

		// Add to result
		result.push(LoadedFile {
			path: path.to_path_buf(),
			source,
			ast,
			inferred_namespace: inferred_namespace.clone(),
		});

		// Log what we loaded
		if let Some(ref ns) = inferred_namespace {
			eprintln!("WA2: Loaded namespace '{}' from {:?}", ns, path);
		} else {
			eprintln!("WA2: Loaded entry {:?}", path);
		}

		loading_stack.pop();
		Ok(())
	}

	/// Create resolver that knows loaded namespaces + model namespaces
	fn namespace_resolver<'a>(&'a self, model: &'a Model) -> Resolver<'a> {
		Box::new(move |name: &str| {
			if self.loaded_namespaces.contains(name) {
				return true;
			}
			if let Some(id) = model.resolve(name) {
				model.has_type(id, NAMESPACE_TYPE)
			} else {
				false
			}
		})
	}
}

/// Extract use statements from source using just the lexer
/// Extract use statements from source using just the lexer
fn extract_uses_from_source(source: &str) -> Vec<String> {
	let wa2_source = Wa2Source::from_str(source);
	let mut lexer = wa2_source.lexer();
	let mut uses = Vec::new();
	let mut pending_use = false;

	while let Some(tok_result) = lexer.next() {
		if let Ok(tok) = tok_result {
			if pending_use {
				// We're collecting namespace parts
				match tok {
					Token::Ident(s) => {
						// Start or continue collecting parts
						let last: &mut String = uses.last_mut().unwrap();
						if last.is_empty() {
							*last = s;
						} else {
							last.push(':');
							last.push_str(&s);
						}
					}
					Token::Colon => {
						// Continue to next part
					}
					Token::KwUse => {
						// New use statement
						uses.push(String::new());
					}
					_ => {
						// End of this use statement
						pending_use = false;
					}
				}
			} else if tok == Token::KwUse {
				pending_use = true;
				uses.push(String::new());
			}
		}
	}

	// Remove any empty entries
	uses.retain(|s| !s.is_empty());
	uses
}

/// A loaded file with its AST
#[derive(Debug)]
pub struct LoadedFile {
	pub path: PathBuf,
	pub source: String,
	pub ast: Ast,
	pub inferred_namespace: Option<String>,
}

/// Extract namespace names from use statements
fn extract_uses(ast: &Ast) -> Vec<String> {
	let mut uses = Vec::new();
	for item in &ast.items {
		if let Item::Use(u) = item {
			uses.push(u.namespace.to_string());
		}
	}
	uses
}

#[cfg(test)]
mod tests {
	use super::*;
	use tempfile::TempDir;

	fn create_test_framework(dir: &Path) {
		// core/core.wa2
		fs::create_dir_all(dir.join("core")).unwrap();
		fs::write(
			dir.join("core/core.wa2"),
			r#"
enum Node { Store, Run, Move }
struct Workload { nodes: Node[] }
predicate source
instance core:workload: core:Workload
"#,
		)
		.unwrap();

		// aws/aws.wa2
		fs::create_dir_all(dir.join("aws")).unwrap();
		fs::write(
			dir.join("aws/aws.wa2"),
			r#"
predicate type
predicate logicalId
"#,
		)
		.unwrap();

		// aws/cfn/cfn.wa2
		fs::create_dir_all(dir.join("aws/cfn")).unwrap();
		fs::write(
			dir.join("aws/cfn/cfn.wa2"),
			r#"
use core
use aws

type Template
type Resource
"#,
		)
		.unwrap();

		// data/data.wa2
		fs::create_dir_all(dir.join("data")).unwrap();
		fs::write(
			dir.join("data/data.wa2"),
			r#"
use core

struct Criticality {}
type isCritical
"#,
		)
		.unwrap();

		// quickstart.wa2 (entry)
		fs::write(
			dir.join("quickstart.wa2"),
			r#"
use core
use aws
use aws:cfn
use data

namespace my {
    enum DataCriticality { Low, High }
}
"#,
		)
		.unwrap();
	}

	#[test]
	fn load_single_file() {
		let dir = TempDir::new().unwrap();
		fs::create_dir_all(dir.path().join("core")).unwrap();
		fs::write(
			dir.path().join("core/core.wa2"),
			r#"
enum Node { Store }
"#,
		)
		.unwrap();

		let model = Model::bootstrap();
		let mut loader = Loader::new(dir.path());
		let files = loader
			.load_entry(&dir.path().join("core/core.wa2"), &model)
			.unwrap();

		assert_eq!(files.len(), 1);
		assert!(files[0].path.ends_with("core/core.wa2"));
	}

	#[test]
	fn load_with_dependencies() {
		let dir = TempDir::new().unwrap();
		create_test_framework(dir.path());

		let model = Model::bootstrap();
		let mut loader = Loader::new(dir.path());
		let files = loader
			.load_entry(&dir.path().join("quickstart.wa2"), &model)
			.unwrap();

		// Should load: core, aws, aws:cfn, data, quickstart (in dependency order)
		assert_eq!(files.len(), 5);

		// Dependencies should come before dependents
		let names: Vec<_> = files
			.iter()
			.map(|f| f.path.file_stem().unwrap().to_string_lossy().to_string())
			.collect();

		// core should come before cfn (cfn uses core)
		let core_idx = names.iter().position(|n| n == "core").unwrap();
		let cfn_idx = names.iter().position(|n| n == "cfn").unwrap();
		assert!(core_idx < cfn_idx, "core should load before cfn");

		// aws should come before cfn
		let aws_idx = names.iter().position(|n| n == "aws").unwrap();
		assert!(aws_idx < cfn_idx, "aws should load before cfn");
	}

	#[test]
	fn detect_circular_dependency() {
		let dir = TempDir::new().unwrap();

		// a uses b, b uses a
		fs::create_dir_all(dir.path().join("a")).unwrap();
		fs::write(dir.path().join("a/a.wa2"), "use b").unwrap();

		fs::create_dir_all(dir.path().join("b")).unwrap();
		fs::write(dir.path().join("b/b.wa2"), "use a").unwrap();

		let model = Model::bootstrap();
		let mut loader = Loader::new(dir.path());
		let err = loader
			.load_entry(&dir.path().join("a/a.wa2"), &model)
			.unwrap_err();

		assert!(matches!(err, LoadError::Circular(_)));
	}

	#[test]
	fn missing_dependency() {
		let dir = TempDir::new().unwrap();

		fs::create_dir_all(dir.path().join("test")).unwrap();
		fs::write(dir.path().join("test/test.wa2"), "use nonexistent").unwrap();

		let model = Model::bootstrap();
		let mut loader = Loader::new(dir.path());
		let err = loader
			.load_entry(&dir.path().join("test/test.wa2"), &model)
			.unwrap_err();

		assert!(matches!(err, LoadError::Resolve(_, _)));
	}
}
