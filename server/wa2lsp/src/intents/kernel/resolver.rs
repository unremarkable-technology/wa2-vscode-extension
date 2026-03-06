//! File resolver for namespace imports

use std::path::PathBuf;

/// Resolves namespace names to file paths
pub struct FileResolver {
	root: PathBuf,
}

impl FileResolver {
	pub fn new(root: impl Into<PathBuf>) -> Self {
		Self { root: root.into() }
	}

	/// Resolve a namespace to a file path
	/// e.g., "core" -> "<root>/core/core.wa2"
	/// e.g., "aws:cfn" -> "<root>/aws/cfn/cfn.wa2"
	pub fn resolve(&self, namespace: &str) -> Option<PathBuf> {
		let parts: Vec<&str> = namespace.split(':').collect();
		if parts.is_empty() {
			return None;
		}

		// Build directory path
		let mut path = self.root.clone();
		for part in &parts {
			path.push(part);
		}

		// Add filename: last part + .wa2
		let filename = format!("{}.wa2", parts.last().unwrap());
		path.push(&filename);

		if path.exists() { Some(path) } else { None }
	}

	/// Resolve and return error if not found
	pub fn resolve_or_err(&self, namespace: &str) -> Result<PathBuf, ResolveError> {
		self.resolve(namespace).ok_or_else(|| ResolveError {
			namespace: namespace.to_string(),
			expected_path: self.expected_path(namespace),
		})
	}

	/// Get the expected path (for error messages)
	fn expected_path(&self, namespace: &str) -> PathBuf {
		let parts: Vec<&str> = namespace.split(':').collect();
		let mut path = self.root.clone();
		for part in &parts {
			path.push(part);
		}
		let filename = format!("{}.wa2", parts.last().unwrap_or(&""));
		path.push(&filename);
		path
	}
}

#[derive(Debug)]
pub struct ResolveError {
	pub namespace: String,
	pub expected_path: PathBuf,
}

impl std::fmt::Display for ResolveError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"namespace '{}' not found at {:?}",
			self.namespace, self.expected_path
		)
	}
}

impl std::error::Error for ResolveError {}

#[cfg(test)]
mod tests {
	use super::*;
	use std::fs;
	use tempfile::TempDir;
   use std::path::Path;

	fn setup_test_files(dir: &Path) {
		// core/core.wa2
		fs::create_dir_all(dir.join("core")).unwrap();
		fs::write(dir.join("core/core.wa2"), "// core").unwrap();

		// aws/aws.wa2
		fs::create_dir_all(dir.join("aws")).unwrap();
		fs::write(dir.join("aws/aws.wa2"), "// aws").unwrap();

		// aws/cfn/cfn.wa2
		fs::create_dir_all(dir.join("aws/cfn")).unwrap();
		fs::write(dir.join("aws/cfn/cfn.wa2"), "// cfn").unwrap();

		// data/data.wa2
		fs::create_dir_all(dir.join("data")).unwrap();
		fs::write(dir.join("data/data.wa2"), "// data").unwrap();
	}

	#[test]
	fn resolve_simple_namespace() {
		let dir = TempDir::new().unwrap();
		setup_test_files(dir.path());

		let resolver = FileResolver::new(dir.path());

		let path = resolver.resolve("core").unwrap();
		assert_eq!(path, dir.path().join("core/core.wa2"));
	}

	#[test]
	fn resolve_nested_namespace() {
		let dir = TempDir::new().unwrap();
		setup_test_files(dir.path());

		let resolver = FileResolver::new(dir.path());

		let path = resolver.resolve("aws:cfn").unwrap();
		assert_eq!(path, dir.path().join("aws/cfn/cfn.wa2"));
	}

	#[test]
	fn resolve_missing_namespace() {
		let dir = TempDir::new().unwrap();
		setup_test_files(dir.path());

		let resolver = FileResolver::new(dir.path());

		assert!(resolver.resolve("unknown").is_none());
	}

	#[test]
	fn resolve_or_err_missing() {
		let dir = TempDir::new().unwrap();
		setup_test_files(dir.path());

		let resolver = FileResolver::new(dir.path());

		let err = resolver.resolve_or_err("unknown").unwrap_err();
		assert_eq!(err.namespace, "unknown");
		assert!(err.expected_path.ends_with("unknown/unknown.wa2"));
	}

	#[test]
	fn resolve_all_framework_namespaces() {
		let dir = TempDir::new().unwrap();
		setup_test_files(dir.path());

		let resolver = FileResolver::new(dir.path());

		assert!(resolver.resolve("core").is_some());
		assert!(resolver.resolve("aws").is_some());
		assert!(resolver.resolve("aws:cfn").is_some());
		assert!(resolver.resolve("data").is_some());
	}
}
