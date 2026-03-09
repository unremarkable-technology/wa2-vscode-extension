//! WA2 project configuration

use std::fs;
use std::path::{Path, PathBuf};

use serde::Deserialize;

/// WA2 project configuration from wa2.toml
#[derive(Debug, Deserialize)]
pub struct Wa2Config {
	pub project: ProjectConfig,
	pub framework: Option<FrameworkConfig>,
}

#[derive(Debug, Deserialize)]
pub struct ProjectConfig {
	pub name: String,
	#[serde(default)]
	pub entry: Option<String>,
	#[serde(default)]
	pub profile: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct FrameworkConfig {
	pub path: String,
}

impl Wa2Config {
	/// Load config from a directory, returns None if no wa2.toml found
	pub fn load(dir: &Path) -> Option<Self> {
		let config_path = dir.join("wa2.toml");
		let contents = fs::read_to_string(&config_path).ok()?;
		let config: Self = toml::from_str(&contents).ok()?;

		// Validate: cannot have both entry and profile
		if config.project.entry.is_some() && config.project.profile.is_some() {
			eprintln!(
				"WA2: Error in wa2.toml: cannot specify both 'entry' and 'profile' in [project]"
			);
			return None;
		}

		Some(config)
	}

	/// Get entry path if specified
	pub fn entry_path(&self, base_dir: &Path) -> Option<PathBuf> {
		self.project.entry.as_ref().map(|e| base_dir.join(e))
	}

	/// Get profile if specified in config
	pub fn profile(&self) -> Option<&str> {
		self.project.profile.as_deref()
	}

	/// Resolve the framework path relative to the config directory
	pub fn framework_path(&self, base_dir: &Path) -> Option<PathBuf> {
		self.framework.as_ref().map(|f| base_dir.join(&f.path))
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use std::io::Write;
	use tempfile::TempDir;

	#[test]
	fn parse_minimal_config() {
		let toml = r#"
[project]
name = "my-policies"
"#;
		let config: Wa2Config = toml::from_str(toml).unwrap();
		assert_eq!(config.project.name, "my-policies");
		assert_eq!(config.project.entry, None);
		assert!(config.framework.is_none());
	}

	#[test]
	fn parse_config_with_entry() {
		let toml = r#"
[project]
name = "my-policies"
entry = "src/main.wa2"
"#;
		let config: Wa2Config = toml::from_str(toml).unwrap();
		assert_eq!(config.project.name, "my-policies");
		assert_eq!(config.project.entry, Some("src/main.wa2".to_string()));
	}

	#[test]
	fn parse_config_with_framework() {
		let toml = r#"
[project]
name = "my-policies"
entry = "main.wa2"

[framework]
path = "../wa2-framework"
"#;
		let config: Wa2Config = toml::from_str(toml).unwrap();
		assert_eq!(config.project.name, "my-policies");
		assert_eq!(config.project.entry, Some("main.wa2".to_string()));
		assert_eq!(config.framework.as_ref().unwrap().path, "../wa2-framework");
	}

	#[test]
	fn load_from_directory() {
		let dir = TempDir::new().unwrap();
		let config_path = dir.path().join("wa2.toml");

		let mut file = fs::File::create(&config_path).unwrap();
		writeln!(
			file,
			r#"
[project]
name = "test-project"
entry = "policies.wa2"

[framework]
path = "../framework"
"#
		)
		.unwrap();

		let config = Wa2Config::load(dir.path()).unwrap();
		assert_eq!(config.project.name, "test-project");
		assert_eq!(config.project.entry, Some("policies.wa2".to_string()));
		assert_eq!(config.framework.as_ref().unwrap().path, "../framework");
	}

	#[test]
	fn load_missing_config_returns_none() {
		let dir = TempDir::new().unwrap();
		assert!(Wa2Config::load(dir.path()).is_none());
	}

	#[test]
	fn entry_path_resolution() {
		let toml = r#"
[project]
name = "test"
entry = "src/main.wa2"
"#;
		let config: Wa2Config = toml::from_str(toml).unwrap();
		let base = Path::new("/workspace");
		assert_eq!(
			config.entry_path(base),
			Some(PathBuf::from("/workspace/src/main.wa2"))
		);
	}

	#[test]
	fn framework_path_resolution() {
		let toml = r#"
[project]
name = "test"

[framework]
path = "../wa2-framework"
"#;
		let config: Wa2Config = toml::from_str(toml).unwrap();
		let base = Path::new("/workspace/project");
		assert_eq!(
			config.framework_path(base),
			Some(PathBuf::from("/workspace/project/../wa2-framework"))
		);
	}

	#[test]
	fn no_entry_returns_none() {
		let toml = r#"
[project]
name = "test"
"#;
		let config: Wa2Config = toml::from_str(toml).unwrap();
		let base = Path::new("/workspace");
		assert_eq!(config.entry_path(base), None);
	}
}
