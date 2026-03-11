use std::fs;
use std::path::Path;
use std::process;

use url::Url;
use wa2lsp::iaac::cloudformation::cfn_ir::types::CfnTemplate;
use wa2lsp::iaac::cloudformation::spec_cache::load_default_spec_store;
use wa2lsp::intents::kernel::Kernel;
use wa2lsp::intents::vendor::{DocumentFormat, Method, Vendor};

pub async fn run(profile: &str, stack: &Path, intent: Option<&Path>) {
	// Validate stack exists
	if !stack.exists() {
		eprintln!("Error: Stack file not found: {}", stack.display());
		process::exit(1);
	}

	// Load stack text
	let stack_text = match fs::read_to_string(stack) {
		Ok(t) => t,
		Err(e) => {
			eprintln!("Error: Could not read stack file: {}", e);
			process::exit(1);
		}
	};

	let stack_uri =
		Url::from_file_path(stack.canonicalize().unwrap_or_else(|_| stack.to_path_buf()))
			.unwrap_or_else(|_| Url::parse("file:///unknown").unwrap());

	// Determine format from path
	let format = DocumentFormat::from_language_id_or_path(None, &stack_uri);

	// Fast path: parse stack
	let template = match format {
		DocumentFormat::Json => CfnTemplate::from_json(&stack_text, &stack_uri),
		DocumentFormat::Yaml => CfnTemplate::from_yaml(&stack_text, &stack_uri),
	};

	let template = match template {
		Ok(t) => t,
		Err(diags) => {
			eprintln!("Stack {}: syntax errors", stack.display());
			for d in diags {
				eprintln!("  - {}", d.message);
			}
			process::exit(1);
		}
	};

	// Validate against CloudFormation spec
	match load_default_spec_store().await {
		Ok(spec_store) => {
			let spec_diags = template.validate_against_spec(&spec_store);
			if !spec_diags.is_empty() {
				eprintln!("Stack {}: specification errors", stack.display());
				for d in spec_diags {
					eprintln!("  - {}", d.message);
				}
				process::exit(1);
			}
		}
		Err(e) => {
			eprintln!("Warning: Could not load CloudFormation spec: {}", e);
			eprintln!("         Skipping spec validation.");
		}
	}

	println!(
		"Stack {}: parsed and validated successfully.",
		stack.display()
	);

	// Load kernel (uses wa2.toml if present, else embedded)
   let skip_quickstart = intent.is_some();
   let mut kernel = Kernel::new(skip_quickstart);

	// Layer intent file if provided
	if let Some(intent_path) = intent {
		if !intent_path.exists() {
			eprintln!("Error: Intent file not found: {}", intent_path.display());
			process::exit(1);
		}

		if let Err(e) = kernel.load_intent(intent_path) {
			eprintln!("Intent {}: error", intent_path.display());
			eprintln!("  {}", e);
			process::exit(1);
		}

		println!(
			"Intent {}: parsed and validated successfully.",
			intent_path.display()
		);
	}

	// Override profile selection
	if let Err(e) = kernel.set_profile(profile.to_string()) {
		eprintln!("Error: {}", e);
		process::exit(1);
	}

	// Run analysis
	let result = kernel.analyse(
		&stack_text,
		&stack_uri,
		format,
		Vendor::Aws,
		Method::CloudFormation,
	);

	match result {
		Ok(analysis) => {
			if analysis.failures.is_empty() {
				println!("\nSuccess: stack does satisfy intent of profile {}.", profile);
			} else {
				println!(
					"\nFailed: stack does not satisfy intent of profile {}",
					profile
				);
				println!("\nCauses:");
				for failure in &analysis.failures {
					println!("✖ {}", failure.assertion);
					if let Some(subject) = failure.subject {
						let name = analysis.model.qualified_name(subject);
						println!("  - Subject: {}", name);
					}
					if let Some(ref msg) = failure.message {
						println!("  - Message: {}", msg);
					}
				}
				process::exit(1);
			}
		}
		Err(diags) => {
			eprintln!("Stack {}: analysis errors", stack.display());
			for d in diags {
				eprintln!("  - {}", d.message);
			}
			process::exit(1);
		}
	}
}
