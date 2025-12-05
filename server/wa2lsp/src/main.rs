use std::{env, process};
use tokio::io::{stdin, stdout};
use tower_lsp::{LspService, Server};
use wa2lsp::server::Backend;

#[tokio::main]
async fn main() {
	let mut args = env::args().skip(1);

	match args.next().as_deref() {
		Some("--serve") => {
			// Normal LSP mode: used by VS Code
			run_lsp().await;
		}
		Some("--version") | Some("-V") => {
			println!("wa2lsp {}", env!("CARGO_PKG_VERSION"));
		}
		Some("--help") | Some("-h") => {
			eprintln!(
				"wa2lsp - WA2 Language Server\n\n\
                 Usage:\n  wa2lsp --serve      Start the LSP server on stdin/stdout\n  wa2lsp --version   Show version\n  wa2lsp --help      Show this help"
			);
		}
		Some(other) => {
			eprintln!(
				"wa2lsp: unrecognised argument '{}'\n\
                 Try 'wa2lsp --help' for usage.",
				other
			);
			process::exit(2);
		}
		None => {
			eprintln!(
				"wa2lsp: no arguments provided.\n\
                 Usage: wa2lsp --serve\n\
                 For more options, run: wa2lsp --help"
			);
			process::exit(1);
		}
	}
}

async fn run_lsp() {
	// constructs our instance of the Backend
	let (service, socket) = LspService::new(Backend::new);

	// starts reading JSON RPC messages from stdin, replies to stdout
	Server::new(stdin(), stdout(), socket).serve(service).await;
}
