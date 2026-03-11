use std::path::PathBuf;
use clap::{Parser, Subcommand};

mod check;

#[derive(Parser)]
#[command(name = "wa2", version, about = "WA2 - Well-Architected 2")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Check a stack against an intent
    Check {
        #[arg(long)]
        profile: String,
        #[arg(long)]
        stack: PathBuf,
        #[arg(long)]
        intent: Option<PathBuf>,
    },
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Check { profile, stack, intent } => {
            check::run(&profile, &stack, intent.as_deref()).await;
        }
    }
}