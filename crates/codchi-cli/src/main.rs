//! `codchi` binary entry point.
//!
//! Parses the command line, ensures a ready `codchi-server` (spawning it if
//! needed, with a bounded readiness wait — D8), then dispatches. Phase 1 (C5)
//! ships `codchi status`, the default command.

use std::process::ExitCode;

use clap::Parser;
use codchi_cli::cli::{Cli, Command};
use codchi_cli::daemon::{StartupConfig, connect_or_spawn};
use codchi_cli::status;

#[tokio::main]
async fn main() -> ExitCode {
    let cli = Cli::parse();

    // Every command needs a ready daemon; resolve it once, up front. A failure
    // here is a structured startup error (never a hang — D8).
    let client = match connect_or_spawn(&StartupConfig::default()).await {
        Ok(client) => client,
        Err(err) => {
            eprintln!("codchi: {err}");
            return ExitCode::FAILURE;
        }
    };

    let result = match cli.command.unwrap_or_default() {
        Command::Status => status::run(&client, cli.json).await,
    };

    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("codchi: {err}");
            ExitCode::FAILURE
        }
    }
}
