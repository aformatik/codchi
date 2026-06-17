//! `codchi` binary entry point.
//!
//! Parses the command line, ensures a ready `codchi-server` (spawning it if
//! needed, with a bounded readiness wait — D8), then dispatches. Phase 1 (C5)
//! ships `codchi status`, the default command.

use std::process::ExitCode;

use clap::Parser;
use codchi_cli::cli::{Cli, Command};
#[cfg(unix)]
use codchi_cli::daemon::{StartupConfig, connect_or_spawn};
use codchi_cli::status;

#[tokio::main]
async fn main() -> ExitCode {
    let cli = Cli::parse();

    // Resolve the service to dispatch against, once, up front.
    //
    // Unix: the real daemon over the per-user socket. Every command needs it
    // ready, so we spawn-and-wait here; a failure is a structured startup error
    // (never a hang — D8).
    #[cfg(unix)]
    let client = match connect_or_spawn(&StartupConfig::default()).await {
        Ok(client) => client,
        Err(err) => {
            eprintln!("codchi: {err}");
            return ExitCode::FAILURE;
        }
    };

    // Windows (dev-only): there is no host daemon or transport yet (Phase 12),
    // so dispatch against the in-process contract mock. This keeps `codchi.exe`
    // usable for developing the Windows client without a backend.
    #[cfg(not(unix))]
    let client = codchi_api::testing::MockCodchiService::new();

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
