//! Command-line surface (clap).
//!
//! Phase 1 (C5) ships only `codchi status`, which is also the default when no
//! subcommand is given — mirroring the beta. The full command parity pass is
//! Phase 14, so this stays deliberately small.

use clap::{Parser, Subcommand};

/// Top-level `codchi` invocation.
#[derive(Parser, Debug)]
#[command(
    name = "codchi",
    version,
    about = "Codchi — reproducible code machines"
)]
pub struct Cli {
    /// Emit machine-readable JSON instead of human-readable text.
    #[arg(long, global = true)]
    pub json: bool,

    #[command(subcommand)]
    pub command: Option<Command>,
}

/// Subcommands. `codchi` with no subcommand defaults to [`Command::Status`].
#[derive(Subcommand, Debug, Default)]
pub enum Command {
    /// Show daemon lifecycle, store state, and machines (the default command).
    #[default]
    Status,
}
