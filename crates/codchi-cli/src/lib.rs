//! `codchi-cli` — the v1 command-line client (binary `codchi`).
//!
//! Phase 1 provides the typed [`HttpClient`] (C4): a
//! [`CodchiService`](codchi_api::CodchiService) implementation that speaks the
//! v1 contract over the per-user Unix socket (D6), so command code calls
//! semantic methods rather than building URLs. C5 adds client-initiated daemon
//! spawn with a bounded readiness wait ([`daemon`]) and the `codchi status`
//! command ([`status`]).

pub mod cli;
pub mod client;
pub mod daemon;
pub mod service_impl;
pub mod status;

pub use client::HttpClient;
pub use daemon::{StartupConfig, StartupError, connect_or_spawn};
