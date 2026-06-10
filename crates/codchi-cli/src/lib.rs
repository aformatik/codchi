//! `codchi-cli` — the v1 command-line client (binary `codchi`).
//!
//! Phase 1 (C4) provides the typed [`HttpClient`]: a
//! [`CodchiService`](codchi_api::CodchiService) implementation that speaks the
//! v1 contract over the per-user Unix socket (D6), so command code calls
//! semantic methods rather than building URLs. The client-initiated daemon
//! spawn and the `codchi status` command land in C5.

pub mod client;
pub mod service_impl;

pub use client::HttpClient;
