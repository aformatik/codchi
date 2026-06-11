//! Process-wide tracing setup for `codchi-server`, writing to stderr.
//!
//! This is the "basic stdio logging" slice of C7 (D9): the daemon's own
//! lifecycle and store bring-up become visible on the console. C7 proper will
//! add a second `tracing` layer here that fans `Server`/`Store` source events
//! into durable source logs + the in-memory ring behind `stream_logs` — so the
//! init point stays here and call sites (plain `tracing` macros) don't change.

use tracing_subscriber::{EnvFilter, fmt, prelude::*};

/// Install the global subscriber. Verbosity follows `RUST_LOG`; absent that,
/// `CODCHI_DEBUG` (the same switch the store image's init honors) bumps the
/// default to `debug`, else `info`. Idempotent-ish: a second call is ignored
/// because the global default can only be set once.
pub fn init() {
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| {
        let default = if std::env::var_os("CODCHI_DEBUG").is_some() {
            "codchi_server=debug,info"
        } else {
            "info"
        };
        EnvFilter::new(default)
    });

    let _ = tracing_subscriber::registry()
        .with(filter)
        .with(fmt::layer().with_writer(std::io::stderr))
        // C7: `.with(source_log_layer)` slots in here.
        .try_init();
}
