//! Process-wide tracing setup for `codchi-server` (C7 / D9).
//!
//! Two sinks, each with its **own** filter so they don't constrain each other:
//! - a human-readable stderr console, gated by `RUST_LOG` / `CODCHI_DEBUG`
//!   (operator-facing verbosity);
//! - the [`ServerLogLayer`], which fans `codchi_server` events at `INFO`+ into
//!   the durable `Server` source log — independent of console verbosity, so the
//!   log stays stable whatever `RUST_LOG` is set to.
//!
//! Because the filters are per-layer (not a global registry filter), turning the
//! console down to `warn` does not starve the `Server` log of `info` lines.

use tracing::Level;
use tracing_subscriber::filter::filter_fn;
use tracing_subscriber::{EnvFilter, fmt, prelude::*};

use crate::logs::{LogStore, ServerLogLayer};

/// Install the global subscriber, fanning the daemon's events to stderr and to
/// the `Server` source log. Idempotent-ish: a second call is ignored (the global
/// default can only be set once).
pub fn init(logs: LogStore) {
    // Console verbosity: explicit `RUST_LOG` wins; else `CODCHI_DEBUG` (the same
    // switch the store image's init honors) bumps our crate to debug; else info.
    let console_filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| {
        let default = if std::env::var_os("CODCHI_DEBUG").is_some() {
            "codchi_server=debug,info"
        } else {
            "info"
        };
        EnvFilter::new(default)
    });

    // Durable Server log: our crate's own narrative at info+, always. (`Level`
    // orders ERROR < WARN < INFO < DEBUG < TRACE, so `<= INFO` is info+severe.)
    let capture_filter = filter_fn(|meta| {
        meta.target().starts_with("codchi_server") && *meta.level() <= Level::INFO
    });

    let _ = tracing_subscriber::registry()
        .with(
            fmt::layer()
                .with_writer(std::io::stderr)
                .with_filter(console_filter),
        )
        .with(ServerLogLayer::new(logs).with_filter(capture_filter))
        .try_init();
}
