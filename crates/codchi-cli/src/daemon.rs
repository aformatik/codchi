//! Client-initiated daemon spawn and the bounded readiness wait (D8).
//!
//! The CLI never assumes a running daemon: [`connect_or_spawn`] dials the
//! per-user socket, and on a server that isn't reachable it spawns
//! `codchi-server` detached, then waits for it to report `Ready` — but only up
//! to a bound. The bound is the **hang-forever guard** (D8 / A1): a server that
//! stalls mid-startup, never binds, or comes up `Degraded` surfaces a structured
//! [`StartupError`] instead of blocking the CLI indefinitely.
//!
//! This is the one spawn/lifecycle model across platforms — Windows mirrors it
//! in Phase 12 (`codchi-hostctl.exe` spawns the server) rather than inventing a
//! second path.

use std::fmt;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::Duration;

use codchi_api::dto::{ServerLifecycle, ServerStatus};
use codchi_api::{ApiError, CodchiService};

use crate::client::HttpClient;

/// Environment override for the `codchi-server` binary to spawn. When unset the
/// binary is looked up next to the running `codchi` executable, then on `PATH`.
pub const SERVER_BIN_ENV: &str = "CODCHI_SERVER_BIN";

/// Tunables for the spawn/readiness handshake. Defaults suit interactive use;
/// tests inject a tight bound to exercise the guard quickly.
#[derive(Clone, Copy, Debug)]
pub struct StartupConfig {
    /// Upper bound on the wait for the server to become `Ready`. Once elapsed,
    /// the wait fails with [`StartupError::Timeout`] — it never blocks past this.
    pub timeout: Duration,
    /// Delay between readiness polls.
    pub poll_interval: Duration,
}

impl Default for StartupConfig {
    fn default() -> Self {
        StartupConfig {
            // First-time store initialization may need to populate its Nix
            // runtime. Keep the wait bounded (A1), but aligned with the
            // server's five-minute store-startup bound.
            timeout: Duration::from_secs(5 * 60 + 5),
            poll_interval: Duration::from_millis(100),
        }
    }
}

/// A structured failure of the spawn/readiness handshake — the alternative to
/// hanging (D8). Carries enough context for the CLI to print an actionable line.
#[derive(Debug)]
pub enum StartupError {
    /// The `codchi-server` process could not be spawned.
    Spawn {
        bin: PathBuf,
        source: std::io::Error,
    },
    /// The server did not reach `Ready` within [`StartupConfig::timeout`].
    Timeout { waited: Duration },
    /// The server came up but reported `Degraded`; carries its `startup_error`.
    Degraded { error: Option<ApiError> },
}

impl fmt::Display for StartupError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StartupError::Spawn { bin, source } => {
                write!(
                    f,
                    "could not start codchi-server ({}): {source}",
                    bin.display()
                )
            }
            StartupError::Timeout { waited } => write!(
                f,
                "codchi-server did not become ready within {:.1}s",
                waited.as_secs_f64()
            ),
            StartupError::Degraded { error } => match error {
                Some(err) => write!(f, "codchi-server started in a degraded state: {err}"),
                None => write!(f, "codchi-server started in a degraded state"),
            },
        }
    }
}

impl std::error::Error for StartupError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            StartupError::Spawn { source, .. } => Some(source),
            _ => None,
        }
    }
}

/// Return a client for a ready server, spawning `codchi-server` if it isn't
/// reachable. The fast path is a single `server_status` call; only when that
/// fails (no socket / connection refused) do we spawn and wait. A server that is
/// already reachable is returned as-is — surfacing its (possibly degraded) state
/// is the caller's job; the [`StartupError::Degraded`] path is reserved for a
/// server we just started.
pub async fn connect_or_spawn(config: &StartupConfig) -> Result<HttpClient, StartupError> {
    let client = HttpClient::connect_default();
    if client.server_status().await.is_ok() {
        return Ok(client);
    }
    spawn_server()?;
    await_ready(&client, config).await?;
    Ok(client)
}

/// Poll `server_status` until the server reports `Ready`, bounded by
/// `config.timeout`. The outer timeout is the load-bearing guard: it caps the
/// total wait even if an individual request stalls (a server that accepts the
/// connection but never answers). `Degraded` short-circuits to a structured
/// error; transient states (`Starting`/`Healthcheck`) and connection failures
/// (server still binding) are retried until the deadline.
pub async fn await_ready(
    client: &HttpClient,
    config: &StartupConfig,
) -> Result<ServerStatus, StartupError> {
    let poll = async {
        loop {
            // An `Err` is "not reachable yet" (socket not bound / connection
            // refused) — the server is mid-startup, so keep polling. Transient
            // lifecycle states (`Starting`/`Healthcheck`) likewise keep polling.
            if let Ok(status) = client.server_status().await {
                match status.lifecycle {
                    ServerLifecycle::Ready => return Ok(status),
                    ServerLifecycle::Degraded => {
                        return Err(StartupError::Degraded {
                            error: status.startup_error,
                        });
                    }
                    _ => {}
                }
            }
            tokio::time::sleep(config.poll_interval).await;
        }
    };

    match tokio::time::timeout(config.timeout, poll).await {
        Ok(result) => result,
        Err(_elapsed) => Err(StartupError::Timeout {
            waited: config.timeout,
        }),
    }
}

/// Spawn `codchi-server` detached so it outlives this CLI invocation. stdio is
/// sent to `/dev/null`; the child handle is dropped without waiting (on Unix
/// that does not reap or kill it). Fuller daemonization (`setsid`) is deferred —
/// the readiness handshake, not the process tree, is what the CLI depends on.
fn spawn_server() -> Result<(), StartupError> {
    let bin = server_binary();
    std::process::Command::new(&bin)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|source| StartupError::Spawn { bin, source })?;
    Ok(())
}

/// Locate the `codchi-server` binary: the `CODCHI_SERVER_BIN` override, else a
/// sibling of the running `codchi` executable (how the Nix package lays them
/// out), else the bare name for a `PATH` lookup.
fn server_binary() -> PathBuf {
    if let Some(path) = std::env::var_os(SERVER_BIN_ENV) {
        return PathBuf::from(path);
    }
    if let Ok(exe) = std::env::current_exe()
        && let Some(sibling) = exe.parent().map(|dir| dir.join("codchi-server"))
        && sibling.is_file()
    {
        return sibling;
    }
    PathBuf::from("codchi-server")
}
