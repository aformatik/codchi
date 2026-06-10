//! C5 acceptance, incl. **A1** (the hang-forever guard, D8).
//!
//! [`await_ready`] is the bounded readiness wait that `connect_or_spawn` runs
//! after spawning the daemon. These tests drive it directly (rather than
//! spawning the real `codchi-server` binary, which the Phase 1 DoD smoke covers)
//! so the guard's behavior is pinned: a stalled server, a never-bound socket, a
//! degraded server, and the happy path all resolve **within the bound** — none
//! hang.

use std::path::PathBuf;
use std::time::{Duration, Instant};

use codchi_api::dto::ServerLifecycle;
use codchi_cli::HttpClient;
use codchi_cli::daemon::{StartupConfig, StartupError, await_ready};
use codchi_server::{AppState, build_router};
use tokio::net::UnixListener;

/// A tight bound so the guard fires fast in tests.
fn fast_config() -> StartupConfig {
    StartupConfig {
        timeout: Duration::from_millis(400),
        poll_interval: Duration::from_millis(20),
    }
}

fn temp_socket(tag: &str) -> PathBuf {
    std::env::temp_dir().join(format!(
        "codchi-spawn-{tag}-{}-{}.sock",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ))
}

struct SocketGuard(PathBuf);
impl Drop for SocketGuard {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

/// Serve a router whose lifecycle is fixed to `lifecycle` on a throwaway socket.
fn serve_with_lifecycle(lifecycle: ServerLifecycle, tag: &str) -> (HttpClient, SocketGuard) {
    let socket = temp_socket(tag);
    let _ = std::fs::remove_file(&socket);
    let listener = UnixListener::bind(&socket).expect("bind socket");
    let state = AppState::with_mock();
    state.lifecycle.set(lifecycle);
    let app = build_router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    (HttpClient::new(socket.clone()), SocketGuard(socket))
}

/// A1: a server stuck in `Starting` (stalled mid-startup) must make the client
/// time out with a structured error, well within the bound — not hang.
#[tokio::test]
async fn stalled_server_times_out_within_bound() {
    let (client, _guard) = serve_with_lifecycle(ServerLifecycle::Starting, "stalled");
    let config = fast_config();

    let started = Instant::now();
    let err = await_ready(&client, &config)
        .await
        .expect_err("a stalled server must not be reported ready");
    let elapsed = started.elapsed();

    assert!(
        matches!(err, StartupError::Timeout { .. }),
        "expected a structured timeout, got {err:?}"
    );
    // The guard is bounded: it returns shortly after the timeout, never blocks.
    assert!(
        elapsed < config.timeout * 3,
        "await_ready overran its bound: {elapsed:?}"
    );
}

/// A1: a socket nobody is listening on (server killed / never bound) also times
/// out with a structured error rather than blocking on connection failures.
#[tokio::test]
async fn absent_server_times_out_within_bound() {
    let client = HttpClient::new(temp_socket("absent"));
    let config = fast_config();

    let started = Instant::now();
    let err = await_ready(&client, &config)
        .await
        .expect_err("an absent server cannot become ready");
    let elapsed = started.elapsed();

    assert!(
        matches!(err, StartupError::Timeout { .. }),
        "expected a structured timeout, got {err:?}"
    );
    assert!(
        elapsed < config.timeout * 3,
        "await_ready overran its bound: {elapsed:?}"
    );
}

/// A server reporting `Degraded` short-circuits to a structured error — no need
/// to wait out the whole bound.
#[tokio::test]
async fn degraded_server_is_structured_error() {
    let (client, _guard) = serve_with_lifecycle(ServerLifecycle::Degraded, "degraded");

    let err = await_ready(&client, &fast_config())
        .await
        .expect_err("a degraded server is not ready");
    assert!(
        matches!(err, StartupError::Degraded { .. }),
        "expected a structured degraded error, got {err:?}"
    );
}

/// The happy path: a `Ready` server resolves immediately.
#[tokio::test]
async fn ready_server_resolves() {
    let (client, _guard) = serve_with_lifecycle(ServerLifecycle::Ready, "ready");

    let status = await_ready(&client, &fast_config())
        .await
        .expect("a ready server resolves");
    assert_eq!(status.lifecycle, ServerLifecycle::Ready);
}
