//! `codchi-server` binary entry point.
//!
//! Binds the per-user Unix socket (D6) and serves the v1 API. Phase 1 (C3) holds
//! the [`MockCodchiService`](codchi_api::testing::MockCodchiService) behind the
//! router and flips the lifecycle straight to `Ready` after binding; C6 will
//! drive the real `Starting → Healthcheck → Ready/Degraded` lifecycle off the
//! Podman store bring-up.

use std::error::Error;

use axum::serve;
use codchi_api::dto::ServerLifecycle;
use codchi_server::{AppState, build_router};
use codchi_shared::server_socket_path;
use tokio::net::UnixListener;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let socket = server_socket_path();
    if let Some(parent) = socket.parent() {
        std::fs::create_dir_all(parent)?;
    }
    // A stale socket from a previous run would make `bind` fail with EADDRINUSE;
    // the client's spawn model (D8) treats us as the single owner, so removing
    // it is safe.
    if socket.exists() {
        std::fs::remove_file(&socket)?;
    }

    let state = AppState::with_mock();
    let listener = UnixListener::bind(&socket)?;
    // C3 has no real store to bring up yet (that is C6), so readiness is
    // immediate once the socket is live.
    state.lifecycle.set(ServerLifecycle::Ready);
    eprintln!(
        "codchi-server listening on {} ({:?})",
        socket.display(),
        state.lifecycle.current()
    );

    let app = build_router(state);
    serve(listener, app).await?;
    Ok(())
}
