//! `codchi-server` binary entry point.
//!
//! Binds the per-user Unix socket (D6), starts the server-owned Podman store
//! manager (C6), and serves the v1 API. Machine data remains backed by
//! [`MockCodchiService`](codchi_api::testing::MockCodchiService) until Phase 2,
//! while lifecycle/store health are real infrastructure state.

use std::error::Error;
use std::sync::Arc;

use axum::serve;
use codchi_api::dto::ServerLifecycle;
use codchi_api::testing::MockCodchiService;
use codchi_server::{AppState, PodmanStore, StoreManager, StoreManagerConfig, build_router, logging};
use codchi_shared::server_socket_path;
use tokio::net::UnixListener;
use tracing::{error, info};

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    logging::init();

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

    let state = AppState::new(Arc::new(MockCodchiService::new()));
    let listener = UnixListener::bind(&socket)?;

    match PodmanStore::from_env() {
        Ok(store) => {
            let manager = StoreManager::new(
                Arc::new(store),
                state.clone(),
                StoreManagerConfig::default(),
            );
            tokio::spawn(manager.run());
        }
        Err(error) => {
            error!(%error, "store driver unavailable; serving in a degraded state");
            state.infrastructure.store_down(error.to_string());
            state.lifecycle.set(ServerLifecycle::Degraded);
        }
    }
    info!(
        socket = %socket.display(),
        lifecycle = ?state.lifecycle.current(),
        "codchi-server listening"
    );

    let app = build_router(state);
    serve(listener, app).await?;
    Ok(())
}
