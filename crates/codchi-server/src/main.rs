//! `codchi-server` binary entry point.
//!
//! Binds the per-user Unix socket (D6), constructs the [`ServerCore`] over the
//! store-condition channel, spawns the [`StoreSupervisor`] that owns the store
//! lifecycle, serves the v1 API, and tears everything down in order on
//! SIGINT/SIGTERM (SC8). Machine data remains backed by the internal mock until
//! Phase 4.
//!
//! Platform split: on Unix the store is the real rootless-Podman store and the
//! API is served over the Unix socket. On other platforms (Windows, dev-only)
//! the host store driver and host transport are unported (Phase 12/13), so the
//! daemon runs the mock store under the supervisor and skips the socket — enough
//! for the windows-msvc build to compile and its lifecycle to be exercised.

use std::error::Error;
use std::sync::Arc;
use std::time::Duration;

use chrono::Utc;
use codchi_server::{
    AppState, LogStore, ServerCore, Store, StoreCondition, StoreError, StoreSupervisor,
    StoreSupervisorConfig, db, logging,
};
use codchi_shared::{data_dir, logs_dir};
use tokio::sync::{oneshot, watch};
use tokio_util::sync::CancellationToken;
use tracing::{error, info, warn};

/// Overall shutdown budget bounding HTTP drain + supervisor teardown, so a hung
/// `podman stop` cannot wedge the daemon (SC8 step 6).
const SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(40);

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // Build the log store first: the tracing layer writes the `Server` source,
    // and `ServerCore` serves it — both must share this one instance.
    let logs = LogStore::new(&logs_dir());
    logging::init(logs.clone());

    // Open + migrate the DB synchronously, before binding the socket and
    // bringing the store up (DB-before-store, DB8). A schema problem surfaces as
    // `Degraded` + `startup_error` via the lifecycle projection-join; it never
    // wipes data and never masquerades as `store.unavailable`. The store start
    // below has no dependency on DB content yet (no machine/store tables until
    // Phase 4).
    let db_path = data_dir().join("state.db");
    let startup = db::bring_up(&db_path).await;
    if let codchi_server::SchemaState::Failed(error) = &startup.schema {
        error!(%error, "database bring-up failed; serving in a degraded state");
    } else {
        info!(version = startup.current, "state database ready");
    }

    // The store-condition channel: the supervisor (sole writer, SC7) holds the
    // sender; `ServerCore` (reader) holds the receiver and projects on read.
    let (condition_tx, condition_rx) = watch::channel(StoreCondition::Starting);
    let shutdown = CancellationToken::new();

    let core = Arc::new(ServerCore::new(
        logs.clone(),
        condition_rx,
        shutdown.clone(),
        startup.db,
        startup.schema,
        startup.current,
    ));
    let state = AppState::new(core);

    // SIGINT/SIGTERM (Unix) / Ctrl-C (Windows) trip the token (SC8).
    spawn_signal_handler(shutdown.clone());

    // Bring the store up under the supervisor. If the driver is unavailable,
    // publish a degraded condition and serve without a supervisor (the lifecycle
    // projects `Degraded`, the `store.unavailable` finding appears).
    let mut keepalive_tx = None;
    let supervisor = match build_store() {
        Ok(store) => {
            // `drained` gates the store teardown until HTTP has drained (SC8).
            let (drained_tx, drained_rx) = oneshot::channel();
            let supervisor = StoreSupervisor::new(
                store,
                condition_tx,
                logs.clone(),
                StoreSupervisorConfig::default(),
            );
            let handle = tokio::spawn(supervisor.run(shutdown.clone(), drained_rx));
            Some((handle, drained_tx))
        }
        Err(error) => {
            error!(%error, "store driver unavailable; serving in a degraded state");
            let _ = condition_tx.send(StoreCondition::Degraded {
                reason: error.to_string(),
                since: Utc::now(),
            });
            // Hold the sender so the receiver keeps projecting the last value.
            keepalive_tx = Some(condition_tx);
            None
        }
    };

    // Serve until the token trips, draining in-flight requests first (SC8 step 2).
    serve_until_shutdown(state, shutdown.clone()).await?;

    // HTTP drained. Release the teardown gate and await the supervisor's ordered
    // store teardown (SC8 steps 4–6), bounded by the overall timeout.
    if let Some((handle, drained_tx)) = supervisor {
        let _ = drained_tx.send(());
        if tokio::time::timeout(SHUTDOWN_TIMEOUT, handle)
            .await
            .is_err()
        {
            warn!("shutdown timed out; exiting without a clean store teardown");
        }
    }
    drop(keepalive_tx);
    info!("codchi-server stopped");
    Ok(())
}

/// Build the platform store driver: the real rootless-Podman store on Unix; the
/// mock store on other platforms (Windows, dev-only) until the host store driver
/// is ported (Phase 12/13).
#[cfg(unix)]
fn build_store() -> Result<Arc<dyn Store>, StoreError> {
    Ok(Arc::new(codchi_server::PodmanStore::from_env()?))
}

#[cfg(not(unix))]
fn build_store() -> Result<Arc<dyn Store>, StoreError> {
    Ok(Arc::new(codchi_server::MockStore::new()))
}

/// Bind the per-user Unix socket and serve the v1 API until `shutdown` trips,
/// draining in-flight requests first (SC8 step 2).
#[cfg(unix)]
async fn serve_until_shutdown(
    state: AppState,
    shutdown: CancellationToken,
) -> std::io::Result<()> {
    let socket = codchi_shared::server_socket_path();
    if let Some(parent) = socket.parent() {
        std::fs::create_dir_all(parent)?;
    }
    // A stale socket from a previous run would make `bind` fail with EADDRINUSE;
    // the client's spawn model (D8) treats us as the single owner, so removing
    // it is safe.
    if socket.exists() {
        std::fs::remove_file(&socket)?;
    }
    let listener = tokio::net::UnixListener::bind(&socket)?;
    info!(socket = %socket.display(), "codchi-server listening");

    let app = codchi_server::build_router(state);
    let graceful = shutdown.clone();
    axum::serve(listener, app)
        .with_graceful_shutdown(async move { graceful.cancelled().await })
        .await
}

/// Windows (Phase 12): no host transport yet. The mock store still runs under
/// the supervisor, so the daemon's lifecycle (bring-up → teardown) is exercised;
/// there is just no socket to dial. Block until shutdown so teardown still runs.
#[cfg(not(unix))]
async fn serve_until_shutdown(
    _state: AppState,
    shutdown: CancellationToken,
) -> std::io::Result<()> {
    warn!(
        "codchi-server has no Windows host transport yet (Phase 12); running the \
         mock-store lifecycle only, with no client socket"
    );
    shutdown.cancelled().await;
    Ok(())
}

/// Trip `shutdown` on the first SIGINT or SIGTERM.
#[cfg(unix)]
fn spawn_signal_handler(shutdown: CancellationToken) {
    tokio::spawn(async move {
        use tokio::signal::unix::{SignalKind, signal};
        let mut sigint = match signal(SignalKind::interrupt()) {
            Ok(stream) => stream,
            Err(error) => {
                error!(%error, "cannot install SIGINT handler");
                return;
            }
        };
        let mut sigterm = match signal(SignalKind::terminate()) {
            Ok(stream) => stream,
            Err(error) => {
                error!(%error, "cannot install SIGTERM handler");
                return;
            }
        };
        tokio::select! {
            _ = sigint.recv() => info!("received SIGINT; shutting down"),
            _ = sigterm.recv() => info!("received SIGTERM; shutting down"),
        }
        shutdown.cancel();
    });
}

/// Trip `shutdown` on the first Ctrl-C (Windows, dev-only).
#[cfg(not(unix))]
fn spawn_signal_handler(shutdown: CancellationToken) {
    tokio::spawn(async move {
        match tokio::signal::ctrl_c().await {
            Ok(()) => info!("received Ctrl-C; shutting down"),
            Err(error) => {
                error!(%error, "cannot install Ctrl-C handler");
                return;
            }
        }
        shutdown.cancel();
    });
}
