//! `codchi-server` binary entry point.
//!
//! Binds the per-user Unix socket (D6), constructs the [`ServerCore`] over the
//! store-condition channel, spawns the [`StoreSupervisor`] that owns the real
//! Podman store lifecycle, serves the v1 API, and tears everything down in order
//! on SIGINT/SIGTERM (SC8). Machine data remains backed by the internal mock
//! until Phase 4.

use std::error::Error;
use std::sync::Arc;
use std::time::Duration;

use axum::serve;
use chrono::Utc;
use codchi_server::{
    AppState, LogStore, PodmanStore, ServerCore, StoreCondition, StoreSupervisor,
    StoreSupervisorConfig, build_router, db, logging,
};
use codchi_shared::{data_dir, logs_dir, server_socket_path};
use tokio::net::UnixListener;
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
    let listener = UnixListener::bind(&socket)?;

    // Open + migrate the DB synchronously, between `bind` and `serve` and
    // DB-before-store (DB8). A schema problem surfaces as `Degraded` +
    // `startup_error` via the lifecycle projection-join; it never wipes data and
    // never masquerades as `store.unavailable`. The store start below has no
    // dependency on DB content yet (no machine/store tables until Phase 4).
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

    // SIGINT/SIGTERM trip the token (SC8). Unix-only; Windows is Phase 12.
    spawn_signal_handler(shutdown.clone());

    // Bring the store up under the supervisor. If the driver is unavailable,
    // publish a degraded condition and serve without a supervisor (the lifecycle
    // projects `Degraded`, the `store.unavailable` finding appears).
    let mut keepalive_tx = None;
    let supervisor = match PodmanStore::from_env() {
        Ok(store) => {
            // `drained` gates the store teardown until HTTP has drained (SC8).
            let (drained_tx, drained_rx) = oneshot::channel();
            let supervisor = StoreSupervisor::new(
                Arc::new(store),
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

    info!(socket = %socket.display(), "codchi-server listening");

    // Serve until the token trips, draining in-flight requests first (SC8 step 2).
    let app = build_router(state);
    let graceful = shutdown.clone();
    serve(listener, app)
        .with_graceful_shutdown(async move { graceful.cancelled().await })
        .await?;

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

/// Trip `shutdown` on the first SIGINT or SIGTERM.
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
