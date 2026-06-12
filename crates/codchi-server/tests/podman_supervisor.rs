//! SC9 real-podman E2E: **start → ready → shutdown → container stopped** — the
//! closest in-tree proxy to Phase 7's no-orphan acceptance. Drives the actual
//! [`StoreSupervisor`] over a real [`PodmanStore`], so it needs podman and the
//! baked `CODCHI_PODMAN_STORE_IMAGE`; `#[ignore]`d so the normal `cargo test`
//! gate stays hermetic. Run with:
//!
//! ```text
//! cargo test -p codchi-server --test podman_supervisor -- --ignored
//! ```

#![cfg(unix)]

use std::sync::Arc;
use std::time::Duration;

use codchi_server::{
    LogStore, PodmanStore, Store, StoreCondition, StorePlatformStatus, StoreSupervisor,
    StoreSupervisorConfig,
};
use tokio::sync::{oneshot, watch};
use tokio_util::sync::CancellationToken;

/// How long to wait for the real store's `nix-daemon` to answer. First-time
/// bring-up (image load + volume init) is the slow case.
const READY_TIMEOUT: Duration = Duration::from_secs(180);

#[tokio::test]
#[ignore = "requires real podman + CODCHI_PODMAN_STORE_IMAGE"]
async fn real_store_starts_becomes_ready_then_stops_on_shutdown() {
    let store: Arc<PodmanStore> = Arc::new(
        PodmanStore::from_env().expect("CODCHI_PODMAN_STORE_IMAGE must be set for this test"),
    );

    let (tx, mut rx) = watch::channel(StoreCondition::Starting);
    let supervisor = StoreSupervisor::new(
        store.clone(),
        tx,
        LogStore::memory(),
        StoreSupervisorConfig::default(),
    );

    let token = CancellationToken::new();
    let (drained_tx, drained_rx) = oneshot::channel();
    let handle = tokio::spawn(supervisor.run(token.clone(), drained_rx));

    // start → ready: the condition reaches `Up` within the deadline (no hang).
    tokio::time::timeout(
        READY_TIMEOUT,
        rx.wait_for(|c| matches!(c, StoreCondition::Up { .. })),
    )
    .await
    .expect("store did not become ready within the deadline")
    .expect("supervisor dropped the condition channel");

    // shutdown: trip the token, release the teardown gate, await the supervisor.
    token.cancel();
    let _ = drained_tx.send(());
    tokio::time::timeout(Duration::from_secs(60), handle)
        .await
        .expect("supervisor did not terminate within the shutdown bound")
        .expect("supervisor task panicked");

    // container stopped: the store is no longer running (no orphan).
    let status = store.status().expect("query store status");
    assert_eq!(
        status,
        StorePlatformStatus::Stopped,
        "the store container must be stopped after shutdown"
    );
}
