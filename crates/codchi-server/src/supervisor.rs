//! The store supervisor (SC7) — sole writer of the [`StoreCondition`].
//!
//! Reshaped from the Phase-1 `StoreManager`: instead of a detached task poking
//! shared `Arc<RwLock>` mutators, the supervisor *owns* `condition` as a plain
//! field and publishes every change through a [`watch::Sender`]. Because it is
//! the single task that holds the sender, "nobody else can mutate the condition"
//! is enforced by the borrow checker, not by convention (SC6). [`ServerCore`]
//! holds the matching [`watch::Receiver`] and only reads/projects.
//!
//! It owns store *mechanism*: bring-up, the post-startup observe-only health
//! sentinel (P6 — recovery is observed, never driven), the `Store` log follower,
//! and the ordered teardown on shutdown (SC8).
//!
//! [`ServerCore`]: crate::core::ServerCore

use std::sync::Arc;
use std::time::Duration;

use chrono::Utc;
use tokio::sync::{oneshot, watch};
use tokio::task::JoinHandle;
use tokio_util::sync::CancellationToken;
use tracing::{debug, error, info, warn};

use crate::core::{ProbeOutcome, StoreCondition, step};
use crate::logs::{LogStore, capture_store_logs};
use crate::platform::{Store, StoreError, StorePlatformStatus};

/// Store lifecycle timings. Tests use short values; production keeps the
/// sentinel cadence locked by P6 and gives first-time Nix setup a bounded wait.
#[derive(Clone, Copy, Debug)]
pub struct StoreSupervisorConfig {
    pub startup_timeout: Duration,
    pub probe_interval: Duration,
    pub sentinel_interval: Duration,
    /// Upper bound on `store.stop()` during teardown so a hung `podman stop`
    /// cannot wedge shutdown (SC8 step 6, the supervisor's share of it).
    pub stop_timeout: Duration,
}

impl Default for StoreSupervisorConfig {
    fn default() -> Self {
        Self {
            startup_timeout: Duration::from_secs(5 * 60),
            probe_interval: Duration::from_millis(250),
            sentinel_interval: Duration::from_secs(15),
            stop_timeout: Duration::from_secs(30),
        }
    }
}

/// Owns the store condition, its bring-up, the health sentinel, and teardown.
pub struct StoreSupervisor {
    store: Arc<dyn Store>,
    /// The single source of truth for the condition — a plain field, mutated
    /// only by this task (SC6).
    condition: StoreCondition,
    tx: watch::Sender<StoreCondition>,
    logs: LogStore,
    config: StoreSupervisorConfig,
    /// The `Store` log follower (`podman logs --follow`); aborting it on teardown
    /// detaches the follower (its `StoreLogStream` is `kill_on_drop`).
    follower: Option<JoinHandle<()>>,
}

impl StoreSupervisor {
    /// Build a supervisor that publishes to `tx`. The initial condition is read
    /// back from the channel (`main` seeds it with [`StoreCondition::Starting`]).
    pub fn new(
        store: Arc<dyn Store>,
        tx: watch::Sender<StoreCondition>,
        logs: LogStore,
        config: StoreSupervisorConfig,
    ) -> Self {
        let condition = tx.borrow().clone();
        Self {
            store,
            condition,
            tx,
            logs,
            config,
            follower: None,
        }
    }

    /// Advance to a new condition and publish it. The only write path.
    fn publish(&mut self, condition: StoreCondition) {
        self.condition = condition.clone();
        // A send only fails if every receiver has dropped; the daemon is then
        // shutting down and nobody is reading anyway.
        let _ = self.tx.send(condition);
    }

    /// Bring the store to a healthy running state and publish the result.
    ///
    /// `Running → noop`, `Stopped → start`, `NotInstalled → register + start`;
    /// never re-`register`s an existing container (Store Authority / no-orphans).
    pub async fn startup(&mut self) -> Result<(), StoreError> {
        info!("bringing up the store");
        self.publish(StoreCondition::Starting);

        match self.bring_container_up().await {
            Ok(()) => {}
            Err(error) => {
                self.degrade(&error);
                return Err(error);
            }
        }

        self.publish(StoreCondition::Checking);
        info!("waiting for the store's nix-daemon to answer");
        match self.await_health().await {
            Ok(()) => {
                info!("store is ready");
                self.publish(step(&self.condition, ProbeOutcome::Healthy, Utc::now()));
                self.spawn_follower();
                Ok(())
            }
            Err(error) => {
                self.degrade(&error);
                Err(error)
            }
        }
    }

    async fn bring_container_up(&self) -> Result<(), StoreError> {
        match self.call(|store| store.status()).await? {
            StorePlatformStatus::NotInstalled => {
                info!("store container absent; creating and starting it");
                self.call(|store| store.register()).await?;
                self.call(|store| store.start()).await?;
            }
            StorePlatformStatus::Stopped => {
                info!("store container present but stopped; starting it");
                self.call(|store| store.start()).await?;
            }
            // A running store is taken as-is. Detecting that it is *stale* (built
            // from a superseded image) and recreating it (S4) belongs to the
            // store-update/generation work in a later phase, not startup.
            StorePlatformStatus::Running => {
                debug!("store container already running");
            }
        }
        Ok(())
    }

    /// Probe the store until it answers or the startup deadline passes.
    async fn await_health(&self) -> Result<(), StoreError> {
        let deadline = tokio::time::Instant::now() + self.config.startup_timeout;
        loop {
            match self.call(|store| store.probe_health()).await {
                Ok(()) => return Ok(()),
                Err(error) if tokio::time::Instant::now() < deadline => {
                    debug!(%error, "store not ready yet; retrying");
                    tokio::time::sleep(self.config.probe_interval).await;
                }
                Err(error) => return Err(error),
            }
        }
    }

    /// Publish `Degraded` from a bring-up/probe failure (SC6 `step` keeps the
    /// `since` stable if already degraded).
    fn degrade(&mut self, error: &StoreError) {
        error!(%error, "store unavailable; entering degraded");
        let outcome = ProbeOutcome::Unhealthy {
            reason: error.to_string(),
        };
        self.publish(step(&self.condition, outcome, Utc::now()));
    }

    /// One observe-only sentinel probe (P6): probe, `step`, publish. Never
    /// `register`/`start` — recovery is observed, not driven.
    pub async fn reconcile_once(&mut self) {
        let outcome = match self.call(|store| store.probe_health()).await {
            Ok(()) => ProbeOutcome::Healthy,
            Err(error) => ProbeOutcome::Unhealthy {
                reason: error.to_string(),
            },
        };
        let was_up = matches!(self.condition, StoreCondition::Up { .. });
        let next = step(&self.condition, outcome, Utc::now());
        match (&next, was_up) {
            (StoreCondition::Degraded { reason, .. }, true) => {
                warn!(%reason, "store became unavailable")
            }
            (StoreCondition::Up { .. }, false) => info!("store recovered; back to ready"),
            _ => {}
        }
        self.publish(next);
    }

    /// Attach to the running store and stream its output into the `Store` source
    /// log (C7). Best-effort: if the follower can't be opened the store still
    /// runs, we just have no store logs this run.
    fn spawn_follower(&mut self) {
        match self.store.attach() {
            Ok(stream) => {
                self.follower = Some(tokio::spawn(capture_store_logs(stream, self.logs.clone())));
            }
            Err(error) => {
                warn!(%error, "could not attach to store output; store logs unavailable");
            }
        }
    }

    /// Run startup, then the sentinel loop until cancelled, then teardown (SC8
    /// steps 4–5): stop the sentinel, detach the follower, and — only once
    /// `drained` fires — stop the store.
    ///
    /// `shutdown` (the SIGINT/SIGTERM token) stops the sentinel; `drained` is the
    /// gate `main` releases *after* HTTP has drained, so the store is never
    /// stopped out from under an in-flight client (SC8 ordering). If the gate's
    /// sender is dropped, teardown proceeds immediately.
    pub async fn run(mut self, shutdown: CancellationToken, drained: oneshot::Receiver<()>) {
        // Startup failures are already surfaced via the condition (lifecycle /
        // findings); the sentinel below keeps observing either way.
        let _ = self.startup().await;

        let mut interval = tokio::time::interval(self.config.sentinel_interval);
        interval.tick().await; // consume the immediate first tick
        loop {
            tokio::select! {
                _ = shutdown.cancelled() => break,
                _ = interval.tick() => self.reconcile_once().await,
            }
        }

        // Stop observing and detach the follower right away…
        if let Some(follower) = self.follower.take() {
            follower.abort();
        }
        // …but wait for clients to drain before stopping the store (SC8 step 5
        // follows step 2). `Err` means the gate sender dropped — proceed anyway.
        let _ = drained.await;
        self.stop_store().await;
    }

    /// Stop the container, bounded by `stop_timeout` so a hung `podman stop`
    /// cannot wedge shutdown (SC8). The `/nix` volume persists — no data risk.
    async fn stop_store(&self) {
        info!("stopping the store");
        let stop = self.call(|store| store.stop());
        match tokio::time::timeout(self.config.stop_timeout, stop).await {
            Ok(Ok(())) => info!("store stopped"),
            Ok(Err(error)) => warn!(%error, "store stop failed"),
            Err(_) => warn!("store stop timed out; abandoning it"),
        }
    }

    async fn call<T, F>(&self, operation: F) -> Result<T, StoreError>
    where
        T: Send + 'static,
        F: FnOnce(Arc<dyn Store>) -> Result<T, StoreError> + Send + 'static,
    {
        let store = self.store.clone();
        tokio::task::spawn_blocking(move || operation(store))
            .await
            .map_err(|error| StoreError::new(format!("store task failed: {error}")))?
    }
}
