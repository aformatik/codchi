//! Server-owned store startup and observe-only health sentinel.

use std::sync::Arc;
use std::time::Duration;

use codchi_api::dto::ServerLifecycle;
use tracing::{debug, error, info, warn};

use crate::logs::capture_store_logs;
use crate::platform::{Store, StoreError, StorePlatformStatus};
use crate::state::AppState;

/// Store lifecycle timings. Tests use short values; production keeps the
/// sentinel cadence locked by P6 and gives first-time Nix setup a bounded wait.
#[derive(Clone, Copy, Debug)]
pub struct StoreManagerConfig {
    pub startup_timeout: Duration,
    pub probe_interval: Duration,
    pub sentinel_interval: Duration,
}

impl Default for StoreManagerConfig {
    fn default() -> Self {
        Self {
            startup_timeout: Duration::from_secs(5 * 60),
            probe_interval: Duration::from_millis(250),
            sentinel_interval: Duration::from_secs(15),
        }
    }
}

/// Owns store startup and the post-startup observe-only sentinel.
pub struct StoreManager {
    store: Arc<dyn Store>,
    state: AppState,
    config: StoreManagerConfig,
}

impl StoreManager {
    pub fn new(store: Arc<dyn Store>, state: AppState, config: StoreManagerConfig) -> Self {
        Self {
            store,
            state,
            config,
        }
    }

    /// Bring the store to a healthy running state and publish the result.
    pub async fn start(&self) -> Result<(), StoreError> {
        info!("bringing up the store");
        self.state.lifecycle.set(ServerLifecycle::Starting);
        self.state.infrastructure.store_recovering();

        let result = self.start_inner().await;
        match result {
            Ok(()) => {
                info!("store is ready");
                self.state.infrastructure.store_up();
                self.state.lifecycle.set(ServerLifecycle::Ready);
                self.spawn_log_capture();
                Ok(())
            }
            Err(error) => {
                error!(%error, "store startup failed; entering degraded");
                self.state.infrastructure.store_down(error.to_string());
                self.state.lifecycle.set(ServerLifecycle::Degraded);
                Err(error)
            }
        }
    }

    async fn start_inner(&self) -> Result<(), StoreError> {
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

        self.state.lifecycle.set(ServerLifecycle::Healthcheck);
        info!("waiting for the store's nix-daemon to answer");
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

    /// Attach to the now-running store and stream its output into the `Store`
    /// source log. Called once, on the successful-start path (C7). Attaching is
    /// best-effort: if the follower can't be opened the store still runs, we
    /// just have no store logs this run.
    fn spawn_log_capture(&self) {
        match self.store.attach() {
            Ok(stream) => {
                tokio::spawn(capture_store_logs(stream, self.state.logs.clone()));
            }
            Err(error) => {
                warn!(%error, "could not attach to store output; store logs unavailable");
            }
        }
    }

    /// Run one lightweight sentinel probe and update the in-memory snapshot.
    pub async fn reconcile_once(&self) {
        match self.call(|store| store.probe_health()).await {
            Ok(()) => {
                let store_was_unavailable = self.state.infrastructure.store_unavailable();
                self.state.infrastructure.store_up();
                if store_was_unavailable
                    && self.state.lifecycle.current() == ServerLifecycle::Degraded
                {
                    info!("store recovered; back to ready");
                    self.state.lifecycle.set(ServerLifecycle::Ready);
                }
            }
            Err(error) => {
                if !self.state.infrastructure.store_unavailable() {
                    warn!(%error, "store became unavailable");
                }
                self.state.infrastructure.store_down(error.to_string());
                self.state.lifecycle.set(ServerLifecycle::Degraded);
            }
        }
    }

    /// Start the store, then keep observing it every 15 seconds. Recovery is
    /// observed but never attempted after startup (P6).
    pub async fn run(self) {
        // Startup failures are already logged (and surfaced via the lifecycle /
        // findings) inside `start`; the sentinel below keeps observing either way.
        let _ = self.start().await;
        let mut interval = tokio::time::interval(self.config.sentinel_interval);
        interval.tick().await;
        loop {
            interval.tick().await;
            self.reconcile_once().await;
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
