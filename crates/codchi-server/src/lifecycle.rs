//! Server lifecycle state — the readiness seam.
//!
//! D7/D8: readiness is *server-infrastructure* state held in [`AppState`], not a
//! [`CodchiService`] method, so handlers stay thin. The Phase 1 subset of the
//! lifecycle is `Starting → Healthcheck → Ready/Degraded` (no `Migrating` until
//! SQLite in Phase 3; `Stopping` is minimal).
//!
//! C6 makes this handle load-bearing: the store manager drives it from the real
//! Podman store bring-up and the readiness endpoint reports it rather than the
//! mock service's canned lifecycle.
//!
//! [`AppState`]: crate::AppState
//! [`CodchiService`]: codchi_api::CodchiService

use std::sync::{Arc, RwLock};

use codchi_api::dto::ServerLifecycle;

/// A cheap, cloneable handle to the daemon's current [`ServerLifecycle`] state.
#[derive(Clone, Debug)]
pub struct LifecycleHandle(Arc<RwLock<ServerLifecycle>>);

impl LifecycleHandle {
    /// A fresh handle in the initial `Starting` state.
    pub fn new() -> Self {
        LifecycleHandle(Arc::new(RwLock::new(ServerLifecycle::Starting)))
    }

    /// The current lifecycle state.
    pub fn current(&self) -> ServerLifecycle {
        *self.0.read().expect("lifecycle lock poisoned")
    }

    /// Transition to a new lifecycle state.
    pub fn set(&self, state: ServerLifecycle) {
        *self.0.write().expect("lifecycle lock poisoned") = state;
    }
}

impl Default for LifecycleHandle {
    fn default() -> Self {
        Self::new()
    }
}
