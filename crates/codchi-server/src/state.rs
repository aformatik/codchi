//! The shared [`AppState`] handle (doc 01).

use std::sync::Arc;

use codchi_api::CodchiService;
use codchi_api::testing::MockCodchiService;

use crate::lifecycle::LifecycleHandle;

/// The single shared state handle threaded through every handler.
///
/// Per doc 01 / D7 it carries two things: the semantic service the handlers
/// dispatch into (`Arc<dyn CodchiService>` — `MockCodchiService` in Phase 1,
/// swapped for the real `ServerCore` in Phase 2), and the server-infrastructure
/// [`LifecycleHandle`] (readiness is *not* a service method).
#[derive(Clone)]
pub struct AppState {
    /// The semantic service the typed routes dispatch into.
    pub service: Arc<dyn CodchiService>,
    /// Server-owned readiness/lifecycle state (the C6 seam).
    pub lifecycle: LifecycleHandle,
}

impl AppState {
    /// Build state from any service implementation.
    pub fn new(service: Arc<dyn CodchiService>) -> Self {
        AppState {
            service,
            lifecycle: LifecycleHandle::new(),
        }
    }

    /// The Phase 1 wiring: machine-data endpoints are served by
    /// [`MockCodchiService`] (D7). Real machine data arrives with `ServerCore`
    /// in Phase 2.
    ///
    /// A mock-backed daemon has no real store to bring up, so it is `Ready`
    /// immediately — the readiness endpoint overlays this (D7), so transport
    /// tests and a mock-backed `codchi status` both observe `Ready` without the
    /// caller flipping the handle. The real `main` keeps the honest
    /// `Starting → Ready` progression via [`AppState::new`].
    pub fn with_mock() -> Self {
        let state = Self::new(Arc::new(MockCodchiService::new()));
        state.lifecycle.set(codchi_api::dto::ServerLifecycle::Ready);
        state
    }
}
