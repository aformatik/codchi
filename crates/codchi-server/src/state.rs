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
    pub fn with_mock() -> Self {
        Self::new(Arc::new(MockCodchiService::new()))
    }
}
