//! The shared [`AppState`] handle (doc 01).

use std::sync::{Arc, RwLock};

use chrono::Utc;
use codchi_api::dto::{Component, Finding, FindingCode, Severity, StoreState, StoreStatus};
use codchi_api::testing::MockCodchiService;
use codchi_api::{ApiError, CodchiService, FindingId};

use crate::lifecycle::LifecycleHandle;
use crate::logs::LogStore;

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
    /// Cheap in-memory store/finding snapshot maintained by infrastructure.
    pub infrastructure: InfrastructureHandle,
    /// Server-owned `Server`/`Store` source logs (the C7 seam). `Machine` logs
    /// stay on `service` until Phase 7; the router branches on the source.
    pub logs: LogStore,
}

impl AppState {
    /// Build state from a service implementation and the server-owned log store.
    /// `main` passes the same [`LogStore`] the [`crate::logging`] layer writes
    /// to, so the `Server` source and the daemon's stderr share one emit path.
    pub fn new(service: Arc<dyn CodchiService>, logs: LogStore) -> Self {
        AppState {
            service,
            lifecycle: LifecycleHandle::new(),
            infrastructure: InfrastructureHandle::new(),
            logs,
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
    ///
    /// The `Server`/`Store` source logs are seeded with a couple of demo lines
    /// so a mock-backed `codchi logs store` (and the NDJSON shape tests) have
    /// something to stream; the real daemon fills these from the tracing layer
    /// and the store-output capture instead.
    pub fn with_mock() -> Self {
        use codchi_api::{LogLevel, LogSource};
        let state = Self::new(Arc::new(MockCodchiService::new()), LogStore::memory());
        state.infrastructure.store_up();
        state.lifecycle.set(codchi_api::dto::ServerLifecycle::Ready);
        state.logs.append(
            LogSource::Server,
            LogLevel::Info,
            "main",
            "mock daemon ready",
        );
        state.logs.append(
            LogSource::Store,
            LogLevel::Info,
            "store",
            "store container started",
        );
        state
            .logs
            .append(LogSource::Store, LogLevel::Info, "gc", "store gc complete");
        state
    }
}

#[derive(Clone, Debug)]
pub struct InfrastructureHandle(Arc<RwLock<InfrastructureSnapshot>>);

#[derive(Clone, Debug)]
pub struct InfrastructureSnapshot {
    pub store: StoreStatus,
    pub findings: Vec<Finding>,
    pub startup_error: Option<ApiError>,
}

impl InfrastructureHandle {
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(InfrastructureSnapshot {
            store: StoreStatus {
                state: StoreState::Unknown,
                last_checked_at: None,
                last_error: None,
            },
            findings: Vec::new(),
            startup_error: None,
        })))
    }

    pub fn snapshot(&self) -> InfrastructureSnapshot {
        self.0.read().expect("infrastructure lock poisoned").clone()
    }

    pub fn store_recovering(&self) {
        let mut state = self.0.write().expect("infrastructure lock poisoned");
        state.store.state = StoreState::Recovering;
        state.store.last_error = None;
        state.startup_error = None;
    }

    pub fn store_up(&self) {
        let mut state = self.0.write().expect("infrastructure lock poisoned");
        state.store = StoreStatus {
            state: StoreState::Up,
            last_checked_at: Some(Utc::now()),
            last_error: None,
        };
        state
            .findings
            .retain(|finding| finding.code != FindingCode::StoreUnavailable);
        state.startup_error = None;
    }

    pub fn store_down(&self, reason: String) {
        let mut state = self.0.write().expect("infrastructure lock poisoned");
        state.store = StoreStatus {
            state: StoreState::Down,
            last_checked_at: Some(Utc::now()),
            last_error: Some(reason.clone()),
        };
        state.startup_error = Some(ApiError::StoreUnavailable {
            reason: reason.clone(),
        });

        if let Some(finding) = state
            .findings
            .iter_mut()
            .find(|finding| finding.code == FindingCode::StoreUnavailable)
        {
            finding.message = reason;
            return;
        }
        state.findings.push(Finding {
            id: FindingId::new(),
            severity: Severity::Error,
            component: Component::Store,
            machine: None,
            source_job: None,
            code: FindingCode::StoreUnavailable,
            message: reason,
            suggested_action: Some("Run `codchi doctor` for details.".to_owned()),
            auto_fixable: false,
            created_at: Utc::now(),
        });
    }

    pub fn store_unavailable(&self) -> bool {
        self.0
            .read()
            .expect("infrastructure lock poisoned")
            .findings
            .iter()
            .any(|finding| finding.code == FindingCode::StoreUnavailable)
    }
}

impl Default for InfrastructureHandle {
    fn default() -> Self {
        Self::new()
    }
}
