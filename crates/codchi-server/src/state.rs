//! The shared [`AppState`] handle (doc 01 / SC1).
//!
//! Phase 2 collapses the Phase-1 four-field state to a single `core` handle: the
//! [`ServerCore`] *is* the service and owns everything the router used to
//! assemble by hand (lifecycle/store projection, findings, the `LogStore`). The
//! router becomes a thin adapter over it.

use std::sync::Arc;

use chrono::Utc;
use codchi_api::dto::ServerLifecycle;
use tokio::sync::watch;
use tokio_util::sync::CancellationToken;

use crate::core::{ServerCore, StoreCondition};
use crate::logs::LogStore;

/// The single shared state handle threaded through every handler: just the
/// [`ServerCore`] that implements the whole `CodchiService` (SC1). Cheap to
/// [`Clone`] (an `Arc`), as axum's `State` requires.
#[derive(Clone)]
pub struct AppState {
    pub core: Arc<ServerCore>,
}

impl AppState {
    /// Wrap a constructed [`ServerCore`] (the real `main` path).
    pub fn new(core: Arc<ServerCore>) -> Self {
        AppState { core }
    }

    /// A mock-backed state reporting `Ready`/store-`Up`, for the transport tests
    /// and a mock-backed `codchi status` (D7). Machine data is the
    /// [`MockCodchiService`](codchi_api::testing::MockCodchiService) fixtures.
    pub fn with_mock() -> Self {
        Self::with_mock_lifecycle(ServerLifecycle::Ready)
    }

    /// A mock-backed state whose projected lifecycle is forced to `lifecycle`,
    /// for the C5 readiness-guard tests. Implemented by seeding the underlying
    /// [`StoreCondition`] the lifecycle projects from (SC8) — there is no
    /// lifecycle *setter* (SC6); the projection is the only path.
    ///
    /// Only the lifecycles a `StoreCondition` can produce are supported;
    /// `Migrating`/`Stopping` have no Phase-2 condition and panic.
    pub fn with_mock_lifecycle(lifecycle: ServerLifecycle) -> Self {
        let condition = match lifecycle {
            ServerLifecycle::Ready => StoreCondition::Up { since: Utc::now() },
            ServerLifecycle::Degraded => StoreCondition::Degraded {
                reason: "mock store unavailable".to_owned(),
                since: Utc::now(),
            },
            ServerLifecycle::Starting => StoreCondition::Starting,
            ServerLifecycle::Healthcheck => StoreCondition::Checking,
            ServerLifecycle::Migrating | ServerLifecycle::Stopping => {
                panic!("{lifecycle:?} is not representable as a StoreCondition in Phase 2")
            }
        };
        Self::mock_with_condition(condition)
    }

    fn mock_with_condition(condition: StoreCondition) -> Self {
        use codchi_api::{LogLevel, LogSource};

        // No supervisor backs the mock, so the sender is dropped immediately;
        // `watch` retains the last value for `borrow()`-time projection.
        let (_tx, condition) = watch::channel(condition);

        let logs = LogStore::memory();
        logs.append(
            LogSource::Server,
            LogLevel::Info,
            "main",
            "mock daemon ready",
        );
        logs.append(
            LogSource::Store,
            LogLevel::Info,
            "store",
            "store container started",
        );
        logs.append(LogSource::Store, LogLevel::Info, "gc", "store gc complete");

        let core = ServerCore::new(logs, condition, CancellationToken::new());
        AppState {
            core: Arc::new(core),
        }
    }
}
