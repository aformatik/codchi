//! SC9 supervisor invariants over a fake `Store`, one test per invariant. These
//! defend the README invariants a parallel-agent refactor must not regress:
//! no-hang to `Degraded` (D8), observe-only sentinel (P6), Store-Authority /
//! no-orphans, graceful store stop (SC8), and diagnosability.
//!
//! Assertions are on the published [`StoreCondition`] and the fake's recorded
//! calls — the supervisor's own surface. The pure `step`/projection matrix is
//! unit-tested inside `core::condition`; the projection-join through the real
//! HTTP handler is exercised once here (diagnosability) and by the C3/C4 tests.

use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use axum::body::Body;
use axum::http::{Request, StatusCode};
use codchi_api::dto::{DoctorReport, FindingCode, ServerLifecycle, ServerStatus, StoreState};
use codchi_server::{
    AppState, LogStore, MAX_SCHEMA_VERSION, SchemaState, ServerCore, Store, StoreCondition,
    StoreError, StoreLogStream, StorePlatformStatus, StoreSupervisor, StoreSupervisorConfig,
    build_router,
};
use http_body_util::BodyExt;
use tokio::sync::{oneshot, watch};
use tokio_util::sync::CancellationToken;
use tower::ServiceExt;

#[derive(Default)]
struct FakeStore {
    calls: Mutex<Vec<&'static str>>,
    status: Mutex<StorePlatformStatus>,
    health_error: Mutex<Option<String>>,
}

impl FakeStore {
    fn with_status(status: StorePlatformStatus) -> Arc<Self> {
        Arc::new(Self {
            status: Mutex::new(status),
            ..Self::default()
        })
    }

    fn calls(&self) -> Vec<&'static str> {
        self.calls.lock().unwrap().clone()
    }

    fn set_health_error(&self, error: Option<&str>) {
        *self.health_error.lock().unwrap() = error.map(str::to_owned);
    }
}

impl Store for FakeStore {
    fn status(&self) -> Result<StorePlatformStatus, StoreError> {
        self.calls.lock().unwrap().push("status");
        Ok(*self.status.lock().unwrap())
    }

    fn register(&self) -> Result<(), StoreError> {
        self.calls.lock().unwrap().push("register");
        *self.status.lock().unwrap() = StorePlatformStatus::Stopped;
        Ok(())
    }

    fn start(&self) -> Result<(), StoreError> {
        self.calls.lock().unwrap().push("start");
        *self.status.lock().unwrap() = StorePlatformStatus::Running;
        Ok(())
    }

    fn probe_health(&self) -> Result<(), StoreError> {
        self.calls.lock().unwrap().push("probe_health");
        match self.health_error.lock().unwrap().clone() {
            Some(message) => Err(StoreError::new(message)),
            None => Ok(()),
        }
    }

    fn stop(&self) -> Result<(), StoreError> {
        self.calls.lock().unwrap().push("stop");
        *self.status.lock().unwrap() = StorePlatformStatus::Stopped;
        Ok(())
    }

    fn attach(&self) -> Result<StoreLogStream, StoreError> {
        self.calls.lock().unwrap().push("attach");
        Ok(StoreLogStream::Empty)
    }
}

fn fast_config() -> StoreSupervisorConfig {
    StoreSupervisorConfig {
        startup_timeout: Duration::from_millis(20),
        probe_interval: Duration::from_millis(1),
        sentinel_interval: Duration::from_secs(15),
        stop_timeout: Duration::from_secs(5),
    }
}

/// A supervisor over `store`, plus a receiver on the condition it publishes.
fn supervise(store: Arc<FakeStore>) -> (StoreSupervisor, watch::Receiver<StoreCondition>) {
    let (tx, rx) = watch::channel(StoreCondition::Starting);
    let supervisor = StoreSupervisor::new(store, tx, LogStore::memory(), fast_config());
    (supervisor, rx)
}

fn condition(rx: &watch::Receiver<StoreCondition>) -> StoreCondition {
    rx.borrow().clone()
}

// ---- Store Authority / no-orphans: idempotent restart ----

#[tokio::test]
async fn missing_store_is_registered_started_and_reaches_up() {
    let store = FakeStore::with_status(StorePlatformStatus::NotInstalled);
    let (mut supervisor, rx) = supervise(store.clone());

    supervisor.startup().await.expect("store starts");

    assert_eq!(
        store.calls(),
        ["status", "register", "start", "probe_health", "attach"]
    );
    assert!(matches!(condition(&rx), StoreCondition::Up { .. }));
}

#[tokio::test]
async fn existing_stopped_store_is_started_without_reregister() {
    // The common case after a reboot: the container exists, so startup `start`s
    // it but never `register`s (which would recreate it — only for install/update).
    let store = FakeStore::with_status(StorePlatformStatus::Stopped);
    let (mut supervisor, rx) = supervise(store.clone());

    supervisor.startup().await.expect("store starts");

    assert_eq!(store.calls(), ["status", "start", "probe_health", "attach"]);
    assert!(matches!(condition(&rx), StoreCondition::Up { .. }));
}

#[tokio::test]
async fn already_running_store_is_only_probed() {
    let store = FakeStore::with_status(StorePlatformStatus::Running);
    let (mut supervisor, rx) = supervise(store.clone());

    supervisor.startup().await.expect("store starts");

    assert_eq!(store.calls(), ["status", "probe_health", "attach"]);
    assert!(matches!(condition(&rx), StoreCondition::Up { .. }));
}

// ---- D8: no hang — an always-unhealthy store reaches Degraded by the deadline ----

#[tokio::test]
async fn always_unhealthy_store_reaches_degraded_within_deadline() {
    let store = FakeStore::with_status(StorePlatformStatus::Running);
    store.set_health_error(Some("nix daemon did not answer"));
    let (mut supervisor, rx) = supervise(store.clone());

    let started = Instant::now();
    supervisor
        .startup()
        .await
        .expect_err("health check must fail");
    let elapsed = started.elapsed();

    assert!(
        elapsed < Duration::from_secs(2),
        "startup overran its bounded deadline: {elapsed:?}"
    );
    assert!(matches!(
        condition(&rx),
        StoreCondition::Degraded { ref reason, .. } if reason == "nix daemon did not answer"
    ));
}

// ---- P6: the post-startup sentinel only observes — never register/start ----

#[tokio::test]
async fn sentinel_never_registers_or_starts_after_startup() {
    let store = FakeStore::with_status(StorePlatformStatus::Running);
    let (mut supervisor, rx) = supervise(store.clone());
    supervisor.startup().await.expect("store starts");

    // Drive several sentinel ticks across a health blip, including while degraded.
    supervisor.reconcile_once().await; // healthy
    store.set_health_error(Some("store stopped"));
    supervisor.reconcile_once().await; // -> Degraded
    supervisor.reconcile_once().await; // still Degraded
    store.set_health_error(None);
    supervisor.reconcile_once().await; // -> Up

    // After the startup prefix, every sentinel call is a bare probe: recovery is
    // observed, never driven (no register/start).
    let post_startup = &store.calls()[3..]; // skip status/probe_health/attach
    assert!(
        post_startup.iter().all(|&c| c == "probe_health"),
        "sentinel did more than probe: {post_startup:?}"
    );
    assert!(matches!(condition(&rx), StoreCondition::Up { .. }));
}

// ---- SC8: cancellation stops the store and terminates within the timeout ----

#[tokio::test]
async fn cancel_stops_the_store_and_terminates() {
    let store = FakeStore::with_status(StorePlatformStatus::Running);
    let (supervisor, _rx) = supervise(store.clone());
    let token = CancellationToken::new();
    let (drained_tx, drained_rx) = oneshot::channel();

    let handle = tokio::spawn(supervisor.run(token.clone(), drained_rx));
    // Let startup settle, request shutdown, then release the teardown gate (as
    // `main` does once HTTP has drained).
    tokio::time::sleep(Duration::from_millis(20)).await;
    token.cancel();
    let _ = drained_tx.send(());

    tokio::time::timeout(Duration::from_secs(5), handle)
        .await
        .expect("supervisor terminates within the shutdown bound")
        .expect("supervisor task did not panic");

    assert!(
        store.calls().contains(&"stop"),
        "shutdown must stop the store: {:?}",
        store.calls()
    );
}

// ---- diagnosability: bring-up failure surfaces a finding; clears on recovery ----

#[tokio::test]
async fn bringup_failure_surfaces_finding_and_clears_on_recovery() {
    let store = FakeStore::with_status(StorePlatformStatus::Running);
    store.set_health_error(Some("nix daemon did not answer"));
    let (mut supervisor, rx) = supervise(store.clone());

    // The ServerCore projects the same condition the supervisor publishes; the
    // schema is healthy so the lifecycle join reflects the store alone (DB8).
    let core = Arc::new(ServerCore::new(
        LogStore::memory(),
        rx,
        CancellationToken::new(),
        None,
        SchemaState::Ready,
        MAX_SCHEMA_VERSION,
    ));
    let state = AppState::new(core);

    supervisor.startup().await.expect_err("health check fails");

    let status: ServerStatus = get_json(&state, "/v1/server").await;
    assert_eq!(status.lifecycle, ServerLifecycle::Degraded);
    assert_eq!(status.store.state, StoreState::Down);
    assert_eq!(
        status.store.last_error.as_deref(),
        Some("nix daemon did not answer")
    );
    assert!(status.startup_error.is_some());
    assert_eq!(status.findings_summary.error, 1);

    let report: DoctorReport = get_json(&state, "/v1/doctor").await;
    assert_eq!(report.findings.len(), 1);
    assert_eq!(report.findings[0].code, FindingCode::StoreUnavailable);

    // Recovery clears the finding and returns to Ready.
    store.set_health_error(None);
    supervisor.reconcile_once().await;

    let recovered: ServerStatus = get_json(&state, "/v1/server").await;
    assert_eq!(recovered.lifecycle, ServerLifecycle::Ready);
    assert_eq!(recovered.store.state, StoreState::Up);
    assert_eq!(recovered.startup_error, None);
    assert_eq!(recovered.findings_summary.error, 0);
}

async fn get_json<T: serde::de::DeserializeOwned>(state: &AppState, path: &str) -> T {
    let response = build_router(state.clone())
        .oneshot(Request::builder().uri(path).body(Body::empty()).unwrap())
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    let body = response.into_body().collect().await.unwrap().to_bytes();
    serde_json::from_slice(&body).unwrap()
}
