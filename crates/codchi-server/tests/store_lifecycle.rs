use std::sync::{Arc, Mutex};
use std::time::Duration;

use axum::body::Body;
use axum::http::{Request, StatusCode};
use codchi_api::ApiError;
use codchi_api::dto::{DoctorReport, FindingCode, ServerLifecycle, ServerStatus, StoreState};
use codchi_api::testing::MockCodchiService;
use codchi_server::{
    AppState, LogStore, Store, StoreError, StoreLogStream, StoreManager, StoreManagerConfig,
    StorePlatformStatus, build_router,
};
use http_body_util::BodyExt;
use tower::ServiceExt;

#[derive(Default)]
struct FakeStore {
    calls: Mutex<Vec<&'static str>>,
    status: Mutex<StorePlatformStatus>,
    health_error: Mutex<Option<String>>,
}

impl FakeStore {
    fn with_status(status: StorePlatformStatus) -> Self {
        Self {
            status: Mutex::new(status),
            ..Self::default()
        }
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
        // The fake has no process to follow; an empty stream ends immediately.
        Ok(StoreLogStream::Empty)
    }
}

fn test_config() -> StoreManagerConfig {
    StoreManagerConfig {
        startup_timeout: Duration::from_millis(20),
        probe_interval: Duration::from_millis(1),
        sentinel_interval: Duration::from_secs(15),
    }
}

fn test_state() -> AppState {
    AppState::new(Arc::new(MockCodchiService::new()), LogStore::memory())
}

async fn get_json<T: serde::de::DeserializeOwned>(state: AppState, path: &str) -> T {
    let response = build_router(state)
        .oneshot(Request::builder().uri(path).body(Body::empty()).unwrap())
        .await
        .unwrap();
    assert_eq!(response.status(), StatusCode::OK);
    let body = response.into_body().collect().await.unwrap().to_bytes();
    serde_json::from_slice(&body).unwrap()
}

#[tokio::test]
async fn missing_store_is_registered_started_and_reported_ready() {
    let store = Arc::new(FakeStore::with_status(StorePlatformStatus::NotInstalled));
    let state = test_state();
    let manager = StoreManager::new(store.clone(), state.clone(), test_config());

    manager.start().await.expect("store starts");

    assert_eq!(
        store.calls(),
        ["status", "register", "start", "probe_health", "attach"]
    );

    let status: ServerStatus = get_json(state.clone(), "/v1/server").await;
    assert_eq!(status.lifecycle, ServerLifecycle::Ready);
    assert_eq!(status.store.state, StoreState::Up);
    assert!(status.store.last_checked_at.is_some());
    assert_eq!(status.store.last_error, None);
    assert_eq!(status.startup_error, None);
    assert_eq!(status.findings_summary.error, 0);

    let report: DoctorReport = get_json(state, "/v1/doctor").await;
    assert!(report.findings.is_empty());
}

#[tokio::test]
async fn existing_stopped_store_is_started_without_reregister() {
    // The common case after a reboot: the container already exists, so startup
    // must `start` it but never `register` (which would recreate it and is only
    // for first install / update).
    let store = Arc::new(FakeStore::with_status(StorePlatformStatus::Stopped));
    let state = test_state();
    let manager = StoreManager::new(store.clone(), state.clone(), test_config());

    manager.start().await.expect("store starts");

    assert_eq!(store.calls(), ["status", "start", "probe_health", "attach"]);
    let status: ServerStatus = get_json(state, "/v1/server").await;
    assert_eq!(status.lifecycle, ServerLifecycle::Ready);
    assert_eq!(status.store.state, StoreState::Up);
}

#[tokio::test]
async fn already_running_store_is_only_health_probed() {
    // An already-running store (the server restarted while the container kept
    // running) is neither re-registered nor restarted — startup just confirms
    // it is healthy.
    let store = Arc::new(FakeStore::with_status(StorePlatformStatus::Running));
    let state = test_state();
    let manager = StoreManager::new(store.clone(), state.clone(), test_config());

    manager.start().await.expect("store starts");

    assert_eq!(store.calls(), ["status", "probe_health", "attach"]);
    let status: ServerStatus = get_json(state, "/v1/server").await;
    assert_eq!(status.lifecycle, ServerLifecycle::Ready);
    assert_eq!(status.store.state, StoreState::Up);
}

#[tokio::test]
async fn unavailable_store_degrades_readiness_and_creates_a_finding() {
    let store = Arc::new(FakeStore::with_status(StorePlatformStatus::Running));
    store.set_health_error(Some("nix daemon did not answer"));
    let state = test_state();
    let manager = StoreManager::new(store, state.clone(), test_config());

    manager.start().await.expect_err("health check must fail");

    let status: ServerStatus = get_json(state.clone(), "/v1/server").await;
    assert_eq!(status.lifecycle, ServerLifecycle::Degraded);
    assert_eq!(status.store.state, StoreState::Down);
    assert_eq!(
        status.store.last_error.as_deref(),
        Some("nix daemon did not answer")
    );
    assert!(matches!(
        status.startup_error,
        Some(ApiError::StoreUnavailable { ref reason })
            if reason == "nix daemon did not answer"
    ));
    assert_eq!(status.findings_summary.error, 1);

    let report: DoctorReport = get_json(state, "/v1/doctor").await;
    assert_eq!(report.findings.len(), 1);
    assert_eq!(report.findings[0].code, FindingCode::StoreUnavailable);
}

#[tokio::test]
async fn sentinel_marks_a_lost_store_down_and_clears_on_recovery() {
    let store = Arc::new(FakeStore::with_status(StorePlatformStatus::Running));
    let state = test_state();
    let manager = StoreManager::new(store.clone(), state.clone(), test_config());
    manager.start().await.expect("initial health check");

    store.set_health_error(Some("store stopped"));
    manager.reconcile_once().await;
    let down: ServerStatus = get_json(state.clone(), "/v1/server").await;
    assert_eq!(down.lifecycle, ServerLifecycle::Degraded);
    assert_eq!(down.store.state, StoreState::Down);
    assert_eq!(down.findings_summary.error, 1);

    store.set_health_error(None);
    manager.reconcile_once().await;
    let recovered: ServerStatus = get_json(state, "/v1/server").await;
    assert_eq!(recovered.lifecycle, ServerLifecycle::Ready);
    assert_eq!(recovered.store.state, StoreState::Up);
    assert_eq!(recovered.findings_summary.error, 0);
    assert_eq!(recovered.startup_error, None);
}
