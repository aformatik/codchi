//! C4 acceptance: the typed [`HttpClient`] round-trips against the C3 server
//! over a real Unix socket, exercising all three catalog response shapes (JSON,
//! NDJSON, empty) plus the typed-error path — proving the generic `call<E>` /
//! `mount<E>` plumbing and `impl CodchiService for HttpClient` end to end.

use std::path::PathBuf;

use codchi_api::dto::*;
use codchi_api::events::EventStreamOpts;
use codchi_api::ids::MachineId;
use codchi_api::{ApiError, CodchiService};
use codchi_cli::HttpClient;
use codchi_server::{AppState, build_router};
use futures::StreamExt;
use tokio::net::UnixListener;

/// Spawn a C3 server on a throwaway socket and return a client pointed at it.
/// The server task is detached; the socket file is unlinked on drop of the guard.
async fn start_server() -> (HttpClient, SocketGuard) {
    let socket = std::env::temp_dir().join(format!(
        "codchi-roundtrip-{}-{}.sock",
        std::process::id(),
        // nanosecond clock keeps concurrent tests on distinct sockets.
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    let _ = std::fs::remove_file(&socket);
    let listener = UnixListener::bind(&socket).expect("bind socket");
    let app = build_router(AppState::with_mock());
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    (HttpClient::new(socket.clone()), SocketGuard(socket))
}

struct SocketGuard(PathBuf);
impl Drop for SocketGuard {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

#[tokio::test]
async fn json_request_roundtrips() {
    let (client, _guard) = start_server().await;

    // No path/body, JSON response.
    let status = client.server_status().await.expect("server_status");
    assert_eq!(status.lifecycle, ServerLifecycle::Ready);
    assert_eq!(status.store.state, StoreState::Up);

    // Path param + JSON response.
    let detail = client
        .get_machine(&MachineId("demo".to_owned()))
        .await
        .expect("get_machine");
    assert_eq!(detail.view.id, MachineId("demo".to_owned()));

    // Request body + JSON `JobView` response (the typed-output decode path that
    // the `JobView<O>` serde-bound fix unblocked).
    let job = client
        .create_machine(CreateMachineRequest {
            id: MachineId("demo".to_owned()),
            modules: vec![],
            keep_on_fail: false,
        })
        .await
        .expect("create_machine");
    assert_eq!(job.kind, JobKind::Init);

    // A list response.
    let machines = client.list_machines().await.expect("list_machines");
    assert_eq!(machines.len(), 1);
}

#[tokio::test]
async fn empty_request_roundtrips() {
    let (client, _guard) = start_server().await;

    // 204 / empty-body shape via a path param + body.
    client
        .set_modules(
            &MachineId("demo".to_owned()),
            SetModulesRequest { modules: vec![] },
        )
        .await
        .expect("set_modules");

    // 204 via a UUID path param.
    let job = codchi_api::JobId::new();
    client.cancel_job(&job).await.expect("cancel_job");
}

#[tokio::test]
async fn ndjson_stream_roundtrips() {
    let (client, _guard) = start_server().await;

    let stream = client
        .stream_logs(LogSource::Store, EventStreamOpts::default())
        .await
        .expect("stream_logs");
    let events: Vec<_> = stream.collect().await;
    assert!(!events.is_empty(), "store log stream was empty");
    assert!(
        events.iter().all(|e| e.is_ok()),
        "every streamed line decoded as an Event"
    );
}

#[tokio::test]
async fn validation_error_roundtrips_typed() {
    let (client, _guard) = start_server().await;

    // The mock validates the body's machine id; a reserved-prefix name is
    // rejected as a typed `Validation` error carried over the wire.
    let err = client
        .create_machine(CreateMachineRequest {
            id: MachineId("codchi-machine-x".to_owned()),
            modules: vec![],
            keep_on_fail: false,
        })
        .await
        .expect_err("reserved machine id must be rejected");
    assert!(
        matches!(err, ApiError::Validation { ref field, .. } if field == "id"),
        "expected typed Validation error, got {err:?}"
    );
}

#[tokio::test]
async fn connection_refused_is_structured() {
    // No server at this socket: the client surfaces a typed internal error
    // rather than panicking (the foundation for C5's hang-forever guard).
    let client = HttpClient::new(std::env::temp_dir().join("codchi-nonexistent-xyz.sock"));
    let err = client.server_status().await.expect_err("must fail");
    assert!(matches!(err, ApiError::Internal { .. }), "got {err:?}");
}
