//! C3 acceptance: the generic `mount<E>` machinery registers **every** entry in
//! the `codchi-api` `ROUTES` catalog. Mirrors the OpenAPI-coverage test in
//! `codchi-api`, but for the live axum router.
//!
//! For each route we drive one in-process request (`tower`'s `oneshot`, no
//! socket needed) with the route's method and assert the response is **not**
//! `404 Not Found` — i.e. the path+method matched a handler. Parse/validation
//! failures (e.g. a non-UUID job id, or an empty body where one is required)
//! surface as `400`, which still proves the route is mounted.

use axum::body::Body;
use axum::http::{Method as HttpMethod, Request, StatusCode};
use codchi_api::endpoints::{Method, ROUTES};
use codchi_server::{AppState, build_router};
use tower::ServiceExt;

fn http_method(method: Method) -> HttpMethod {
    match method {
        Method::Get => HttpMethod::GET,
        Method::Post => HttpMethod::POST,
        Method::Delete => HttpMethod::DELETE,
    }
}

/// Fill every `{param}` hole with a placeholder segment. The exact value is
/// irrelevant to coverage: a real id yields 200/204, a malformed one yields a
/// 400 from the typed path parse — neither is the 404 we are ruling out.
fn fill_path(template: &str) -> String {
    let mut out = String::with_capacity(template.len());
    let mut rest = template;
    while let Some((before, after)) = rest.split_once('{') {
        let (_, after) = after.split_once('}').expect("balanced braces");
        out.push_str(before);
        out.push_str("demo");
        rest = after;
    }
    out.push_str(rest);
    out
}

#[tokio::test]
async fn router_covers_all_routes() {
    assert_eq!(
        ROUTES.len(),
        28,
        "catalog size changed; update coverage test"
    );

    for route in ROUTES {
        let app = build_router(AppState::with_mock());
        let request = Request::builder()
            .method(http_method(route.method))
            .uri(fill_path(route.path))
            .body(Body::empty())
            .expect("valid request");

        let response = app.oneshot(request).await.expect("router responds");

        assert_ne!(
            response.status(),
            StatusCode::NOT_FOUND,
            "{} {} ({}) is not mounted",
            route.method.as_str(),
            route.path,
            route.operation_id,
        );
    }
}

/// C3 acceptance: `GET /v1/server` serializes `ServerStatus` (the JSON shape),
/// and the three response shapes (JSON / NDJSON / empty) all serialize over the
/// router. Driven in-process; the socket transport is exercised by the
/// `codchi-cli` round-trip test.
#[tokio::test]
async fn response_shapes_serialize() {
    use http_body_util::BodyExt;

    // JSON: GET /v1/server.
    let app = build_router(AppState::with_mock());
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/v1/server")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
    assert_eq!(resp.headers()["content-type"], "application/json");
    let bytes = resp.into_body().collect().await.unwrap().to_bytes();
    let status: codchi_api::dto::ServerStatus = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(status.lifecycle, codchi_api::dto::ServerLifecycle::Ready);

    // Empty: POST /v1/jobs/<uuid>/cancel returns 204 with no body.
    let job = codchi_api::JobId::new();
    let app = build_router(AppState::with_mock());
    let resp = app
        .oneshot(
            Request::builder()
                .method(HttpMethod::POST)
                .uri(format!("/v1/jobs/{job}/cancel"))
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::NO_CONTENT);
    let bytes = resp.into_body().collect().await.unwrap().to_bytes();
    assert!(bytes.is_empty());

    // NDJSON: GET /v1/logs/store streams newline-delimited events.
    let app = build_router(AppState::with_mock());
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/v1/logs/store")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
    assert_eq!(resp.headers()["content-type"], "application/x-ndjson");
    let bytes = resp.into_body().collect().await.unwrap().to_bytes();
    let lines: Vec<&[u8]> = bytes
        .split(|&b| b == b'\n')
        .filter(|l| !l.is_empty())
        .collect();
    assert!(!lines.is_empty(), "ndjson stream had no lines");
    for line in lines {
        serde_json::from_slice::<codchi_api::Event>(line).expect("each line is an Event");
    }
}
