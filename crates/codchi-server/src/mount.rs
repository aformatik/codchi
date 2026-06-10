//! Generic, `Endpoint`-driven route registration (doc 06).
//!
//! The payoff of the typed catalog: the wire plumbing is written **once**,
//! generic over `E: Endpoint`, instead of one hand-written axum handler per
//! route. There is a registration helper per response shape — [`mount_json`],
//! [`mount_ndjson`], [`mount_empty`] — each of which builds the route at
//! `E::PATH` for `E::METHOD`, extracts the typed path params (`E::Path`), query
//! (`E::Query`), and JSON body (`E::Body`), hands them to the per-route glue
//! closure (the residual mapping into a [`CodchiService`] method that Rust
//! cannot auto-derive — doc 06), and serialises the result per `E::RESPONSE`.
//!
//! The transport-neutral path render/parse lives in `codchi-api`
//! ([`PathParams`]); axum supplies the matched segment values in template order
//! via [`RawPathParams`], so the server hands them straight through without
//! `codchi-api` ever depending on axum.
//!
//! [`CodchiService`]: codchi_api::CodchiService

use std::future::Future;

use axum::Router;
use axum::body::{Body, to_bytes};
use axum::extract::{FromRequestParts, RawPathParams, Request, State};
use axum::http::header::CONTENT_TYPE;
use axum::response::Response;
use axum::routing::{MethodFilter, on};
use bytes::Bytes;
use codchi_api::service::EventStream;
use codchi_api::{ApiError, Endpoint, Method, PathParams};
use futures::StreamExt;
use serde::Serialize;
use serde::de::DeserializeOwned;

use crate::state::AppState;

/// Cap on request body size, guarding the daemon against an unbounded upload.
const MAX_BODY_BYTES: usize = 4 * 1024 * 1024;

const JSON: &str = "application/json";
const NDJSON: &str = "application/x-ndjson";

/// Register a JSON-returning endpoint: status 200 + `application/json` body of
/// `E::Response` on success, the typed [`ApiError`] otherwise.
pub fn mount_json<E, F, Fut>(router: Router<AppState>, glue: F) -> Router<AppState>
where
    E: Endpoint + 'static,
    E::Path: Send,
    E::Query: DeserializeOwned + Send,
    E::Body: DeserializeOwned + Send,
    E::Response: Serialize,
    F: Fn(AppState, E::Path, E::Query, E::Body) -> Fut + Clone + Send + Sync + 'static,
    Fut: Future<Output = Result<E::Response, ApiError>> + Send,
{
    let handler = move |State(state): State<AppState>, req: Request| {
        let glue = glue.clone();
        async move {
            match extract::<E>(req).await {
                Ok((path, query, body)) => match glue(state, path, query, body).await {
                    Ok(value) => json_response(E::SUCCESS_STATUS, &value),
                    Err(err) => error_response(&err),
                },
                Err(err) => error_response(&err),
            }
        }
    };
    register::<E, _, _>(router, handler)
}

/// Register an empty (204, no body) endpoint.
pub fn mount_empty<E, F, Fut>(router: Router<AppState>, glue: F) -> Router<AppState>
where
    E: Endpoint + 'static,
    E::Path: Send,
    E::Query: DeserializeOwned + Send,
    E::Body: DeserializeOwned + Send,
    F: Fn(AppState, E::Path, E::Query, E::Body) -> Fut + Clone + Send + Sync + 'static,
    Fut: Future<Output = Result<(), ApiError>> + Send,
{
    let handler = move |State(state): State<AppState>, req: Request| {
        let glue = glue.clone();
        async move {
            match extract::<E>(req).await {
                Ok((path, query, body)) => match glue(state, path, query, body).await {
                    Ok(()) => Response::builder()
                        .status(E::SUCCESS_STATUS)
                        .body(Body::empty())
                        .expect("empty response is always valid"),
                    Err(err) => error_response(&err),
                },
                Err(err) => error_response(&err),
            }
        }
    };
    register::<E, _, _>(router, handler)
}

/// Register an NDJSON-streaming endpoint (Q2). The glue resolves an
/// [`EventStream`]; the initial `Result` becomes the HTTP status (e.g. a
/// `resume_gap_too_large` error), then a 200 `application/x-ndjson` body streams
/// one JSON object per line.
pub fn mount_ndjson<E, F, Fut>(router: Router<AppState>, glue: F) -> Router<AppState>
where
    E: Endpoint + 'static,
    E::Path: Send,
    E::Query: DeserializeOwned + Send,
    E::Body: DeserializeOwned + Send,
    F: Fn(AppState, E::Path, E::Query, E::Body) -> Fut + Clone + Send + Sync + 'static,
    Fut: Future<Output = Result<EventStream, ApiError>> + Send,
{
    let handler = move |State(state): State<AppState>, req: Request| {
        let glue = glue.clone();
        async move {
            match extract::<E>(req).await {
                Ok((path, query, body)) => match glue(state, path, query, body).await {
                    Ok(stream) => ndjson_response(stream),
                    Err(err) => error_response(&err),
                },
                Err(err) => error_response(&err),
            }
        }
    };
    register::<E, _, _>(router, handler)
}

/// Add `handler` to the router at `E::PATH` for `E::METHOD`. Routes that share a
/// path but differ in method (e.g. `GET`/`DELETE /v1/machines/{id}`) merge into
/// one method router — axum panics only on a duplicated method.
fn register<E, H, T>(router: Router<AppState>, handler: H) -> Router<AppState>
where
    E: Endpoint,
    H: axum::handler::Handler<T, AppState>,
    T: 'static,
{
    router.route(E::PATH, on(method_filter(E::METHOD), handler))
}

fn method_filter(method: Method) -> MethodFilter {
    match method {
        Method::Get => MethodFilter::GET,
        Method::Post => MethodFilter::POST,
        Method::Delete => MethodFilter::DELETE,
    }
}

/// Pull the typed `(Path, Query, Body)` triple out of a request, mapping every
/// failure to a typed [`ApiError`]. Path values arrive in template order from
/// [`RawPathParams`] and are validated by the catalog's [`PathParams::parse`];
/// the query is `application/x-www-form-urlencoded`; the body is JSON (an empty
/// body decodes as the unit `()` for body-less routes).
async fn extract<E>(req: Request) -> Result<(E::Path, E::Query, E::Body), ApiError>
where
    E: Endpoint,
    E::Query: DeserializeOwned,
    E::Body: DeserializeOwned,
{
    let (mut parts, body) = req.into_parts();

    let raw = RawPathParams::from_request_parts(&mut parts, &())
        .await
        .map_err(|e| ApiError::internal(format!("path parameter extraction failed: {e}")))?;
    let values: Vec<&str> = raw.iter().map(|(_, value)| value).collect();
    let path = <E::Path as PathParams>::parse(E::PATH, &values)?;

    let query: E::Query =
        serde_urlencoded::from_str(parts.uri.query().unwrap_or("")).map_err(|e| {
            ApiError::Validation {
                field: "query".to_owned(),
                message: e.to_string(),
            }
        })?;

    let bytes = to_bytes(body, MAX_BODY_BYTES)
        .await
        .map_err(|e| ApiError::Validation {
            field: "body".to_owned(),
            message: format!("could not read request body: {e}"),
        })?;
    let body = decode_body::<E::Body>(&bytes)?;

    Ok((path, query, body))
}

/// Decode a JSON request body. An empty body is treated as the JSON `null` so
/// body-less routes (`E::Body = ()`) need send nothing on the wire.
fn decode_body<B: DeserializeOwned>(bytes: &[u8]) -> Result<B, ApiError> {
    let slice: &[u8] = if bytes.is_empty() { b"null" } else { bytes };
    serde_json::from_slice(slice).map_err(|e| ApiError::Validation {
        field: "body".to_owned(),
        message: e.to_string(),
    })
}

fn json_response<T: Serialize>(status: u16, value: &T) -> Response {
    match serde_json::to_vec(value) {
        Ok(bytes) => Response::builder()
            .status(status)
            .header(CONTENT_TYPE, JSON)
            .body(Body::from(bytes))
            .expect("json response is always valid"),
        Err(e) => error_response(&ApiError::internal(format!(
            "failed to serialize response: {e}"
        ))),
    }
}

/// Serialize a typed [`ApiError`] to its mapped HTTP status + JSON body.
pub fn error_response(err: &ApiError) -> Response {
    let body = serde_json::to_vec(err).unwrap_or_else(|_| b"{\"code\":\"internal\"}".to_vec());
    Response::builder()
        .status(err.http_status())
        .header(CONTENT_TYPE, JSON)
        .body(Body::from(body))
        .expect("error response is always valid")
}

fn ndjson_response(stream: EventStream) -> Response {
    let lines = stream.map(|item| {
        let mut bytes = match &item {
            Ok(event) => serde_json::to_vec(event),
            // A mid-stream error is serialized as one trailing error frame; the
            // stream is informational (R2), so this never demands client action.
            Err(err) => serde_json::to_vec(err),
        }
        .unwrap_or_default();
        bytes.push(b'\n');
        Ok::<Bytes, std::io::Error>(Bytes::from(bytes))
    });
    Response::builder()
        .status(200)
        .header(CONTENT_TYPE, NDJSON)
        .body(Body::from_stream(lines))
        .expect("ndjson response is always valid")
}
