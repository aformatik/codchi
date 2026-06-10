//! The typed HTTP client (doc 06).
//!
//! [`HttpClient`] dials the per-user Unix socket (D6) and speaks the v1 contract.
//! The wire work is written **once**, generic over `E: Endpoint`: [`call_json`],
//! [`call_ndjson`], and [`call_empty`] render the path + query from the typed
//! catalog, send the request, and decode the response per its shape. The
//! semantic [`CodchiService`](codchi_api::CodchiService) surface is then a set of
//! one-line mappings (see [`crate::service_impl`]) — the residual glue doc 06
//! keeps because Rust can't auto-derive a method's argument list.
//!
//! Each call opens its own short-lived connection: a `UnixStream` + HTTP/1
//! handshake, with the connection task spawned so streaming (NDJSON) bodies keep
//! flowing while the response is consumed.

use std::path::{Path, PathBuf};

use bytes::Bytes;
use codchi_api::service::EventStream;
use codchi_api::{ApiError, Endpoint, Event, Method, PathParams};
use futures::StreamExt;
use http_body_util::{BodyExt, BodyStream, Full};
use hyper::Request;
use hyper::body::Incoming;
use hyper::client::conn::http1;
use hyper::header::{CONTENT_TYPE, HOST};
use hyper_util::rt::TokioIo;
use serde::Serialize;
use serde::de::DeserializeOwned;
use tokio::net::UnixStream;

/// A typed client for a `codchi-server` listening on a Unix socket.
#[derive(Clone, Debug)]
pub struct HttpClient {
    socket: PathBuf,
}

impl HttpClient {
    /// Build a client targeting `socket`. Connection is lazy — established per
    /// request — so this never fails.
    pub fn new(socket: impl Into<PathBuf>) -> Self {
        HttpClient {
            socket: socket.into(),
        }
    }

    /// A client targeting the default per-user socket
    /// ([`codchi_shared::server_socket_path`]).
    pub fn connect_default() -> Self {
        Self::new(codchi_shared::server_socket_path())
    }

    /// The socket path this client dials.
    pub fn socket(&self) -> &Path {
        &self.socket
    }

    /// Call a JSON-returning endpoint, decoding `E::Response` on 2xx and the
    /// typed [`ApiError`] otherwise.
    pub async fn call_json<E>(
        &self,
        path: E::Path,
        query: E::Query,
        body: E::Body,
    ) -> Result<E::Response, ApiError>
    where
        E: Endpoint,
        E::Query: Serialize,
        E::Body: Serialize,
        E::Response: DeserializeOwned,
    {
        let response = self.send::<E>(path, &query, &body).await?;
        let status = response.status();
        let bytes = collect(response).await?;
        if status.is_success() {
            serde_json::from_slice(&bytes)
                .map_err(|e| ApiError::internal(format!("could not decode response body: {e}")))
        } else {
            Err(decode_error(status, &bytes))
        }
    }

    /// Call an empty (204) endpoint, mapping a non-2xx status to the typed error.
    pub async fn call_empty<E>(
        &self,
        path: E::Path,
        query: E::Query,
        body: E::Body,
    ) -> Result<(), ApiError>
    where
        E: Endpoint,
        E::Query: Serialize,
        E::Body: Serialize,
    {
        let response = self.send::<E>(path, &query, &body).await?;
        let status = response.status();
        if status.is_success() {
            Ok(())
        } else {
            let bytes = collect(response).await?;
            Err(decode_error(status, &bytes))
        }
    }

    /// Call an NDJSON endpoint. A non-2xx initial status is the typed error;
    /// otherwise the body is decoded line-by-line into an [`EventStream`].
    pub async fn call_ndjson<E>(
        &self,
        path: E::Path,
        query: E::Query,
        body: E::Body,
    ) -> Result<EventStream, ApiError>
    where
        E: Endpoint,
        E::Query: Serialize,
        E::Body: Serialize,
    {
        let response = self.send::<E>(path, &query, &body).await?;
        let status = response.status();
        if !status.is_success() {
            let bytes = collect(response).await?;
            return Err(decode_error(status, &bytes));
        }
        Ok(into_event_stream(response.into_body()))
    }

    /// Render the request from the typed catalog and send it over a fresh
    /// connection.
    async fn send<E>(
        &self,
        path: E::Path,
        query: &E::Query,
        body: &E::Body,
    ) -> Result<hyper::Response<Incoming>, ApiError>
    where
        E: Endpoint,
        E::Query: Serialize,
        E::Body: Serialize,
    {
        let uri = build_uri::<E>(&path, query)?;
        let builder = Request::builder()
            .method(http_method(E::METHOD))
            .uri(uri)
            .header(HOST, "localhost");
        let request = match encode_body(body)? {
            Some(bytes) => builder
                .header(CONTENT_TYPE, "application/json")
                .body(Full::new(bytes)),
            None => builder.body(Full::new(Bytes::new())),
        }
        .map_err(|e| ApiError::internal(format!("could not build request: {e}")))?;

        let stream = UnixStream::connect(&self.socket).await.map_err(|e| {
            ApiError::internal(format!(
                "could not connect to codchi-server at {}: {e}",
                self.socket.display()
            ))
        })?;
        let (mut sender, conn) = http1::handshake(TokioIo::new(stream))
            .await
            .map_err(|e| ApiError::internal(format!("connection handshake failed: {e}")))?;
        // Drive the connection in the background so a streaming body keeps
        // flowing while the response is read.
        tokio::spawn(async move {
            let _ = conn.await;
        });

        sender
            .send_request(request)
            .await
            .map_err(|e| ApiError::internal(format!("request failed: {e}")))
    }
}

fn http_method(method: Method) -> hyper::Method {
    match method {
        Method::Get => hyper::Method::GET,
        Method::Post => hyper::Method::POST,
        Method::Delete => hyper::Method::DELETE,
    }
}

/// Build the origin-form request URI (`/v1/...[?query]`) from the typed path and
/// query. An empty query string is omitted.
fn build_uri<E>(path: &E::Path, query: &E::Query) -> Result<String, ApiError>
where
    E: Endpoint,
    E::Query: Serialize,
{
    let rendered = path.render(E::PATH);
    let query = serde_urlencoded::to_string(query)
        .map_err(|e| ApiError::internal(format!("could not encode query: {e}")))?;
    if query.is_empty() {
        Ok(rendered)
    } else {
        Ok(format!("{rendered}?{query}"))
    }
}

/// Serialize a request body to JSON. A body that serializes to `null` (notably
/// the unit `()` of body-less routes) is sent as no body at all, matching the
/// server's empty-body decoding.
fn encode_body<B: Serialize>(body: &B) -> Result<Option<Bytes>, ApiError> {
    let bytes = serde_json::to_vec(body)
        .map_err(|e| ApiError::internal(format!("could not encode request body: {e}")))?;
    if bytes == b"null" {
        Ok(None)
    } else {
        Ok(Some(Bytes::from(bytes)))
    }
}

async fn collect(response: hyper::Response<Incoming>) -> Result<Bytes, ApiError> {
    response
        .into_body()
        .collect()
        .await
        .map(|buf| buf.to_bytes())
        .map_err(|e| ApiError::internal(format!("could not read response body: {e}")))
}

/// Decode an error response: the typed [`ApiError`] JSON if present, else a
/// synthetic internal error carrying the status and raw body.
fn decode_error(status: hyper::StatusCode, bytes: &[u8]) -> ApiError {
    serde_json::from_slice::<ApiError>(bytes).unwrap_or_else(|_| {
        ApiError::internal(format!(
            "server returned {status}: {}",
            String::from_utf8_lossy(bytes)
        ))
    })
}

/// Decode a chunked NDJSON body into an [`EventStream`], buffering across frame
/// boundaries and splitting on `\n`.
fn into_event_stream(body: Incoming) -> EventStream {
    let frames = BodyStream::new(body);
    let state = (frames, Vec::<u8>::new(), false);
    let stream = futures::stream::unfold(state, |(mut frames, mut buf, mut ended)| async move {
        loop {
            if let Some(pos) = buf.iter().position(|&b| b == b'\n') {
                let line: Vec<u8> = buf.drain(..=pos).collect();
                let trimmed = &line[..line.len() - 1];
                if trimmed.is_empty() {
                    continue;
                }
                return Some((parse_line(trimmed), (frames, buf, ended)));
            }
            if ended {
                if buf.is_empty() {
                    return None;
                }
                let line = std::mem::take(&mut buf);
                return Some((parse_line(&line), (frames, buf, true)));
            }
            match frames.next().await {
                Some(Ok(frame)) => {
                    if let Ok(data) = frame.into_data() {
                        buf.extend_from_slice(&data);
                    }
                }
                Some(Err(e)) => {
                    return Some((
                        Err(ApiError::internal(format!("stream error: {e}"))),
                        (frames, buf, true),
                    ));
                }
                None => ended = true,
            }
        }
    });
    Box::pin(stream)
}

/// Parse one NDJSON line. Lines are normally [`Event`]s; a trailing line may be
/// a serialized [`ApiError`] (a mid-stream failure frame), surfaced as `Err`.
fn parse_line(line: &[u8]) -> Result<Event, ApiError> {
    match serde_json::from_slice::<Event>(line) {
        Ok(event) => Ok(event),
        Err(_) => match serde_json::from_slice::<ApiError>(line) {
            Ok(err) => Err(err),
            Err(e) => Err(ApiError::internal(format!("undecodable stream line: {e}"))),
        },
    }
}
