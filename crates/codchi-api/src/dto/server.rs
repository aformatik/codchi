//! Server status DTO (P5).
//!
//! Cheap to compute, answered from in-memory daemon state, no probing on call.

use chrono::{DateTime, Utc};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::dto::doctor::FindingsSummary;
use crate::error::ApiError;

/// Headline daemon lifecycle state.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ServerLifecycle {
    Starting,
    Migrating,
    Healthcheck,
    Ready,
    Degraded,
    Stopping,
}

/// Store availability, fed by the 15 s store sentinel (P6), not a fresh probe.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum StoreState {
    Up,
    Down,
    Recovering,
    Unknown,
}

/// Store status carried in [`ServerStatus`].
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct StoreStatus {
    pub state: StoreState,
    #[serde(default)]
    pub last_checked_at: Option<DateTime<Utc>>,
    #[serde(default)]
    pub last_error: Option<String>,
}

/// SQLite schema status.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct SchemaStatus {
    pub current: u32,
    pub required: u32,
    pub migrating: bool,
}

/// Server status (P5). `api_version` is also exposed as an HTTP response header
/// on every call; this field is authoritative for consumers of this endpoint.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct ServerStatus {
    pub lifecycle: ServerLifecycle,
    /// Q5 mismatch detection.
    pub api_version: u32,
    /// Build version of the running server.
    pub server_version: String,
    pub started_at: DateTime<Utc>,
    pub store: StoreStatus,
    pub schema: SchemaStatus,
    /// Reconciler overall heartbeat.
    #[serde(default)]
    pub last_reconciled_at: Option<DateTime<Utc>>,
    pub findings_summary: FindingsSummary,
    /// `Some` only when `lifecycle` indicates a recoverable failure.
    #[serde(default)]
    pub startup_error: Option<ApiError>,
}
