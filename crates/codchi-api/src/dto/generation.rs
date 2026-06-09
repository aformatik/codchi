//! Machine + store generation views.

use chrono::{DateTime, Utc};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::ids::{GenerationId, JobId, StoreGenerationId};

/// Lifecycle of a generation. Shared by machine and store generations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum GenerationStatus {
    /// Currently activated.
    Active,
    /// Built and retained, not active.
    Inactive,
    /// Pinned against garbage collection.
    Protected,
    /// Tombstoned; retained only for history.
    Deleted,
}

/// One machine generation (a built + activatable system).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct GenerationView {
    pub id: GenerationId,
    pub created_at: DateTime<Utc>,
    pub activated_at: Option<DateTime<Utc>>,
    pub flake_lock_hash: String,
    pub system_store_path: String,
    pub status: GenerationStatus,
    /// The job that produced this generation.
    pub source_job: JobId,
}

/// One store generation. Read-only in v1 (no activation endpoint); exposed for
/// diagnostics and tray display.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct StoreGenerationView {
    pub id: StoreGenerationId,
    pub created_at: DateTime<Utc>,
    pub activated_at: Option<DateTime<Utc>>,
    pub flake_lock_hash: String,
    pub runtime_store_path: String,
    pub status: GenerationStatus,
}
