//! Stable error catalog (Q5, P7, R5).
//!
//! Each variant declares its wire `code` explicitly. Codes are stable across
//! non-breaking releases. This enum is the authoritative catalog — adding a
//! code requires an entry here.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::dto::job::JobState;
use crate::dto::secret::SecretKey;
use crate::ids::{JobId, MachineId};

/// Typed API error. Serialized as an internally-tagged object with a stable
/// `code` discriminator, e.g. `{"code":"machine_not_found","machine":"foo"}`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema, thiserror::Error)]
#[serde(tag = "code")]
pub enum ApiError {
    /// No machine with this id exists.
    #[serde(rename = "machine_not_found")]
    #[error("machine '{machine}' not found")]
    MachineNotFound { machine: MachineId },

    /// Another writer job holds this machine (P7 rule 2).
    #[serde(rename = "machine_busy")]
    #[error("machine '{machine}' is busy with job {job}")]
    MachineBusy { machine: MachineId, job: JobId },

    /// No job with this id exists, or its metadata was pruned.
    #[serde(rename = "job_not_found")]
    #[error("job {job} not found")]
    JobNotFound { job: JobId },

    /// The job is past the point where cancellation is accepted (P7).
    #[serde(rename = "job_not_cancellable")]
    #[error("job {job} is not cancellable in state {state:?}")]
    JobNotCancellable { job: JobId, state: JobState },

    /// The store is unavailable, migrating, or needs repair (P7 rule 5).
    #[serde(rename = "store_unavailable")]
    #[error("store unavailable: {reason}")]
    StoreUnavailable { reason: String },

    /// A store repair/migration job is already running (P7).
    #[serde(rename = "store_busy")]
    #[error("store is busy with job {job}")]
    StoreBusy { job: JobId },

    /// The on-disk SQLite schema must be migrated before serving.
    #[serde(rename = "schema_migration_required")]
    #[error("schema migration required: current {current}, required {required}")]
    SchemaMigrationRequired { current: u32, required: u32 },

    /// The client and server disagree on the API major version (Q5).
    #[serde(rename = "api_version_mismatch")]
    #[error("API version mismatch: client v{client}, server v{server}")]
    ApiVersionMismatch { client: u32, server: u32 },

    /// Implicit start blocked because declared secrets are unset (R5).
    #[serde(rename = "missing_required_secrets")]
    #[error("machine '{machine}' is missing required secrets")]
    MissingRequiredSecrets {
        machine: MachineId,
        keys: Vec<SecretKey>,
    },

    /// A request field failed validation (P4). `field` is the request field
    /// path (e.g. `id`, `source`, `target`).
    #[serde(rename = "validation")]
    #[error("validation error on '{field}': {message}")]
    Validation { field: String, message: String },

    /// Unexpected server-side failure.
    #[serde(rename = "internal")]
    #[error("internal error: {message}")]
    Internal { message: String },
}

impl ApiError {
    /// The HTTP status this error maps to on the wire. Informational here; the
    /// `codchi-server` transport layer is authoritative.
    pub fn http_status(&self) -> u16 {
        match self {
            ApiError::MachineNotFound { .. } | ApiError::JobNotFound { .. } => 404,
            ApiError::MachineBusy { .. }
            | ApiError::StoreBusy { .. }
            | ApiError::JobNotCancellable { .. } => 409,
            ApiError::StoreUnavailable { .. } => 503,
            ApiError::SchemaMigrationRequired { .. } => 503,
            ApiError::ApiVersionMismatch { .. } => 426,
            ApiError::MissingRequiredSecrets { .. } => 422,
            ApiError::Validation { .. } => 400,
            ApiError::Internal { .. } => 500,
        }
    }

    /// Convenience for the common internal-error case.
    pub fn internal(message: impl Into<String>) -> Self {
        ApiError::Internal {
            message: message.into(),
        }
    }
}
