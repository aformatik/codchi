//! Stable error catalog (Q5, P7, R5).
//!
//! Variant names map directly to wire `code` strings. Codes are stable across
//! non-breaking releases. This enum is the authoritative catalog — adding a
//! code requires an entry here.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::dto::job::JobState;
use crate::dto::secret::SecretKey;
use crate::ids::{EventSeq, JobId, MachineId};

/// Typed API error. Serialized as an internally-tagged object with a stable
/// `code` discriminator, e.g. `{"code":"machine_not_found","machine":"foo"}`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema, thiserror::Error)]
#[serde(tag = "code", rename_all = "snake_case")]
pub enum ApiError {
    /// No machine with this id exists.
    #[error("machine '{machine}' not found")]
    MachineNotFound { machine: MachineId },

    /// Another writer job holds this machine (P7 rule 2).
    #[error("machine '{machine}' is busy with job {job}")]
    MachineBusy { machine: MachineId, job: JobId },

    /// No job with this id exists, or its metadata was pruned.
    #[error("job {job} not found")]
    JobNotFound { job: JobId },

    /// The job is past the point where cancellation is accepted (P7).
    #[error("job {job} is not cancellable in state {state:?}")]
    JobNotCancellable { job: JobId, state: JobState },

    /// The store is unavailable, migrating, or needs repair (P7 rule 5).
    #[error("store unavailable: {reason}")]
    StoreUnavailable { reason: String },

    /// A store repair/migration job is already running (P7).
    #[error("store is busy with job {job}")]
    StoreBusy { job: JobId },

    /// The on-disk SQLite schema must be migrated before serving.
    #[error("schema migration required: current {current}, required {required}")]
    SchemaMigrationRequired { current: u32, required: u32 },

    /// Requested `since_seq` is older than retained events (Q2 → `410 Gone`).
    #[error("resume gap too large: requested {requested}, oldest {oldest}")]
    ResumeGapTooLarge {
        requested: EventSeq,
        oldest: EventSeq,
    },

    /// The client and server disagree on the API major version (Q5).
    #[error("API version mismatch: client v{client}, server v{server}")]
    ApiVersionMismatch { client: u32, server: u32 },

    /// Implicit start blocked because declared secrets are unset (R5).
    #[error("machine '{machine}' is missing required secrets")]
    MissingRequiredSecrets {
        machine: MachineId,
        keys: Vec<SecretKey>,
    },

    /// A request field failed validation (P4). `field` is the request field
    /// path (e.g. `id`, `source`, `target`).
    #[error("validation error on '{field}': {message}")]
    Validation { field: String, message: String },

    /// Unexpected server-side failure.
    #[error("internal error: {message}")]
    Internal { message: String },
}

impl ApiError {
    /// The stable wire `code` string for this error.
    pub fn code(&self) -> &'static str {
        match self {
            ApiError::MachineNotFound { .. } => "machine_not_found",
            ApiError::MachineBusy { .. } => "machine_busy",
            ApiError::JobNotFound { .. } => "job_not_found",
            ApiError::JobNotCancellable { .. } => "job_not_cancellable",
            ApiError::StoreUnavailable { .. } => "store_unavailable",
            ApiError::StoreBusy { .. } => "store_busy",
            ApiError::SchemaMigrationRequired { .. } => "schema_migration_required",
            ApiError::ResumeGapTooLarge { .. } => "resume_gap_too_large",
            ApiError::ApiVersionMismatch { .. } => "api_version_mismatch",
            ApiError::MissingRequiredSecrets { .. } => "missing_required_secrets",
            ApiError::Validation { .. } => "validation",
            ApiError::Internal { .. } => "internal",
        }
    }

    /// The HTTP status this error maps to on the wire. Informational here; the
    /// `codchi-server` transport layer is authoritative.
    pub fn http_status(&self) -> u16 {
        match self {
            ApiError::MachineNotFound { .. } | ApiError::JobNotFound { .. } => 404,
            ApiError::ResumeGapTooLarge { .. } => 410,
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
