//! Newtype identifiers (Q1, P3, P4).
//!
//! All IDs are newtypes; raw `String` / `Uuid` must not appear in DTOs. UUIDs
//! are v7 (time-ordered) so logs and job lists sort chronologically.

use std::fmt;
use std::str::FromStr;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::error::ApiError;

/// User-visible, permanent machine name. One identifier across API, CLI,
/// errors, events, and platform resources (Q1). Machines cannot be renamed.
///
/// Wire form is a plain string. Validation (P4) is *not* enforced in
/// `Deserialize` so that malformed input surfaces as a typed
/// [`ApiError::Validation`] at the service boundary rather than a generic parse
/// error. Use [`MachineId::new`] / [`MachineId::validate`] to validate.
#[derive(
    Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
#[serde(transparent)]
pub struct MachineId(pub String);

/// Reserved platform-resource prefix; rejected case-insensitively (P4).
const RESERVED_PREFIX: &str = "codchi-machine-";

impl MachineId {
    /// Construct and validate per P4. `field` is the request field path used in
    /// the returned [`ApiError::Validation`] (e.g. `"id"`, `"source"`,
    /// `"target"`).
    pub fn new(value: impl Into<String>, field: impl Into<String>) -> Result<Self, ApiError> {
        let id = MachineId(value.into());
        id.validate(field)?;
        Ok(id)
    }

    /// The raw name as a `&str`.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Validate against the P4 rules:
    /// regex `^[A-Za-z0-9](?:[A-Za-z0-9._-]{0,61}[A-Za-z0-9])?$`,
    /// length `1..=63`, and a case-insensitive reject of the
    /// `codchi-machine-` prefix. Implemented by hand (no `regex` dep).
    pub fn validate(&self, field: impl Into<String>) -> Result<(), ApiError> {
        let field = field.into();
        let invalid = |message: &str| {
            Err(ApiError::Validation {
                field: field.clone(),
                message: message.to_owned(),
            })
        };

        let s = &self.0;
        let len = s.len();
        if len == 0 {
            return invalid("machine name must not be empty");
        }
        if len > 63 {
            return invalid("machine name must be at most 63 characters");
        }
        if !s.is_ascii() {
            return invalid("machine name must be ASCII");
        }
        if s.to_ascii_lowercase().starts_with(RESERVED_PREFIX) {
            return invalid(
                "machine name must not start with the reserved prefix 'codchi-machine-'",
            );
        }

        let is_inner = |c: char| c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-');
        let is_edge = |c: char| c.is_ascii_alphanumeric();

        let bytes: Vec<char> = s.chars().collect();
        let first = bytes[0];
        let last = bytes[len - 1];
        if !is_edge(first) || !is_edge(last) {
            return invalid("machine name must start and end with a letter or digit");
        }
        if !bytes.iter().all(|&c| is_inner(c)) {
            return invalid("machine name may only contain letters, digits, '.', '_' and '-'");
        }
        Ok(())
    }
}

impl fmt::Display for MachineId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl FromStr for MachineId {
    type Err = ApiError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        MachineId::new(s, "id")
    }
}

/// Job identifier (UUID v7).
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
pub struct JobId(pub Uuid);

impl JobId {
    /// Mint a fresh time-ordered id.
    pub fn new() -> Self {
        JobId(Uuid::now_v7())
    }
}

impl Default for JobId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for JobId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Finding identifier (UUID v7).
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
pub struct FindingId(pub Uuid);

impl FindingId {
    /// Mint a fresh time-ordered id.
    pub fn new() -> Self {
        FindingId(Uuid::now_v7())
    }
}

impl Default for FindingId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for FindingId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Per-machine monotonic generation number.
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
pub struct GenerationId(pub u64);

impl fmt::Display for GenerationId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Monotonic store-generation number.
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
pub struct StoreGenerationId(pub u64);

impl fmt::Display for StoreGenerationId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Per-job monotonic event sequence number (Q2). Wire-stable `u64`.
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
pub struct EventSeq(pub u64);

impl fmt::Display for EventSeq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
