//! Log sources (R11).
//!
//! A [`LogSource`] is a long-lived entity that owns a durable log stream:
//! the server, the store container, or a machine container. It is also the
//! [`crate::dto::job::JobView::subject`] of a job — "a job is an operation
//! against a source" — so the same type names both the streaming target of
//! `stream_logs` and what a job acts on.

use std::fmt;
use std::str::FromStr;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::ApiError;
use crate::ids::MachineId;

/// A long-lived log-emitting entity (R11).
///
/// Adjacently tagged on the wire — `{"type":"server"}`, `{"type":"store"}`,
/// `{"type":"machine","id":"foo"}` — because the `Machine` variant wraps a
/// transparent-string newtype, which serde cannot internally tag.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type", content = "id", rename_all = "snake_case")]
pub enum LogSource {
    /// The `codchi-server` daemon itself.
    Server,
    /// The store container.
    Store,
    /// A machine container.
    Machine(MachineId),
}

impl LogSource {
    /// The id-less discriminant, e.g. for [`JobFilter`](crate::dto::job::JobFilter).
    pub fn kind(&self) -> LogSourceKind {
        match self {
            LogSource::Server => LogSourceKind::Server,
            LogSource::Store => LogSourceKind::Store,
            LogSource::Machine(_) => LogSourceKind::Machine,
        }
    }
}

/// The `{source}` path-segment form used by `stream_logs`: `server`, `store`,
/// or `machine-<id>`. The `machine-` prefix keeps a machine literally named
/// `server`/`store` unambiguous.
impl fmt::Display for LogSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogSource::Server => f.write_str("server"),
            LogSource::Store => f.write_str("store"),
            LogSource::Machine(id) => write!(f, "machine-{id}"),
        }
    }
}

impl FromStr for LogSource {
    type Err = ApiError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "server" => Ok(LogSource::Server),
            "store" => Ok(LogSource::Store),
            other => match other.strip_prefix("machine-") {
                Some(id) => Ok(LogSource::Machine(MachineId::new(id, "source")?)),
                None => Err(ApiError::Validation {
                    field: "source".to_owned(),
                    message: format!(
                        "unknown log source '{other}' (expected 'server', 'store', or 'machine-<id>')"
                    ),
                }),
            },
        }
    }
}

/// The id-less kind of a [`LogSource`], used to filter `list_jobs`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum LogSourceKind {
    Server,
    Store,
    Machine,
}
