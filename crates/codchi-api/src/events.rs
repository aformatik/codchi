//! Job event stream (Q2, R2, R8).
//!
//! Streams are informational only — nothing on the stream requires client
//! reaction (R2). All control flow is in the terminal [`crate::dto::JobView`].
//! Transport is chunked NDJSON over HTTP (`application/x-ndjson`), one JSON
//! [`Event`] per line, ordered by monotonic `seq` (Q2).

use chrono::{DateTime, Utc};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::ids::{EventSeq, FindingId};

/// Log level for [`Event::Log`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum LogLevel {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

/// Phase boundary status for [`Event::Phase`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum PhaseStatus {
    Started,
    Finished,
    Failed,
}

/// Nix build reference status for [`Event::NixBuild`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum NixBuildStatus {
    Started,
    Built,
    Failed,
}

/// One event on a job stream. Adding variants is non-breaking; renaming or
/// removing one is breaking (Q5).
///
/// Per R8 these split across persistence tiers: `Progress` is a throttled
/// in-memory aggregate (never persisted per-delta); the raw build firehose is
/// live-only `Log` (topic `build`) and is not replayed; `Phase`, `StateChange`,
/// `NixBuild`, `HealthFinding`, and `Log` for nix `Msg{Error,Warn}` / eval
/// errors are the durable, replayable tier.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Event {
    Log {
        seq: EventSeq,
        ts: DateTime<Utc>,
        level: LogLevel,
        topic: String,
        message: String,
    },
    Progress {
        seq: EventSeq,
        ts: DateTime<Utc>,
        phase: String,
        done: u64,
        #[serde(default)]
        total: Option<u64>,
    },
    Phase {
        seq: EventSeq,
        ts: DateTime<Utc>,
        name: String,
        status: PhaseStatus,
    },
    StateChange {
        seq: EventSeq,
        ts: DateTime<Utc>,
        state: crate::dto::job::JobState,
    },
    NixBuild {
        seq: EventSeq,
        ts: DateTime<Utc>,
        drv: String,
        status: NixBuildStatus,
    },
    HealthFinding {
        seq: EventSeq,
        ts: DateTime<Utc>,
        finding: FindingId,
    },
}

impl Event {
    /// The monotonic sequence number carried by every variant.
    pub fn seq(&self) -> EventSeq {
        match self {
            Event::Log { seq, .. }
            | Event::Progress { seq, .. }
            | Event::Phase { seq, .. }
            | Event::StateChange { seq, .. }
            | Event::NixBuild { seq, .. }
            | Event::HealthFinding { seq, .. } => *seq,
        }
    }
}

/// Query options for `stream_job_events` (Q2).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct EventStreamOpts {
    /// Return the last N events before following. Default 200. Ignored if
    /// `since_seq` is set (Q2 precedence).
    #[serde(default)]
    pub tail: Option<u32>,
    /// Resume strictly after this sequence (exclusive). Wins over `tail`.
    #[serde(default)]
    pub since_seq: Option<EventSeq>,
    /// Keep streaming after the current tail. Default `true`.
    #[serde(default = "default_follow")]
    pub follow: bool,
}

fn default_follow() -> bool {
    true
}

impl Default for EventStreamOpts {
    fn default() -> Self {
        EventStreamOpts {
            tail: None,
            since_seq: None,
            follow: true,
        }
    }
}

/// Default tail length when neither `tail` nor `since_seq` is given (Q2).
pub const DEFAULT_TAIL: u32 = 200;
