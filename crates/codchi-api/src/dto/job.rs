//! Job model (Q4, R1, R2, R8, R9).
//!
//! Every operation that touches external reality is a job. The event stream is
//! observability only; the actionable outcome is the terminal
//! [`JobView`]: exactly one of a typed `error` (Failed) or a typed `output`
//! (Succeeded). A retry is always a new job (R9).

use chrono::{DateTime, Utc};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::dto::config::ConfigResolution;
use crate::dto::doctor::DoctorReport;
use crate::dto::exec::ExecPlan;
use crate::dto::log::{LogSource, LogSourceKind};
use crate::dto::migration::MigrationSummary;
use crate::error::ApiError;
use crate::ids::{EventSeq, GenerationId, JobId, MachineId};

/// Job lifecycle states (P7).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum JobState {
    Queued,
    Running,
    CancelRequested,
    CleaningUp,
    Succeeded,
    Failed,
    Cancelled,
}

impl JobState {
    /// Whether the job has reached a terminal state.
    pub fn is_terminal(self) -> bool {
        matches!(
            self,
            JobState::Succeeded | JobState::Failed | JobState::Cancelled
        )
    }
}

/// What a job does. No `Start`/`Stop`/`Restart` (R6); no `Reconcile` (P6).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum JobKind {
    Init,
    Duplicate,
    Rebuild,
    Update,
    Delete,
    ActivateGeneration,
    GarbageCollect,
    Migration,
    DoctorScan,
    DoctorFix,
    /// R3: read-only flake fetch+eval → [`crate::dto::config::ConfigResolution`].
    Resolve,
    /// R7: ensure store+machine running, session, env → [`ExecPlan`].
    PrepareExec,
    /// R11: bring up the store container (a `Store`-subject job).
    StoreStart,
    /// R11: bounded store recovery (a `Store`-subject job).
    StoreRecover,
}

/// One changed flake input in an `update` job's lock diff.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct LockInputChange {
    /// Input name in `flake.lock`.
    pub input: String,
    /// Previous locked revision, if the input existed before.
    #[serde(default)]
    pub old_rev: Option<String>,
    /// New locked revision, if the input still exists after.
    #[serde(default)]
    pub new_rev: Option<String>,
}

/// Typed success payload of a `rebuild` job: the new active generation.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct Rebuilt {
    pub generation: GenerationId,
}

/// Typed success payload of a `duplicate` job (MS13): the new machine's first
/// generation.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct Duplicated {
    pub generation: GenerationId,
}

/// Typed success payload of an `update` job: the new generation plus the
/// `flake.lock` diff.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct Updated {
    pub generation: GenerationId,
    pub input_changes: Vec<LockInputChange>,
}

/// Typed success payload of a `garbage_collect` job: bytes freed in the store.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct GarbageCollected {
    pub freed_bytes: u64,
}

/// Type-erased terminal success output, one variant per producing [`JobKind`].
/// Each variant wraps the same payload type the corresponding typed
/// [`JobView<O>`] carries, so a `JobView<Rebuilt>` and a type-erased
/// [`JobView`] of the same job describe the same data. Adding a variant is
/// additive (Q5). A `None` `JobView.output` is valid for kinds with no payload
/// (e.g. `delete`, `activate_generation`).
///
/// This is the output type of the kind-erased [`crate::service::CodchiService::get_job`];
/// the kind-specific job methods return a narrowed [`JobView<O>`] instead.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum JobOutput {
    ConfigResolution(ConfigResolution),
    Rebuilt(Rebuilt),
    Duplicated(Duplicated),
    Updated(Updated),
    GarbageCollected(GarbageCollected),
    Migrated(MigrationSummary),
    Scanned(DoctorReport),
    ExecPlan(ExecPlan),
}

macro_rules! job_output_from {
    ($($variant:ident => $ty:ty),+ $(,)?) => {
        $(impl From<$ty> for JobOutput {
            fn from(value: $ty) -> Self {
                JobOutput::$variant(value)
            }
        })+
    };
}

job_output_from! {
    ConfigResolution => ConfigResolution,
    Rebuilt => Rebuilt,
    Duplicated => Duplicated,
    Updated => Updated,
    GarbageCollected => GarbageCollected,
    Migrated => MigrationSummary,
    Scanned => DoctorReport,
    ExecPlan => ExecPlan,
}

/// The canonical view of a job, generic over its typed success output `O`.
///
/// A terminal job is exactly one of: `Failed` with `error: Some(_)`, or
/// `Succeeded` with `output` set — or `output: None` for kinds with no output
/// (R1). The type parameter makes the success payload visible at the call site:
/// kind-specific methods return e.g. `JobView<Rebuilt>` or `JobView<()>` (no
/// output), while the kind-erased [`crate::service::CodchiService::get_job`]
/// returns the default `JobView` = `JobView<JobOutput>`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
// `#[serde(default)]` on `output: Option<O>` makes serde infer an over-eager
// `O: Default` bound on the generated `Deserialize` impl (it can't see that the
// default it needs is `Option::<O>::None`). That bound is spurious — no payload
// type is `Default` — and would make typed `JobView<O>` undeserializable by the
// HTTP client. Override it to the correct `O: Deserialize<'de>`. Wire form,
// OpenAPI, and the `Serialize` side are unchanged.
#[serde(bound(deserialize = "O: serde::Deserialize<'de>"))]
pub struct JobView<O = JobOutput> {
    pub id: JobId,
    pub kind: JobKind,
    /// The log source this job acts on (R11). Machine-less operations
    /// (`resolve_config`, `doctor_*`, `migration`) are `Server`-subject; store
    /// operations are `Store`-subject.
    pub subject: LogSource,
    pub state: JobState,
    pub created_at: DateTime<Utc>,
    #[serde(default)]
    pub started_at: Option<DateTime<Utc>>,
    #[serde(default)]
    pub finished_at: Option<DateTime<Utc>>,
    /// `Some` iff terminal `Failed` (R1/R2).
    #[serde(default)]
    pub error: Option<ApiError>,
    /// `Some` iff terminal `Succeeded` for kinds that produce output (R1/R2).
    #[serde(default)]
    pub output: Option<O>,
    /// Highest event sequence emitted so far.
    pub last_event_seq: EventSeq,
}

/// Query filter for `list_jobs` (R11). All fields optional; omitted ⇒ no
/// constraint. Fields flatten to query parameters.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct JobFilter {
    /// Restrict to jobs whose subject is of this kind.
    #[serde(default)]
    pub subject: Option<LogSourceKind>,
    /// Restrict to jobs acting on this specific machine.
    #[serde(default)]
    pub machine: Option<MachineId>,
    /// `true` ⇒ only non-terminal jobs (`Queued`/`Running`/`CancelRequested`/
    /// `CleaningUp`).
    #[serde(default)]
    pub active_only: Option<bool>,
}
