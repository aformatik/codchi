//! Machine views and mutation requests (R9, R10).
//!
//! Status is split into orthogonal axes (R10): `run_status` (platform
//! liveness), `update_status` (desired-vs-built), and the active `findings`.
//! `Building`, `Creating`, and `Failed` are *derived* from `active_generation`
//! + `busy_with`, not stored variants.

use chrono::{DateTime, Utc};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::dto::doctor::Finding;
use crate::dto::generation::GenerationView;
use crate::dto::module::ModuleSpec;
use crate::dto::secret::SecretKey;
use crate::ids::{GenerationId, JobId, MachineId};

/// Platform liveness, from the reconciler snapshot (P6).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum RunStatus {
    Stopped,
    Running,
}

/// Desired-vs-built config status (beta's `ConfigStatus`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum UpdateStatus {
    UpToDate,
    NeedsRebuild,
    UpdatesAvailable,
}

/// The list view of a machine. Returned from the daemon-maintained snapshot;
/// read endpoints must not probe synchronously (P6).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct MachineView {
    pub id: MachineId,
    pub run_status: RunStatus,
    pub update_status: UpdateStatus,
    /// `None` ⇒ creating or failed-create (R9).
    #[serde(default)]
    pub active_generation: Option<GenerationId>,
    /// Active findings; `health = health(&findings)` (R10).
    #[serde(default)]
    pub findings: Vec<Finding>,
    /// The in-flight job owning this machine, if any.
    #[serde(default)]
    pub busy_with: Option<JobId>,
    pub schema_version: u32,
    // P6 snapshot freshness:
    #[serde(default)]
    pub last_reconciled_at: Option<DateTime<Utc>>,
    #[serde(default)]
    pub last_reconcile_attempt_at: Option<DateTime<Utc>>,
    #[serde(default)]
    pub snapshot_stale: bool,
}

/// The detail view of a machine.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct MachineDetail {
    pub view: MachineView,
    pub modules: Vec<ModuleSpec>,
    pub secrets: Vec<SecretKey>,
    pub flake_lock_hash: String,
    pub generations: Vec<GenerationView>,
}

/// Create a new machine (job). All inputs are supplied up front; the job runs
/// to success or fails (R9).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct CreateMachineRequest {
    pub id: MachineId,
    pub modules: Vec<ModuleSpec>,
    /// Retain the failed machine row + artifacts on failure for introspection
    /// (R9). Defaults to `false` (tear down completely).
    #[serde(default)]
    pub keep_on_fail: bool,
}

/// Clone an existing machine into a new one (job). The source is the
/// `{id}` path segment of `POST /machines/{id}/clone`; only the new name
/// travels in the body.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct CloneMachineRequest {
    pub target: MachineId,
}

/// Write a machine's desired module set (sync). Marks `needs_rebuild`; does not
/// auto-trigger a rebuild (Q4).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct SetModulesRequest {
    pub modules: Vec<ModuleSpec>,
}
