//! Machine views and mutation requests (R9, R10).
//!
//! Status is split into orthogonal axes (R10): `run_status` (raw platform
//! container observation, Phase 4 MS2), `configuration_status` (desired-vs-active,
//! MS11), and the active `findings`. `lifecycle` is a server-derived display
//! rollup of `run_status` + `active_generation`; its `Creating`/`Reconciling`
//! states are derived, not stored.

use chrono::{DateTime, Utc};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::dto::doctor::Finding;
use crate::dto::generation::GenerationView;
use crate::dto::module::ModuleSpec;
use crate::dto::secret::SecretKey;
use crate::ids::{GenerationId, JobId, MachineId};

/// Raw platform-container observation, from the in-memory reconciler snapshot
/// (P6, Phase 4 MS2): the status of the realized container *itself*. Carried as
/// `Option<RunStatus>` on `MachineView`, where `None` means the reconciler has
/// not reported an observation yet — surfaced to users as `Lifecycle::Reconciling`,
/// not a `RunStatus` variant. Integrity failures are represented as `findings`,
/// not a runtime `Failed` state. The reconciler that produces these is Phase 5/7;
/// SQLite never stores them (MS2).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum RunStatus {
    /// The container is confirmed gone. A machine is born only at a successful
    /// platform install (MS1), so the only way a born machine observes `Absent`
    /// is its container being removed out-of-band (e.g. `podman rm`).
    Absent,
    Stopped,
    Running,
}

/// Server-derived display rollup of where a machine is in its lifecycle (R10).
/// Computed once from `active_generation` + `run_status` so every client renders
/// the same label without re-deriving the orthogonal axes; never stored.
///
/// Derivation (total):
///
/// ```text
/// active_generation is None   => Creating     (synthesized in-flight create, MS1)
/// run_status is None          => Reconciling  (born, no observation yet)
/// run_status is Some(Absent)  => Absent       (container removed out-of-band)
/// run_status is Some(Stopped) => Stopped
/// run_status is Some(Running) => Running
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum Lifecycle {
    /// Synthesized in-flight-create view: the create job is running and no
    /// durable machine row exists yet (`active_generation == None`, MS1).
    Creating,
    /// Born machine with no reconciler observation yet (e.g. just after daemon
    /// restart, or an initial probe in flight).
    Reconciling,
    /// Container confirmed removed out-of-band (`run_status == Some(Absent)`).
    Absent,
    Stopped,
    Running,
}

/// Pure desired-versus-active configuration projection (Phase 4 MS11); replaces
/// beta's `UpdateStatus`. Never stored — derived on every machine read.
/// Update-availability ("a newer remote input exists") is deliberately **not**
/// on this axis (MS11): it requires fetching remote inputs and is a future
/// separate in-memory field.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ConfigurationStatus {
    /// Only on the synthesized in-flight-create view (active_generation `None`).
    Unbuilt,
    /// Active generation's configuration snapshot equals the desired config.
    Applied,
    /// Desired config differs from the active generation's snapshot.
    NeedsRebuild,
}

/// The list view of a machine. Returned from the daemon-maintained snapshot;
/// read endpoints must not probe synchronously (P6).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct MachineView {
    pub id: MachineId,
    /// Raw platform-container observation; `None` until the reconciler reports
    /// one (MS2). For the display rollup use `lifecycle`.
    #[serde(default)]
    pub run_status: Option<RunStatus>,
    /// Server-derived lifecycle rollup for display (R10) — folds `run_status` +
    /// `active_generation` into one label.
    pub lifecycle: Lifecycle,
    pub configuration_status: ConfigurationStatus,
    /// `None` ⇒ synthesized in-flight-create view only; no durable machine row is
    /// ever `None` (R9/MS1 — a machine is born at its first generation commit).
    #[serde(default)]
    pub active_generation: Option<GenerationId>,
    /// Per-machine **state**-format version for state migration (MS12/MS15): `0`
    /// for beta-migrated machines, `>= 1` otherwise. This is the per-machine state
    /// version (the renamed `schema_version` idea), distinct from the global DB
    /// schema version (`PRAGMA user_version`).
    pub state_version: u32,
    /// Active findings; `health = health(&findings)` (R10).
    #[serde(default)]
    pub findings: Vec<Finding>,
    /// The in-flight job owning this machine, if any.
    #[serde(default)]
    pub busy_with: Option<JobId>,
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
    pub generations: Vec<GenerationView>,
}

impl MachineDetail {
    /// The active generation's committed `flake.lock` hash, or `None` when the
    /// machine has never committed a successful generation (MS9). Derived from
    /// the active generation rather than carried as a field (MS15): looks up
    /// `view.active_generation` in `generations`.
    pub fn flake_lock_hash(&self) -> Option<&str> {
        let active = self.view.active_generation.as_ref()?;
        self.generations
            .iter()
            .find(|g| &g.id == active)
            .map(|g| g.flake_lock_hash.as_str())
    }
}

/// Create a new machine (job). All inputs are supplied up front; the job runs
/// to success or fails (R9).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct CreateMachineRequest {
    pub id: MachineId,
    pub modules: Vec<ModuleSpec>,
    /// On failure, retain the failed **create job**'s artifacts (its diagnostic
    /// workspace + partial platform resources) for introspection, reserving the
    /// machine id until cleared (R9/MS1 — there is no durable machine row to
    /// retain). Defaults to `false` (tear down completely).
    #[serde(default)]
    pub keep_on_fail: bool,
}

/// Duplicate an existing machine into a new, independent one (job, MS13). The
/// source is the `{id}` path segment of `POST /machines/{id}/duplicate`; only
/// the new name travels in the body. Not a rename: the source is untouched and
/// the result has its own fresh generation history. The filesystem copy and the
/// source-stopped requirement are Phase 7.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct DuplicateMachineRequest {
    pub target: MachineId,
}

/// Write a machine's desired module set (sync). Marks `needs_rebuild`; does not
/// auto-trigger a rebuild (Q4).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct SetModulesRequest {
    pub modules: Vec<ModuleSpec>,
}
