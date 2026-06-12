//! Health / doctor / findings DTOs (P6, R10).
//!
//! Stable [`FindingCode`] variants are the contract; `message` may be edited freely.
//! `health` is derived once here so CLI, tray, and server agree.

use chrono::{DateTime, Utc};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::ids::{FindingId, JobId, MachineId};

/// Finding severity, ordered least → most severe.
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
#[serde(rename_all = "snake_case")]
pub enum Severity {
    Info,
    Warning,
    Error,
    Critical,
}

/// The subsystem a finding pertains to.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum Component {
    Server,
    Store,
    Machine,
    Job,
    Migration,
}

/// Stable machine-readable identity of a health finding.
///
/// Each variant declares its wire string explicitly so Rust refactors cannot
/// silently change the contract.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub enum FindingCode {
    #[serde(rename = "store.unavailable")]
    StoreUnavailable,
    #[serde(rename = "podman.container_missing")]
    PodmanContainerMissing,
    #[serde(rename = "podman.mount_missing")]
    PodmanMountMissing,
    #[serde(rename = "podman.gcroot_missing")]
    PodmanGcrootMissing,
    #[serde(rename = "wsl.distro_missing")]
    WslDistroMissing,
    #[serde(rename = "wsl.rootfs_missing")]
    WslRootfsMissing,
    #[serde(rename = "generation.store_path_missing")]
    GenerationStorePathMissing,
    #[serde(rename = "reconcile.probe_failed")]
    ReconcileProbeFailed,
    #[serde(rename = "create.failed")]
    CreateFailed,
}

/// A single health finding produced by the reconciler or a `doctor_scan`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct Finding {
    pub id: FindingId,
    pub severity: Severity,
    pub component: Component,
    #[serde(default)]
    pub machine: Option<MachineId>,
    /// `Some` for `doctor_scan` findings; `None` for background-reconcile.
    #[serde(default)]
    pub source_job: Option<JobId>,
    pub code: FindingCode,
    /// User-facing message; may evolve.
    pub message: String,
    #[serde(default)]
    pub suggested_action: Option<String>,
    /// `true` only if repair cannot touch user data or rewrite `flake.lock`.
    pub auto_fixable: bool,
    pub created_at: DateTime<Utc>,
}

/// A point-in-time report of findings.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct DoctorReport {
    pub findings: Vec<Finding>,
    pub generated_at: DateTime<Utc>,
}

/// Counts of active findings by severity. Lets the tray render a badge without
/// a `doctor` call.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct FindingsSummary {
    pub critical: u32,
    pub error: u32,
    pub warning: u32,
    pub info: u32,
}

impl FindingsSummary {
    /// Tally a slice of findings into a summary.
    pub fn of(findings: &[Finding]) -> Self {
        let mut s = FindingsSummary::default();
        for f in findings {
            match f.severity {
                Severity::Critical => s.critical += 1,
                Severity::Error => s.error += 1,
                Severity::Warning => s.warning += 1,
                Severity::Info => s.info += 1,
            }
        }
        s
    }
}

/// Options for the `doctor` read and `doctor_scan` job.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct DoctorOpts {
    /// Restrict to a single machine, if set.
    #[serde(default)]
    pub machine: Option<MachineId>,
}

/// Derive machine/server health from active findings: the worst severity
/// present, or [`Severity::Info`] as the "Ok" floor when empty.
///
/// Defined once here (R10) so CLI, tray, and server agree. Callers treat an
/// empty finding set as healthy.
pub fn health(findings: &[Finding]) -> Severity {
    findings
        .iter()
        .map(|f| f.severity)
        .max()
        .unwrap_or(Severity::Info)
}

/// Whether a finding set represents a healthy subject (no findings above
/// `Info`).
pub fn is_healthy(findings: &[Finding]) -> bool {
    findings.iter().all(|f| f.severity == Severity::Info)
}
