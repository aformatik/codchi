//! Beta-migration DTOs (Q4, Phase 11).

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::ids::MachineId;

/// What migrating one beta machine would do.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct MigrationMachinePlan {
    /// Beta machine name as found on disk.
    pub source_name: String,
    /// Proposed `MachineId` (Q1: imports the existing name).
    pub target: MachineId,
    /// Why this machine cannot be migrated as-is (P4 dry-run flags), if any.
    /// Empty ⇒ migratable.
    #[serde(default)]
    pub blockers: Vec<String>,
}

/// A pure plan/read view of a beta → v1 migration.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct MigrationPlan {
    /// Whether a beta installation was detected at all.
    pub beta_detected: bool,
    pub machines: Vec<MigrationMachinePlan>,
}

/// Options for `migration_run`.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct MigrationOpts {
    /// Plan only; make no changes.
    #[serde(default)]
    pub dry_run: bool,
}

/// Result summary of a completed migration.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct MigrationSummary {
    pub migrated: u32,
    pub skipped: u32,
    pub failed: u32,
}
