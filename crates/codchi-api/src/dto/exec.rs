//! Exec DTOs (R7).
//!
//! `prepare_exec` is a job that owns the exec-path implicit start and
//! start-time secret enforcement, streaming boot/systemd logs. Its typed output
//! is [`ExecPlan`]. When the machine is already running the job completes
//! near-instantly and the client reads the [`ExecPlan`] directly.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::ids::MachineId;

/// Request to prepare an exec session into a machine. The target machine is the
/// `{id}` path segment of `POST /machines/{id}/exec`; only the command body
/// travels here.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct PrepareExecRequest {
    /// Command + args to run. `None` requests an interactive login shell.
    #[serde(default)]
    pub command: Option<Vec<String>>,
}

/// Everything a client needs to attach an exec session once the machine is
/// ready.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct ExecPlan {
    pub machine: MachineId,
    /// Platform resource the client execs into (Podman container / WSL distro
    /// name, derived from the machine id).
    pub target: String,
    /// Resolved command line to execute inside the machine.
    pub command: Vec<String>,
    /// Working directory inside the machine, if one was resolved.
    #[serde(default)]
    pub cwd: Option<String>,
}
