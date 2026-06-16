# Phase 5 — Job System MVP (decisions)

Owns the job table, runner, conflict detection, cancel flags, the job state
machine, and bounded event streaming (PLAN Phase 5).

> Provisional. The job-artifact / diagnostics API below (JS-D1..JS-D2) was
> grilled during the **Phase 4** machine-state session and relocated here
> because it keys off the job table (which Phase 4 cannot provide) and operates
> on jobs, not machines. Recorded with open caveats; finalize when Phase 5 is
> grilled. The workspace artifact these endpoints act on is specified in
> [06-generation-model.md](06-generation-model.md); the wire contract is
> catalogued in [00-contract-decisions.md](00-contract-decisions.md).

## Job artifacts and diagnostics

### JS-D1 — Diagnostic capability is durable job state

`JobView` exposes whether the capability currently exists:

```rust
pub has_diagnostic_workspace: bool
```

It is `true` while the job has a retained diagnostic workspace
([06-generation-model.md](06-generation-model.md)) and `false` for jobs that
never materialized one or after cleanup / retention pruning.

**Caveat (Phase 4 grill):** this flag is a **stored column on the job row**,
written when the workspace is retained and cleared on cleanup/prune. It must
**not** be derived by probing the store filesystem on each `get_job` /
`list_jobs` — the workspace lives inside the store, and a per-read cross-boundary
stat is exactly the synchronous probe P6 forbids.

### JS-D2 — Two synchronous endpoints operate on the capability

```rust
async fn prepare_job_debug(&self, id: &JobId)  -> Result<JobDebugPlan, ApiError>;
async fn delete_job_artifacts(&self, id: &JobId) -> Result<(), ApiError>;
```

```text
POST   /v1/jobs/{id}/debug
DELETE /v1/jobs/{id}/artifacts
```

These two routes take the typed catalog from 28 to 30.

`prepare_job_debug` returns the native exec plan the CLI uses to attach
interactively; the server does **not** proxy a PTY. It is available only after
the owning job is terminal, so a client cannot attach to or interfere with a
running job.

```rust
pub struct JobDebugPlan {
    pub target: String,       // opaque store container/distro name
    pub command: Vec<String>, // native exec argv
    pub cwd: String,          // retained in-store workspace
}
```

The workspace path is exposed only through `JobDebugPlan`, to the client about
to execute it; clients do not derive store resource names or internal paths.

Typed failure matrix (both operations require retained artifacts):

| Condition | Error | HTTP |
|---|---|---:|
| Job metadata unknown or pruned | `JobNotFound { job }` | 404 |
| `prepare_job_debug` before the job is terminal | `JobNotTerminal { job, state }` | 409 |
| No retained diagnostic workspace | `JobArtifactsNotFound { job }` | 404 |

`JobArtifactsNotFound` applies whether the workspace was never produced, was
already deleted, or was pruned by retention — so repeating a successful
`delete_job_artifacts` returns `JobArtifactsNotFound`; cleanup does not use
success to hide absence.

### JS-D3 — A retained failed-create job reserves its machine id

Because the simplified machine model has no durable creating/failed-create row
(Phase 4 MS1, R9), the retained failed-create **job** is what holds the machine
id against a clobbering retry. The machine-id namespace is **{committed machine
rows} ∪ {retained failed-create jobs}**. A `create` is rejected when the id
names:

- a committed machine (existing collision rule, `ApiError::Validation`);
- a running create job (existing job-conflict, Phase-0 Q4); or
- a terminal-failed create job that still has retained artifacts →
  `CreateArtifactsRetained { machine, job }` (409), carrying the retained job so
  the client can inspect/debug (`prepare_job_debug`) or clear it.

`delete_job_artifacts` on a create job removes **both** the diagnostic workspace
**and** the retained partial `codchi-machine-<id>` platform resources, after
which the id is free. Workspaces are keyed by job-id, so they never collide
across creates; only the machine-id-keyed platform resources need this guard.

The durability of the reservation comes from the durable **job record** (subject
= the machine id, with a retained-artifacts flag) — not a machine row — so it
survives a daemon restart. The same set bounds platform GC: a `codchi-machine-*`
resource is collected only when it is neither a committed machine nor owned by a
retained failed-create job.

## Caveats carried from the Phase 4 grill

- **Reuse the exec-plan machinery, do not reinvent it.** `JobDebugPlan` is the
  same native-exec pattern as the existing `prepare_exec` → `ExecPlan` (server
  returns argv, client execs, no HTTP PTY). When finalizing, decide explicitly
  whether `prepare_job_debug` is a distinct endpoint or a parameterization of
  exec whose subject is "a job's retained workspace in the store" rather than a
  machine — and share the implementation.
- **MVP vs power feature.** The must-have closing the real gap (a failed
  module-add/rebuild on a still-valid machine retains a workspace with no machine
  to delete) is `delete_job_artifacts` + the stored `has_diagnostic_workspace`
  flag. The interactive `nix build --debugger` entry is a justified power feature
  that may ship later, alongside the exec-plan reuse above.
- The "cleanup is synchronous, not a job" exception to Q4 lives with the
  workspace artifact in [06-generation-model.md](06-generation-model.md) GM-W1.
