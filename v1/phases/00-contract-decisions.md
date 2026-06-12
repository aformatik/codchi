# Phase 0 — Locked Contract Decisions

These five decisions are the input to the `codchi-api` crate. They are locked.
Changes require an explicit revision of this document and an API major bump
(see Q5).

## Q1 — MachineId shape

**Decision: Option A. `MachineId` is the user-visible machine name.**

Rules:

- `MachineId` is exactly the permanent machine name, carried **bare** as the one
  identifier across the API, CLI, errors, events, SQLite, and logs.
- URL shape: `/v1/machines/foo`.
- The `codchi-machine-` prefix is **not** part of the identity. It is a
  *derived* platform-resource name (`codchi-machine-foo` for the Podman
  container / WSL distro), produced **only at the platform seam** — never stored
  or carried above it. See the platform-naming seam in
  `phases/01-http-vertical-slice.md` D11. The bare id is the source of truth;
  the qualified name crosses the API in exactly one place, `ExecPlan.target`
  (server-derived, so the client never hardcodes the scheme).
- The prefix partitions the `codchi-*` resource namespace by *type*
  (`codchi-machine-<id>` machines, store/server their own fixed names),
  mirroring the `LogSource` taxonomy (R11) and avoiding the beta's
  `codchistore`-style collision dodge.
- Machine names are **stable identities** and **cannot be renamed**.
- Renaming a machine is unsupported. The supported path is
  `codchi clone foo bar`, which creates a new machine.
- Beta migration imports each beta machine's existing name as its `MachineId`.
  Note the platform scheme **differs** from the beta (beta named containers
  `codchi-<name>`, v1 uses `codchi-machine-<name>`), so migration must
  rename/recreate the underlying container/distro, not assume the old name.
- Migration fails loudly on duplicate or invalid names; it does not silently
  rewrite them.

Validation:

- `MachineId` is a newtype around `String`.
- Implemented rules (`ids.rs`): length `1..=63`, ASCII, charset
  `^[A-Za-z0-9](?:[A-Za-z0-9._-]{0,61}[A-Za-z0-9])?$`, and a case-insensitive
  reject of the reserved `codchi-machine-` prefix. Must accept all current beta
  machine names that exist in the wild; must produce a usable
  container/WSL/gcroot/profile name without escaping.
- The reserved-prefix reject is **hygiene, not a correctness invariant**: the
  `codchi-machine-<id>` derivation is reversible regardless, so the reject only
  stops a machine name from masquerading as a fully-qualified resource (operator
  confusion in `podman ps`) and guards against double-prefix bugs. It is the one
  rule that can clash with "accept all beta names" — a beta machine literally
  named `codchi-machine-*` would fail migration loudly (vanishingly unlikely).

API consequences:

- No `rename_machine` endpoint.
- `clone_machine(source: MachineId, target: MachineId)` exists and is a job.
- `MachineId` collisions during create/clone return `ApiError::Validation`.

## Q2 — Event stream transport

**Decision: Option C. Chunked NDJSON over HTTP.**

> **Revised by R14:** `since_seq` resume and the `410 ResumeGapTooLarge` gap
> error are dropped — the query surface is just `tail` + `follow`. The struck
> rules below are retained for history; see R14 for the rationale.

Rules:

- Content type: `application/x-ndjson`.
- One JSON `Event` object per line, ordered by monotonic `seq: u64`.
- Endpoint: `GET /v1/jobs/{id}/events`.
- Query parameters:
  - `tail=<N>` — return the last N events before following (default 200).
  - ~~`since_seq=<N>` — resume strictly after sequence N. **Exclusive**:
    `since_seq=42` returns events with `seq >= 43`.~~ *(dropped, R14)*
  - `follow=<bool>` — keep streaming after the current tail. Default `true`.
- ~~Precedence: if both `tail` and `since_seq` are supplied, `since_seq` wins and
  `tail` is ignored.~~ *(dropped, R14)*
- ~~Retention gap: if the requested `since_seq` is older than available
  retention, respond with **`410 Gone`** and an `ApiError::ResumeGapTooLarge`
  body.~~ *(dropped, R14)* On any hard stream error the client re-subscribes
  from `tail`.
- Client disconnect: server drops the stream subscription. No keepalive
  protocol beyond TCP/socket level for v1.
- Multiple concurrent followers per job are supported.

OpenAPI:

- The endpoint is documented as `application/x-ndjson` with the schema being a
  stream of `Event` objects, one per line.
- Tooling should not try to model NDJSON as a JSON array; the body is a stream.

Testing:

```sh
curl --no-buffer --unix-socket "$SOCK" \
  'http://localhost/v1/jobs/abc/events?tail=50&follow=true' | jq -c .
```

## Q3 — Secret handling

**Decision: secrets are NixOS-declared values with a SQLite-cached schema;
keys are listable, values readable.** (Folds in R4/R5.)

Secrets are not free-form key/value: each key is declared by the machine's
NixOS config (`codchi.secrets.env`) with a `description`, and the declared
schema is cached into SQLite during build/eval. All declared secrets are
required and enforced at machine **start**, never at build (R5).

Rules:

- API endpoints (all sync SQLite reads/writes; honor the P6 no-probe
  invariant):
  - `set_secret(machine, key, value)` — validates `key` against the cached
    schema (`ApiError::Validation` on unknown keys); SQLite write; marks
    pending restart/rebuild if needed.
  - `get_secret(machine, key) -> String` — returns plaintext.
  - `list_secrets(machine) -> Vec<SecretKey>` — keys only;
    `SecretKey { name, description, has_value }`.
  - `delete_secret(machine, key)` — sync.
- Plaintext is acceptable for v1. Encryption at rest is out of scope.
- Access control is by the transport boundary: anyone who can reach the
  per-user socket is the user who owns the secrets.
- The API never silently logs secret values. Job event streams must not
  include secret values, even on error.

CLI consequences:

- `codchi secret set MACHINE KEY` — prompts or reads stdin, calls `set_secret`.
- `codchi secret get MACHINE KEY` — prints value to stdout. Honors normal
  shell quoting rules.
- `codchi secret list MACHINE` — prints keys only.
- `codchi secret rm MACHINE KEY` — calls `delete_secret`.

Tray consequences:

- May show secret keys.
- Showing values requires explicit user action (click-to-reveal); never on
  initial render.

Migration:

- Beta migration imports existing `config.json` secrets into SQLite. Values
  preserved as-is.

## Q4 — Sync vs. job endpoints

**Decision: Rule and per-endpoint classification.**

Rule:

> **Sync endpoints read or record Codchi state. Job endpoints make Codchi
> reconcile that state with external reality.**

"External reality" means Nix, WSL, Podman, host filesystems, profiles, gcroots,
migrations, repairs, and any operation needing logs, cancellation, cleanup, or
recovery.

| Endpoint | Result | Reason |
|---|---|---|
| `server_status` | Sync | Returns lifecycle state, including `Starting`. Never waits for readiness. |
| `list_machines` | Sync | Returns DB state plus daemon snapshot. |
| `get_machine` | Sync | Same, for one machine. |
| `set_modules` | Sync | Writes desired config; marks `needs_rebuild`. No implicit rebuild. |
| `set_secret` | Sync | Writes desired secret state; marks pending restart/rebuild if needed. |
| `get_secret` | Sync | SQLite read. |
| `list_secrets` | Sync | SQLite read (keys only). |
| `delete_secret` | Sync | SQLite write. |
| `list_generations` | Sync | SQLite read. |
| `doctor` (read) | Sync | Returns cached findings / last check result. |
| `migration_plan` | Sync | Pure plan/read view. |
| `create_machine` | Job | Nix + platform creation. |
| `clone_machine` | Job | Platform copy + state write. |
| `rebuild` | Job | Nix build + activation. |
| `update` | Job | Lock + update + build + activation. |
| `delete_machine` | Job | Platform cleanup; needs logs/recovery. |
| `activate_generation` | Job | Touches profiles/platform; consistency. |
| `doctor_scan` | Job | Active probing of store/machines. |
| `doctor_fix` | Job | Mutating repair. |
| `migration_run` | Job | External, idempotent migration with backups. |
| `resolve_config` | Job | Fetch + eval a flake to discover modules/nixpkgs (R3). Read-only but external. |
| `prepare_exec` | Job | Ensures store + machine running (implicit start), session, env (R7). |

Edge decisions:

- `set_modules` is sync. CLI may optionally chain a rebuild job afterward, but
  the API does not auto-trigger it.
- `delete_machine` is a job. CLI streams events so it still feels like one
  command.
- `activate_generation` is a job for consistency, even when fast.
- `doctor` splits: cached read view is sync; active scan and fix are jobs.
- `server_status` is always sync. It never blocks on readiness; the response
  includes the lifecycle state.

Daemon-snapshot invariant for sync reads:

- `list_machines` and `get_machine` return the daemon's maintained snapshot.
- The daemon keeps that snapshot fresh via startup, periodic, and job-driven
  background reconciliation.
- These endpoints **must not** probe Podman/WSL synchronously on request.

## Q5 — API versioning

**Decision: Option A with additive tweak. Single `API_VERSION` constant + URL
prefix `/v1/`. Only breaking changes bump the major.**

Rules:

- Single constant `API_VERSION` in `codchi-api`.
- URL prefix `/v1/` on every endpoint.
- Breaking changes bump the major and move routes to `/v2/`.
- Additive changes stay under `/v1/`: new endpoints, new optional request
  fields, new optional response fields.
- Clients **must** ignore unknown response fields. Code generated from OpenAPI
  must be configured accordingly.
- The server exposes its API version in `server_status`.
- A client that detects a major mismatch must fail immediately with a clear
  message:

  ```
  Codchi CLI/API mismatch: CLI expects v1, server speaks v2.
  Finish the upgrade or restart the Codchi server.
  ```

- The CLI must never silently downgrade requests or attempt cross-major calls.

OpenAPI CI policy:

- Generated `openapi.json` is committed to the repo.
- CI regenerates and diffs on every PR. Uncommitted regeneration fails CI.
- CI runs `oasdiff breaking old-openapi.json new-openapi.json` to classify
  changes:
  - **Additive:** pass under `/v1/` (new routes, new optional fields).
  - **Breaking:** fail unless the PR also bumps `API_VERSION` major and moves
    affected routes to `/v2/`.
- Field renames are breaking. Adding a required request field is breaking.
  Removing or renaming an enum variant is breaking.

Implemented (2026-06-09): the snapshot-drift check runs inside the hermetic
`checks.codchi-api` nix derivation (`gen-openapi` regenerated and `diff`ed
against the committed `openapi.json`); the breaking-change gate runs in
`.github/workflows/ci.yml` (`openapi-breaking` job) via `oasdiff breaking …
--fail-on ERR` against the PR base branch's snapshot. `oasdiff` is not in
nixpkgs, so it is packaged in-repo at `build/oasdiff.nix` and exposed as
`packages.oasdiff`. The `API_VERSION`-major / `/v2/` migration on a breaking
change remains a manual, deliberate step — the gate only forces it to be
intentional.

## Phase 0 Closeout — P1 to P8

These follow-ups extend Q1–Q5 and unblock the `codchi-api` crate
implementation. Same locking terms as Q1–Q5.

### P1 — OpenAPI generator

**Decision: `schemars` + `aide` core.**

- `schemars::JsonSchema` derive on every DTO in `codchi-api`.
- The endpoint catalog inside `codchi-api` is the single source of truth that
  generates / feeds OpenAPI.
- `codchi-server` `axum` handlers consume the catalog. Handlers are not the
  OpenAPI source.

### P2 — Time crate

**Decision: `chrono`.**

- DTO timestamps use `chrono::DateTime<chrono::Utc>`.
- Wire format remains RFC 3339 in UTC.
- `serde` and `schemars` integration via standard `chrono` features.

### P3 — UUID version

Locked previously. UUID v7 for `JobId`, `FindingId`, and any future
UUID-shaped ID. Also recorded in "Crate Rules".

### P4 — MachineId validation

**Decision: regex `^[A-Za-z0-9](?:[A-Za-z0-9._-]{0,61}[A-Za-z0-9])?$`.**

Rules:

- Allowed chars: ASCII letters, digits, `.`, `_`, `-`.
- First and last char: ASCII letter or digit.
- Length: 1..=63 ASCII bytes.
- Reserved prefix: reject `codchi-machine-` case-insensitively.
- Case: preserve original spelling, no normalization. Enforce
  **case-insensitive uniqueness** to avoid Windows/WSL/config-path collisions.

Rationale: broad enough for known beta names (`myMachine`, `gpu_test`); strict
enough to be safe as URL path segment, Linux path segment, WSL distro name,
and Podman container name without shell/path escaping. Requiring alphanumeric
ends avoids `.`/`..`, hidden-looking names, and trailing-separator weirdness.

Validation errors return `ApiError::Validation { field, message }` with
`field` set to the request field path (e.g. `id`, `source`, `target`).

Phase 11 dry-run **must flag** beta names with any of:

- spaces or shell metacharacters
- `/ \ : " ' < > | ? *`
- non-ASCII characters
- leading or trailing `.`, `_`, `-`
- length over 63
- `codchi-machine-` prefix in any case
- names differing only by ASCII case

If such names exist in real beta installs, revise the rule before release
rather than rewriting silently.

### P5 — `server_status` payload

**Decision: shape below. Cheap to compute, answered from in-memory daemon
state, no probing on call.**

```rust
pub struct ServerStatus {
    pub lifecycle: ServerLifecycle,
    pub api_version: u32,                          // Q5 mismatch detection
    pub server_version: String,                    // build version
    pub started_at: DateTime<Utc>,
    pub store: StoreStatus,                        // see P6 store sentinel
    pub schema: SchemaStatus,
    pub last_reconciled_at: Option<DateTime<Utc>>, // reconciler heartbeat
    pub findings_summary: FindingsSummary,
    pub startup_error: Option<ApiError>,           // Some only when lifecycle indicates failure
}

pub enum ServerLifecycle { Starting, Migrating, Healthcheck, Ready, Degraded, Stopping }

pub struct StoreStatus {
    pub state: StoreState,
    pub last_checked_at: Option<DateTime<Utc>>,
    pub last_error: Option<String>,
}
pub enum StoreState { Up, Down, Recovering, Unknown }

pub struct SchemaStatus { pub current: u32, pub required: u32, pub migrating: bool }
pub struct FindingsSummary { pub critical: u32, pub error: u32, pub warning: u32, pub info: u32 }
```

Rules:

- `lifecycle` is the headline state.
- `store.state` is fed by the 15s store sentinel from P6, not by a fresh
  probe on the request path.
- `last_reconciled_at` is the reconciler's overall heartbeat. Per-machine
  staleness lives in `MachineView` (P6).
- `startup_error` is `Some` only when `lifecycle` is `Degraded` or stuck in a
  pre-`Ready` state due to a recoverable failure. Use typed `ApiError` so
  clients can branch on `code`.
- `findings_summary` lets the tray render a badge without a `doctor` call.
- `api_version` is also exposed as an HTTP response header on every call.
  The field in `ServerStatus` is authoritative for consumers of this
  endpoint.

### P6 — Daemon-snapshot reconciliation

**Decision: internal reconciler + visible doctor. One shared engine, two
callers. No `Reconcile` JobKind.**

Background reconciler is an internal daemon task: not a `JobKind`, invisible
to clients, not cancellable through the job API. It writes daemon logs and
persistent findings. `doctor_scan` is the visible `JobKind::DoctorScan`,
cancellable and event-logged, powered by the same probe code in "force / deep
scan" mode.

**Snapshot model.** `list_machines` / `get_machine` return only SQLite state,
the daemon-maintained snapshot, and snapshot freshness metadata. Reserve
fields equivalent to:

```rust
last_reconciled_at: Option<DateTime<Utc>>,
last_reconcile_attempt_at: Option<DateTime<Utc>>,
snapshot_stale: bool,
```

Keep the last known good snapshot on probe failure. Do **not** synchronously
probe from read endpoints. A confirmed broken artifact (missing
container/rootfs/store path) updates the snapshot — possibly to `Failed`. A
failed probe alone does not flip a machine to `Failed`; it sets
`snapshot_stale = true` and creates a finding.

**Probes.**

Podman, preferably batched:

- Container presence/state: one `podman ps -a` / inspect inventory over
  Codchi-labeled containers; map running/exited/missing to
  `Running`/`Stopped`/`Failed`.
- Mount correctness: expected store mount present; expected machine root/data
  mount present; mount source paths exist on host.
- Gcroot/profile projections: active and protected generation gcroots exist;
  gcroot target matches SQLite generation store path; stale gcroots without
  DB generation become doctor findings.

WSL, preferably batched:

- Distro registration/status: one `wsl.exe --list --verbose` inventory; map
  registered/running/stopped/missing. Do not start distros to reconcile.
- Rootfs presence: configured rootfs/ext4.vhdx/base path exists; missing
  rootfs is a machine `Failed` finding.
- Do not probe inside the distro unless the machine is already running and an
  agent heartbeat is available.

Both platforms:

- SQLite active generation points to an existing generation row.
- SQLite `system_store_path` exists in the managed store.
- Active/protected/rollbackable generations checked in background;
  `doctor_scan` also checks all generations and orphaned projections.
- Store availability is queried through `codchi-server`'s store manager
  only — never a helper fallback.

**Cadence (scheduler, not a single loop).**

- Startup: open/migrate SQLite; immediately verify/start the store with a
  short timeout. If the store does not come up cleanly, attempt **bounded
  auto-repair** before going `Degraded`. Auto-repair is gated by the
  `auto_fixable` rule (see *auto_fixable*): repair steps must not touch
  **user data** or **machine container/distro filesystems**, and must not
  rewrite the stored `flake.lock`. The store itself contains only system
  files (packaged rootfs, Nix store paths) and is **in scope** for repair.
  Examples of permitted startup repair: restarting a stopped Podman container
  that holds the store, re-extracting the packaged store rootfs over a
  corrupt WSL store distro, recreating a missing gcroot or profile symlink.
  Examples of *forbidden* startup repair: re-extracting or wiping a machine's
  rootfs/container other than system files in corrupted wsl distros, deleting machine volumes, rewriting `flake.lock`. If
  auto-repair fails or is not applicable, transition server to `Degraded`,
  create `store.unavailable`, mark machine snapshots stale; enqueue machine
  reconciliation with jitter. After startup, the reconciler is
  **observe-only** — no further auto-repair runs anywhere. User-invoked
  `doctor_fix` is the only repair path.
- Store sentinel: lightweight check every 15 s while daemon runs; job
  preflight also checks store immediately.
- Machine reconciliation: target interval 60 s per machine, jitter ±20 %;
  stagger machines; at most one active Podman inventory probe and one active
  WSL inventory probe.
- Backoff on platform command failure: 1 m, 2 m, 5 m, 10 m max, with jitter;
  per-machine probe failure follows the same cap; success resets backoff.

Batch platform inventory, then apply results to staggered per-machine
snapshots.

**Triggers.** Required: daemon startup, periodic scheduler, post-job
completion, explicit `doctor_scan`. Additional: store lifecycle transition
(unavailable/recovered); machine boot failure reported through hostctl;
machine agent heartbeat/health report; config mutations affecting desired
state (e.g. `set_modules` flips `NeedsRebuild` immediately); optional Podman
event stream if cheap/reliable, used only to enqueue reconcile.

**Failure handling.** Persistent deduped findings, examples:

```
store.unavailable
podman.container_missing
podman.mount_missing
podman.gcroot_missing
wsl.distro_missing
wsl.rootfs_missing
generation.store_path_missing
reconcile.probe_failed
```

Rules:

- Confirmed missing artifact: update snapshot and finding.
- Probe failed/timed out: keep last known snapshot, set
  `snapshot_stale = true`.
- `last_reconcile_attempt_at` updates on every attempt.
- `last_reconciled_at` updates only after required probes succeed.
- Background-reconcile findings have `source_job = None`.
- `doctor_scan` findings have `source_job = Some(job_id)`.

**Interaction with mutating jobs.** Mutating jobs own the machine.

- Background reconciliation for a machine is skipped while
  `rebuild`/`update`/`clone`/`delete`/`activate_generation` is running.
- The reconciler uses a non-blocking try-lock; busy machines record no
  persistent finding and are retried later.
- Store-level checks may continue during machine jobs.
- The job itself updates obvious snapshot state (`Building`, `busy_with`).
- On terminal state + cleanup, enqueue an immediate targeted reconcile for
  that machine.
- `doctor_scan` must not preempt mutating jobs; it skips busy machines or
  reports them as transiently skipped.

**Implementation constraint.** Never hold a SQLite write transaction or job
lock while invoking Podman, WSL, or Nix. Read expected state, drop DB locks,
probe externally, then write results with a short transaction and a
generation/job-epoch check. This prevents deadlocks with job execution.

### P7 — Conflict-detection rules

**Decision: seven rules, no hidden queuing.**

1. **Reads always work.** Machines, jobs, logs, generations, and cached
   doctor results are inspectable anytime.
2. **One writer per machine.** If `foo` is rebuilding, any other write to
   `foo` is rejected — `update`, `set_modules`, `set_secret`,
   `activate_generation`, `delete_machine`, etc.
3. **Different machines can change at the same time.** `rebuild foo` and
   `rebuild bar` are allowed.
4. **GC runs in the background.** `garbage_collect` does not block machine
   jobs and machine jobs do not block GC. GC only collects unprotected store
   paths; correctness comes from gcroots/profiles, not admission locking.
5. **Broken store blocks machine work.** If the store is unavailable,
   migrating, or needs repair, no machine mutation or `prepare_exec` may
   start. Reads and doctor/store repair remain allowed.
6. **Delete is just another machine writer.** `delete_machine foo` is
   rejected if `foo` has an in-flight job. User cancels or waits, then
   deletes.
7. **No hidden waiting.** Every request either starts now or fails fast with
   the blocking reason.

**Error codes:**

- Same-machine conflict: `MachineBusy { machine, job }`.
- Store unhealthy/unavailable: `StoreUnavailable { reason }`.
- Store repair/migration already running (when precision is wanted):
  `StoreBusy { job }` (new variant — add to error catalog).

**`cancel_job` semantics.** Idempotent success on already-cancelling jobs:

```
Queued/Running              + cancel_job -> CancelRequested, Ok(())
CancelRequested             + cancel_job -> Ok(())  (already cancelling)
CleaningUp                  + cancel_job -> JobNotCancellable
Succeeded/Failed/Cancelled  + cancel_job -> JobNotCancellable
```

Rationale: users and CLIs send duplicate cancels through `Ctrl+C`, retries,
or races. Returning success for `CancelRequested` keeps cancellation simple:
"the job is being cancelled" rather than "your second cancel failed."

### P8 — Job log / event retention

**Superseded by R8 (revised).** The original decision here (durable/ephemeral
class split, 64 MiB JSONL rotation, 10 GiB oldest-first global cap) is replaced
by a single job class, tiered logging (most output is in-memory or referenced
via `nix log`), and a flat 30-day-after-completion window. See R8 for the
current rules.

The only part retained verbatim is the resume-error behavior — ~~`410 Gone`
`ResumeGapTooLarge` (job exists, requested events pruned)~~ *(dropped with
`since_seq`, R14)* and:

- `404 Not Found` — job metadata was pruned.

## Phase 0 Refinements — R1–R15

These refinements were derived by grilling the locked Q1–Q5 / P1–P8 against the
beta implementation on `master`. They **revise** specific locked decisions
(noted per item) and are themselves locked under the same terms. Where a
refinement and an earlier Q/P conflict, the refinement wins.

The unifying principle: **every operation that touches external reality
(Nix/Podman/WSL fetch, eval, build, start) is a job; the event stream is
observability only; every actionable outcome is the job's terminal result —
exactly one of a typed `error` or a typed `output`.** Because every "question"
(which module? which secret?) is produced by an evaluation and the evaluation
*is* the job's work, questions always arrive at a job boundary, never
mid-stream. The client loop is always: tail logs → on terminal, either
retry-with-more-input, read the output, or surface the error.

### R1 — Jobs carry a typed success output

`JobView` carries `output: Option<O>`. A terminal job is exactly one of:
`Failed` with `error: Some(_)`, or `Succeeded` with `output` set (or `None`
for kinds with no output).

`JobView` is **generic over its success payload**: `JobView<O = JobOutput>`.
This makes the success type visible at the call site. Kind-specific service
methods return a narrowed view — `rebuild -> JobView<Rebuilt>`,
`resolve_config -> JobView<ConfigResolution>`, `prepare_exec -> JobView<ExecPlan>`,
`doctor_scan -> JobView<DoctorReport>`, `migration_run -> JobView<MigrationSummary>`,
`update -> JobView<Updated>` — and kinds with no payload return `JobView<()>`
(`create`/`clone`/`delete`/`activate_generation`/`doctor_fix`). The kind-erased
`get_job` returns the default `JobView` = `JobView<JobOutput>`, where `JobOutput`
is a `#[serde(tag = "kind")]` enum with one **newtype** variant per producing
`JobKind`, each wrapping that kind's payload type (`Rebuilt`, `Updated`,
`ConfigResolution`, `GarbageCollected`, `MigrationSummary`, `DoctorReport`,
`ExecPlan`). Because the variants are newtypes over the same payload structs the
typed views carry, a `JobView<Rebuilt>` and a `get_job` `JobView<JobOutput>` of
the same job describe identical data; the typed form simply omits the redundant
`kind` discriminator the endpoint already implies. Adding a variant / payload
type is additive (Q5).

Rationale: for most jobs the real payload is data, not just a side effect —
`doctor_scan` → `DoctorReport`, `update` → new generation + lock diff,
`garbage_collect` → bytes freed, `migration_run` → summary, `rebuild`/`clone`
→ new `GenerationId`, `resolve_config` → `ConfigResolution`, `prepare_exec`
→ `ExecPlan`. This also removes the awkward "run `doctor_scan`, then call sync
`doctor` to read what it cached" round-trip (sync `doctor` remains, as the
cheap cached-findings read for the tray badge).

### R2 — Event streams are informational only

The job event stream (Q2) carries logs + progress only. **Nothing on the
stream requires client reaction.** All control flow is in the terminal
`JobView`: `error` (typed `ApiError`) xor `output` (typed `JobOutput`). This
eliminates any need for an interactive/bidirectional channel (no `Question`
events, no server→client requests).

### R3 — `resolve_config` is a read-only job (revises the Q4 endpoint set)

Module resolution (which module(s)? follow which nixpkgs?) is discoverable only
by fetching and evaluating a flake — external reality, slow, fallible, worth
streaming. It is a **read-only job**
`resolve_config(ResolveConfigRequest) -> JobView` whose typed output is
`ConfigResolution`. Used by `init`, `clone`, `module add`, `module set` (every
flow that introduces/changes a module pointing at a flake URL); **not** by
`rebuild`/`update` (those never re-choose modules — they only refresh the
secret schema, which is R4 state). Chosen over carrying the choice in a typed
error because the resolution payload is rich and reused by four flows.

### R4 — Secrets are NixOS-declared; schema cached in SQLite (refines Q3)

Beta secrets are not free-form KV: they are values for keys **declared by the
machine's NixOS config** (`codchi.secrets.env`), each with a `description`
(`secrets.rs::EnvSecret`). The declared schema is **cached into SQLite during
build/eval**, so the four secret endpoints stay genuine **sync** SQLite ops
(honoring the P6 no-probe invariant). The schema is refreshed only by a build
(rebuild) job.

`SecretKey { name: SecretName, description, has_value }` (R12). `set_secret`
validates the key against the cached schema → `ApiError::Validation` on unknown
keys; `SecretName` adds a syntactic boundary guard ahead of that (R12).

### R5 — All secrets required; enforced at start, never at build (refines Q3/Q4)

All declared secrets are required (beta parity); there is **no** per-secret
`required` flag. Beta collects them by prompting mid-build
(`machine.rs::build`); that is impossible over a non-interactive API, so the
enforcement point moves to **start**:

- Build jobs (`create`/`rebuild`) **succeed** with unset secrets; they become
  state (`has_value == false`). No prompt, no failure.
- The **implicit start** (inside `prepare_exec` / `rebuild`-with-local-modules
  / `clone`) fails with `MissingRequiredSecrets { machine, keys }` if any
  declared secret is unset. The CLI prompts (masked), calls `set_secret`, and
  retries the start. This is the **only** place secrets are gathered — there is
  no proactive post-build prompt.

### R6 — No `Start`/`Stop`/`Restart` job

Starting a machine is never its own job (beta has no `start` command either).
It is a **step** inside `rebuild` (local modules), `clone`, and `prepare_exec`.

### R7 — `prepare_exec` is a job (revises Q4: previously unclassified)

`prepare_exec(PrepareExecRequest) -> JobView`, `JobKind::PrepareExec`, with
`ExecPlan` as its typed output. It owns the exec-path implicit start and
start-time secret enforcement, and streams boot/systemd logs (valuable on WSL).

This makes the Q4 rule **uniform with no exceptions**: every operation that
touches external reality is a job. `prepare_exec` is **not** an exclusive
writer (P7 rule 2) — concurrent exec sessions into one running machine are
normal; only the cold-start sub-step is coalesced, and it yields `MachineBusy`
only if the machine is stopped while a writer job holds it. The fast path stays
fast: when the machine is already running the job completes near-instantly, so
the CLI sees an already-terminal `JobView` and reads the `ExecPlan` directly.

### R8 — One job class; tiered logging; flat retention (revises P8)

There is **one job class**. The durable/ephemeral split is dropped and `JobView`
gains no `retention_class`. `prepare_exec` is just another `JobKind`; it cannot
swamp anything because it persists almost nothing (no build, no eval). The two
concerns R8 originally bundled both dissolve on inspection: execs barely produce
log bytes (the hot path is "machine already running" → near-instant, no logs),
and there is no `list_jobs` endpoint to pollute.

**Logs are a debugging aid, not an audit trail.** Codchi does not re-store what
Nix already keeps. Job output is split into three tiers by where it lives and
whether it is durable:

| Tier | Source | Lives where | Durable? |
|---|---|---|---|
| Aggregate progress | every nix progress message (`start` / `setExpected` / `result:progress` / `stop`), folded into one in-memory counter that is a **superset** of nix progress (also spans codchi phases: eval → build → activate → start) | in-memory; emitted as **throttled** `Event::Progress` snapshots | no |
| Raw build firehose | nix `BuildLogLine` / post-build / fetch lines | **~50-line in-memory ring** per running job (live tail + short reattach) | no — historic build output via `nix log <drv>` |
| Relevant events | `Event::Phase` boundaries, `Event::Log` for nix `Msg{Error,Warn}` incl. **eval errors**, `Event::NixBuild{drv}` references, `Event::StateChange`, `Event::HealthFinding` | JSONL on disk, indexed by SQLite | **yes** (tiny) |

Rules:

- The server **consumes the full** nix progress stream — it must, because the
  aggregate counter is only correct if it sees every `start` (activity began),
  `setExpected` (total), intermediate `result:progress`, and `stop`. Progress
  **deltas are never persisted**; only the throttled aggregate is streamed live.
- Nix **evaluation** errors happen before a `drv` exists, so `nix log` cannot
  recover them. Codchi captures and **persists** them (they are usually the
  failure reason and feed the terminal `JobView.error`).
- Raw build output is **live-only** (the ring). It is not written to JSONL and
  not in replay; historic build logs are fetched via the persisted `drv`
  reference. The structured messages carry the context that matters.
- **Replay semantics (refines Q2; `since_seq` dropped by R14):** a live follower
  sees raw build output from the ring; a `tail` replay returns only the persisted
  relevant events (+ eval errors + `drv` refs), never the raw build firehose.
  Consistent with R2 (streams are informational).
- **Retention: a flat time window.** Every job's persisted log + metadata is
  kept **30 days after the job reaches a terminal state**, then pruned —
  regardless of kind or outcome. Running jobs are retained until they finish.
  Because the persisted set is tiny, P8's 64 MiB rotation, 10 GiB cap, and
  oldest-first pruning are **dropped**; a small safety cap may bound pathological
  cases but is not the primary mechanism.
- `404 Not Found` (job/source absent) still applies. ~~`410 Gone`
  `ResumeGapTooLarge`~~ was dropped with `since_seq` (R14).

This **supersedes P8** (durable/ephemeral classes, 64 MiB rotation, 10 GiB cap,
oldest-first prune) and **refines Q2** (replay returns the persisted tier, not
the raw build firehose).

### R9 — Failed-create lifecycle; new-job retry; structural idempotency (resolves the former "Open: idempotency")

The former open item was mis-framed as "idempotency / resumability." Resolved:

- **A retry is always a new job** with a new `JobId`. There is no resume, no
  waiting state, no server→client "needs input" signal. This is the only model
  consistent with the locked R2 (streams informational) and the rule "jobs must
  have all required input before they start"
  ([../03-jobs-logs-doctor.md](../03-jobs-logs-doctor.md)). The word
  "resumability" is retired.
- **No idempotency machinery** — no idempotency keys, no cross-job step
  checkpoints, no resume tokens. `JobView` and requests gain no such surface.
  Cheap retries are **structural**:
  - cheap preflight (secrets / modules) runs before expensive external work, so
    the `fail → gather → retry` loop fails before the costly steps;
  - Nix's content-addressed store + profile generations make a repeated build a
    near-noop;
  - non-update jobs never run `nix flake update`, so a retry can't move
    `flake.lock` (upholds the invariant that failed updates don't advance the
    lock);
  - each `JobKind` is individually responsible for skip-if-already-done on its
    non-Nix steps (e.g. `clone` skips an existing target container).

Because R3 made module resolution a separate read-only job and R5 made build
jobs succeed with unset secrets (enforced at start), **`create_machine` never
pauses for input mid-flight** — it receives all inputs up front and runs to
success or fails. The beta "cancelled-for-input mid-`build` leaves artifacts"
case **cannot occur in v1**.

**No "partially installed" limbo.** A machine is exactly one of:

- *valid* — `active_generation == Some(_)`;
- *creating* — `active_generation == None` **and** `busy_with == Some(<create job>)`;
- *failed create* — `active_generation == None` **and** `busy_with == None`.

A failed / cancelled `create_machine`:

- **default:** the create job's own failure-cleanup tears the machine down
  completely (beta behavior, as the deliberate rule) — no row, no artifacts;
- **`keep_on_fail: bool`** (additive field on `CreateMachineRequest`, default
  `false`): retains the failed machine row + artifacts + a `create.failed`
  finding for introspection. Such a machine permits only `get` / `doctor` /
  `delete`; it is not execable or rebuildable. Retry is `delete` then `create`.

A failed `rebuild` / `update` of an **already-valid** machine never turns it into
a failed machine: it stays `Running` / `Stopped` on its prior generation, the
failure is a job error (+ optional finding), and `flake.lock` is not advanced.

### R10 — MachineView status axes (replaces the flat `MachineStatus`)

The flat `MachineStatus { Stopped, Running, Building, NeedsRebuild, Failed }`
conflated independent axes and could not express the *normal* post-edit state
"Running **and** NeedsRebuild." Beta already kept these separate
(`PlatformStatus` and `ConfigStatus` in `machine.rs`). v1 uses **three
orthogonal axes** plus the active findings, all on `MachineView`:

- `run_status: RunStatus` — `Stopped | Running` (platform liveness; from the
  reconciler snapshot, P6).
- `update_status: UpdateStatus` — `UpToDate | NeedsRebuild | UpdatesAvailable`
  (desired-vs-built; beta's `ConfigStatus`).
- `findings: Vec<Finding>` — the machine's **active** findings, carried on the
  view and **authoritative** for health.

`health` is **not stored state**. It is a pure function
`fn health(&[Finding]) -> Severity` (worst active severity; `Ok` when empty),
defined **once** in `codchi-api` so CLI, tray, and server agree.

`Building`, `Creating`, and `Failed` are **not** status variants — they are
derived (see R9) from `active_generation` + `busy_with`; `busy_with: Some(job)`
plus the job's `JobKind` describes any in-flight transition. The updated
`MachineView` / `MachineDetail` sketch is in *Machine + Generation Views* below.

### R11 — Logs are source-keyed; jobs are subject-tagged (adds log sources; revises the Job/Event model)

Raised by the Phase 1 grill (`phases/01-http-vertical-slice.md` D9, recorded
there as **CR1**). Grounded in the beta (`master`) survey of what emits logs:
the long-lived emitters are the **server**, the **store container**, and each
**machine container**; the short-lived emitters (nix builds, control commands)
are *operations* against one of those.

Model:

- A **log source** is a long-lived entity with a durable log stream:
  `Server | Store | Machine(MachineId)`. Each gets R8 tiering. The CLI's own
  per-command terminal output is client-side and is **not** a source.
- A **job** is an *operation against a source* — it carries an explicit
  `subject: LogSource`, and its events are written into that source's log,
  correlated by `job_id`. There is no more machine-only coupling.
- `stream_logs(source)` = everything the source ever emitted (a job's output
  *and* ongoing chatter); `stream_job_events(job_id)` = the correlated subset for
  one operation. Two lenses over one log store. Store startup is a
  `Store`-subject job; post-startup store chatter is more `Store`-source log
  with no job id.

Contract changes (this **revises** the Q4 service surface and the Job model;
expected to trip the `oasdiff` gate as an intentional Phase 0 revision):

- New `LogSource { Server, Store, Machine(MachineId) }` (adjacently tagged
  `{"type":..,"id":..}` because the `Machine` newtype wraps a transparent
  string; `Display`/`FromStr` give the `server` / `store` / `machine-<id>` path
  form). New `LogSourceKind { Server, Store, Machine }` (id-less, for filtering).
- `JobView.machine: Option<MachineId>` → **`subject: LogSource`** (non-optional;
  machine-less operations like `resolve_config` / `doctor_*` / `migration` are
  `Server`-subject, store ops are `Store`-subject).
- New `JobKind::StoreStart`, `JobKind::StoreRecover` (additive per Q5).
- New service methods + routes:
  - `list_jobs(JobFilter) -> Vec<JobView>` — `GET /v1/jobs`, query `JobFilter
    { subject: Option<LogSourceKind>, machine: Option<MachineId>, active_only:
    Option<bool> }`. Answers "what is the store/server currently doing".
  - `stream_logs(LogSource, EventStreamOpts) -> EventStream` — `GET
    /v1/logs/{source}`, NDJSON `Event`. No separate bounded-query endpoint:
    `EventStreamOpts.follow = false` already does a bounded `tail` replay
    (R14 dropped `since_seq`), exactly as for `stream_job_events`.
- `remoc` is dropped entirely as a transport (doc 01); the beta `ipc`
  logging/health types are not carried into the v1 contract.

Phasing of the *implementation* (the contract is defined in full now) is in
`phases/01-http-vertical-slice.md` D14: Phase 1 implements the source side for
`Server`/`Store` (JSONL + `stream_logs`); jobs → Phase 5, SQLite-indexed tiering
→ Phase 9, `Machine` source → Phase 7. `ROUTES.len()` becomes **28**.

### R12 — Secret key is a validated `SecretName` newtype (tightens R4; satisfies P3/P4)

Raised during the Phase 1 C1 path-parameter work: the three secret endpoints
(`get`/`set`/`delete_secret`) carried their key as a raw `String` in both the
`CodchiService` surface and the `{key}` path segment, violating the P3/P4
newtype rule ("raw `String` / `Uuid` must not appear in the contract"). It was
also the *only* path segment whose value can contain a **reserved** URI
character: the NixOS `codchi.secrets.env` option validates names against
`strMatching "^[a-zA-Z0-9:_.-]*$"`, so a declared name may legitimately contain
`:` — which the path renderer must percent-encode (`%3A`) rather than emit raw.

Decision: introduce **`SecretName`** — a `#[serde(transparent)]` newtype over
`String` (in `dto/secret.rs`), mirroring `MachineId`:

- `SecretName::new` / `validate(field)` enforce the syntactic shape (non-empty,
  ≤255 chars, charset `[A-Za-z0-9:_.-]`, mirroring the Nix module). As with
  `MachineId`, validation is **not** run in `Deserialize`, so malformed input
  surfaces as a typed `ApiError::Validation` at the boundary. This is a
  *syntactic* guard; the authoritative check stays schema membership (R4),
  performed by the server.
- `SecretKey.name: SecretName` (was `String`); the three service methods take
  `key: SecretName` (was `String`); the `{key}` path segment is typed
  `SecretName`. The `PathSegment` impl for raw `String` is removed, so **no**
  path segment is an unvalidated `String`.

This is **wire-compatible**: `#[serde(transparent)]` keeps `SecretName` a plain
string on the wire and inlines it in OpenAPI (path params are already generic
strings), so `openapi.json` is byte-identical and the `oasdiff` gate does **not**
trip. It is a Rust-contract tightening only — no API major bump (additive per
Q5 in the wire sense). `ROUTES.len()` is unchanged at **28**.

### R13 — `JobView<O>` deserialize bound override (wire-neutral correctness fix)

Raised during Phase 1 C4 (the typed HTTP client), the first code to *deserialize*
a typed `JobView<O>` — the mock and the OpenAPI generator only ever serialize or
schema-gen it. `JobView` carries `#[serde(default)]` on `output: Option<O>`; from
that, serde's derive infers an over-eager `O: Default` bound on the generated
`Deserialize` impl (it cannot see that the default it needs is `Option::<O>::None`,
which requires nothing of `O`). No payload type (`Rebuilt`, `Updated`,
`ConfigResolution`, `ExecPlan`, `MigrationSummary`, `DoctorReport`, or the erased
`JobOutput`) is `Default`, so `JobView<O>` was effectively undeserializable — the
client could not decode any job-returning endpoint.

Decision: pin the deserialize bound explicitly on the struct:

```rust
#[serde(bound(deserialize = "O: serde::Deserialize<'de>"))]
pub struct JobView<O = JobOutput> { … }
```

This is the bound the contract always intended (a typed `JobView<O>` decodes for
any `O: Deserialize`). It is **wire-neutral**: it touches only the `Deserialize`
impl, not `Serialize`/`JsonSchema`, so `openapi.json` is byte-identical and the
`oasdiff` gate does not trip. Locked by a contract test (`typed_job_view_deserializes`).

### R14 — Drop `since_seq` resume and `ResumeGapTooLarge` (simplifies Q2; revises R8)

Raised during Phase 1 C7 (source-log capture). The original Q2 stream contract
carried three knobs — `tail`, `since_seq`, `follow` — plus a `410 Gone`
`ResumeGapTooLarge` for a resume cursor that fell off retention. `since_seq` is a
*reconnect-without-gap* cursor: its only value is resuming a **dropped
connection** mid-stream. That scenario does not meaningfully exist for v1's
clients:

- Every stream is followed over a **per-user Unix domain socket** (D6), which
  does not transiently blip the way a networked HTTP connection does — the
  connection holds until one side deliberately closes.
- A **daemon restart** is not resumable by design: the socket dies, seq is
  per-run (it never persisted a global cursor), and the contract already says a
  client must re-subscribe from `tail` after a hard stream error. So `since_seq`
  could never bridge a restart anyway.
- The realistic clients are the local CLI (runs a command, follows until the job
  ends or `Ctrl+C`) and the tray (Phase 15). Neither needs a byte-exact resume;
  both are well served by `tail` (backfill the last N on attach) + `follow`.

Decision: **remove `since_seq` from `EventStreamOpts`** (leaving `{ tail,
follow }`) and **remove the `ResumeGapTooLarge` error variant**, which had no
trigger other than a `since_seq` resume past retention. `EventSeq` is **kept** —
every `Event` still carries a monotonic `seq` for NDJSON ordering and to anchor
`tail`'s "last N". The two streaming endpoints (`stream_job_events`,
`stream_logs`) lose `since_seq` together, since they share `EventStreamOpts`.

Consequences:

- `tail` + `follow` are the whole stream contract. `follow: false` does a
  bounded `tail` replay and then ends (finite NDJSON); `follow: true` backfills
  `tail` then keeps streaming. No retention-gap error path remains.
- This **supersedes the Q2 `since_seq` / `410 ResumeGapTooLarge` rules** and the
  R8 bullet that referenced `tail` / `since_seq` replay + `410`. R8's tiering
  (in-memory ring vs. durable JSONL) is otherwise unchanged; what's persisted is
  still the relevant-event tier, now replayed by `tail` alone.
- **Breaking wire change**: drops a query param and an error code, so it trips
  the `oasdiff` gate. Annotated as an intentional Phase 0 revision, like R11.
  `openapi.json` regenerated.

### R15 — Error and finding codes are typed catalogs

Raised after Phase 1 exposed the first real infrastructure finding. The wire
contract correctly used stable machine-readable strings, but Rust represented
those identities in two inconsistent ways:

- `ApiError` was typed, while `ApiError::code()` duplicated Serde's derived
  discriminator in a second string-literal match.
- `Finding.code` was a raw `String`, so construction, deduplication, clearing,
  repair selection, and future boot-failure throttling all accepted typos.

Decision:

- Every `ApiError` variant declares its existing wire code with an explicit
  `#[serde(rename = "...")]`. Remove `ApiError::code()`; callers match the
  typed variant, while contract tests assert every serialized discriminator.
- Introduce the closed `FindingCode` enum and change `Finding.code` from
  `String` to `FindingCode`. Each variant has an explicit dotted wire name.
  The initial catalog is the finding vocabulary already locked by P6/R9:
  `store.unavailable`, `podman.container_missing`, `podman.mount_missing`,
  `podman.gcroot_missing`, `wsl.distro_missing`, `wsl.rootfs_missing`,
  `generation.store_path_missing`, `reconcile.probe_failed`, and
  `create.failed`.
- Adding a stable finding code requires adding a `FindingCode` variant and a
  contract stability case. Free-form messages and suggested actions remain
  strings and are not identities.

Existing catalog values remain identical on the wire. The `ApiError` change is
wire-neutral. The `Finding.code` schema narrows from any string to the catalog,
which is an intentional OpenAPI tightening while v1 is still locked
pre-release. The pinned `oasdiff` gate classifies this as response-enum
warnings, not errors, so no API major bump is required. `openapi.json` is
regenerated.

## Crate Structure

```
crates/codchi-api/
├── Cargo.toml          # no platform deps, no axum, no reqwest, no rusqlite
├── src/
│   ├── lib.rs
│   ├── version.rs      # API_VERSION constant
│   ├── ids.rs          # newtype IDs
│   ├── error.rs        # ApiError + stable error code catalog
│   ├── events.rs       # Event enum for streams
│   ├── service.rs      # CodchiService trait (semantic surface)
│   ├── endpoints.rs    # typed Endpoint catalog (URL map; single source of truth)
│   ├── openapi.rs      # OpenAPI generation entry point
│   ├── testing.rs      # MockCodchiService
│   └── dto/
│       ├── mod.rs
│       ├── server.rs
│       ├── machine.rs
│       ├── secret.rs
│       ├── generation.rs
│       ├── job.rs
│       ├── exec.rs
│       ├── doctor.rs
│       └── migration.rs
└── tests/
    └── contract.rs     # serde roundtrip + OpenAPI snapshot
```

## Crate Rules

- No `axum`, no `reqwest`, no `rusqlite`, no platform-specific deps. Only
  `serde`, `serde_json`, `thiserror`, `async-trait`, `chrono` (see P2),
  `uuid`, `schemars`, and `aide` core (see P1).
- All wire types derive `Debug`, `Clone`, `Serialize`, `Deserialize`, and
  `schemars::JsonSchema`.
- Wire format is JSON. Field names `snake_case`.
- Enums are tagged: `#[serde(tag = "kind", rename_all = "snake_case")]` for
  variant-bearing payloads; plain string enums for status-style fields.
- Clients must ignore unknown fields. `#[serde(deny_unknown_fields)]` is
  forbidden on response types. Use `#[serde(default)]` for additive fields.
- All IDs are newtypes. Raw `String` / `Uuid` must not appear in DTOs.
- Timestamps are RFC 3339 in UTC on the wire; internal type is
  `chrono::DateTime<chrono::Utc>` (see P2).
- `EventSeq` is wire-stable `u64`, monotonic per job.
- UUIDs are **v7** (time-ordered). Applies to `JobId`, `FindingId`, and any
  future UUID-shaped ID. Logs and job lists sort chronologically by default.

## Type Sketches

These are non-binding sketches showing the intended shape. **Wire-stable field
names and enum variants** are the contract; struct layout and Rust ergonomics
are not. Exact fields are finalized when the crate is implemented.

### IDs

```rust
pub struct MachineId(pub String);      // user-visible name, see Q1
pub struct JobId(pub Uuid);
pub struct GenerationId(pub u64);      // monotonic per machine
pub struct StoreGenerationId(pub u64);
pub struct EventSeq(pub u64);          // per-job / per-source monotonic (R11)
pub struct FindingId(pub Uuid);
```

### Error Catalog

```rust
#[serde(tag = "code")]
pub enum ApiError {
    #[serde(rename = "machine_not_found")]
    MachineNotFound { machine: MachineId },
    #[serde(rename = "machine_busy")]
    MachineBusy { machine: MachineId, job: JobId },
    #[serde(rename = "job_not_found")]
    JobNotFound { job: JobId },
    #[serde(rename = "job_not_cancellable")]
    JobNotCancellable { job: JobId, state: JobState },
    #[serde(rename = "store_unavailable")]
    StoreUnavailable { reason: String },
    #[serde(rename = "store_busy")]
    StoreBusy { job: JobId },
    #[serde(rename = "schema_migration_required")]
    SchemaMigrationRequired { current: u32, required: u32 },
    #[serde(rename = "api_version_mismatch")]
    ApiVersionMismatch { client: u32, server: u32 },
    #[serde(rename = "missing_required_secrets")]
    MissingRequiredSecrets { machine: MachineId, keys: Vec<SecretKey> }, // R5
    #[serde(rename = "validation")]
    Validation { field: String, message: String },
    #[serde(rename = "internal")]
    Internal { message: String },
}
```

Each variant explicitly declares its wire `code` string (R15). Codes are stable
across non-breaking releases per Q5. This list is the authoritative catalog;
new codes require an entry here.

### Service Trait

```rust
#[async_trait]
pub trait CodchiService: Send + Sync {
    // server lifecycle
    async fn server_status(&self) -> Result<ServerStatus, ApiError>;

    // machines
    async fn list_machines(&self) -> Result<Vec<MachineView>, ApiError>;
    async fn get_machine(&self, id: &MachineId) -> Result<MachineDetail, ApiError>;
    async fn create_machine(&self, req: CreateMachineRequest) -> Result<JobView<()>, ApiError>;
    // source is the {id} path segment; req carries only the new name
    async fn clone_machine(&self, source: &MachineId, req: CloneMachineRequest) -> Result<JobView<()>, ApiError>;
    async fn delete_machine(&self, id: &MachineId) -> Result<JobView<()>, ApiError>;

    // modules / config / secrets
    async fn set_modules(&self, id: &MachineId, req: SetModulesRequest) -> Result<(), ApiError>;
    async fn set_secret(&self, id: &MachineId, key: SecretName, value: String) -> Result<(), ApiError>; // R12
    async fn get_secret(&self, id: &MachineId, key: SecretName) -> Result<String, ApiError>;            // R12
    async fn list_secrets(&self, id: &MachineId) -> Result<Vec<SecretKey>, ApiError>;
    async fn delete_secret(&self, id: &MachineId, key: SecretName) -> Result<(), ApiError>;             // R12

    // build / update / activation
    // id is the {id} path segment; rebuild/update take no body
    async fn rebuild(&self, id: &MachineId) -> Result<JobView<Rebuilt>, ApiError>;
    async fn update(&self, id: &MachineId) -> Result<JobView<Updated>, ApiError>;
    async fn list_generations(&self, id: &MachineId) -> Result<Vec<GenerationView>, ApiError>;
    async fn activate_generation(&self, id: &MachineId, gen: GenerationId) -> Result<JobView<()>, ApiError>;

    // store generations (read-only for v1; activation/rollback is internal)
    async fn list_store_generations(&self) -> Result<Vec<StoreGenerationView>, ApiError>;

    // config resolution (R3): read-only flake fetch+eval; output is ConfigResolution
    async fn resolve_config(&self, req: ResolveConfigRequest) -> Result<JobView<ConfigResolution>, ApiError>;

    // exec (R7): PrepareExec job; id is the {id} path segment; req carries only the command
    async fn prepare_exec(&self, id: &MachineId, req: PrepareExecRequest) -> Result<JobView<ExecPlan>, ApiError>;

    // jobs (kind-erased: get_job returns the default JobView = JobView<JobOutput>)
    async fn list_jobs(&self, filter: JobFilter) -> Result<Vec<JobView>, ApiError>;       // R11
    async fn get_job(&self, id: &JobId) -> Result<JobView, ApiError>;
    async fn cancel_job(&self, id: &JobId) -> Result<(), ApiError>;
    async fn stream_job_events(&self, id: &JobId, opts: EventStreamOpts)
        -> Result<BoxStream<'static, Result<Event, ApiError>>, ApiError>;

    // logs (R11): source-keyed stream; stream_job_events is the job-correlated view
    async fn stream_logs(&self, source: LogSource, opts: EventStreamOpts)
        -> Result<BoxStream<'static, Result<Event, ApiError>>, ApiError>;

    // doctor
    async fn doctor(&self, opts: DoctorOpts) -> Result<DoctorReport, ApiError>;
    async fn doctor_scan(&self, opts: DoctorOpts) -> Result<JobView<DoctorReport>, ApiError>;
    async fn doctor_fix(&self, finding: FindingId) -> Result<JobView<()>, ApiError>;

    // migration
    async fn migration_plan(&self) -> Result<MigrationPlan, ApiError>;
    async fn migration_run(&self, opts: MigrationOpts) -> Result<JobView<MigrationSummary>, ApiError>;
}
```

Sync vs. job mapping follows the Q4 table. The server implements this trait
directly. The HTTP client implements the same trait over the wire. Tests use
`MockCodchiService`.

URL mapping is a **typed endpoint catalog** in `endpoints.rs` (one zero-sized
marker type per route implementing an `Endpoint` trait that names the route's
method, path, and `Body`/`Query`/`Response` types), the single source of truth
reused by the `axum` router, the typed client, and OpenAPI. Endpoints are matched
by type, not by stringly-typed `operation_id`. Example mapping:
`POST /v1/machines` → `create_machine`, `GET /v1/machines/{id}` → `get_machine`,
`GET /v1/jobs/{id}/events` → `stream_job_events`. The `01-architecture.md`
sketch of `CodchiService` is illustrative and superseded by this trait. The
catalog mechanism and the Phase 1 generic router/client are specified in
[../06-api-endpoint-codegen.md](../06-api-endpoint-codegen.md).

**Path is the canonical identity; request bodies never repeat a path
parameter.** A resource addressed by `{id}` (or `{key}`, `{generation}`) takes
that value only from the path — the body carries only data that is not already
in the URL. So `rebuild`/`update` (`POST /machines/{id}/rebuild|update`) take no
body; `clone_machine` (`POST /machines/{id}/clone`) takes the source from `{id}`
and only `{ target }` in the body; `prepare_exec` (`POST /machines/{id}/exec`)
takes the machine from `{id}` and only `{ command }` in the body. `create_machine`
is the one place a `MachineId` lives in the body (`POST /machines` has no path
id, because the resource does not exist yet). This avoids the
path-vs-body ambiguity where the two could disagree.

### Job / Event Model

```rust
pub enum JobState {
    Queued, Running, CancelRequested, CleaningUp,
    Succeeded, Failed, Cancelled,
}

pub enum JobKind {
    Init, Clone, Rebuild, Update, Delete,
    ActivateGeneration, GarbageCollect,
    Migration, DoctorScan, DoctorFix,
    Resolve,       // R3: read-only flake fetch+eval -> ConfigResolution
    PrepareExec,   // R7: ensure store+machine running, session, env -> ExecPlan
    StoreStart, StoreRecover,  // R11: store-subject jobs
}

// R1: generic over the success payload `O` (default JobOutput). Kind-specific
// methods return a narrowed JobView<X>; kind-erased get_job returns JobView<JobOutput>.
pub struct JobView<O = JobOutput> {
    pub id: JobId,
    pub kind: JobKind,
    pub subject: LogSource,   // R11: was `machine: Option<MachineId>`
    pub state: JobState,
    pub created_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub finished_at: Option<DateTime<Utc>>,
    pub error: Option<ApiError>, // R1/R2: Some iff terminal Failed
    pub output: Option<O>,       // R1/R2: Some iff terminal Succeeded (kinds with output)
    pub last_event_seq: EventSeq,
}

// Per-kind payload structs the typed views carry.
pub struct Rebuilt          { generation: GenerationId }
pub struct Updated          { generation: GenerationId, input_changes: Vec<LockInputChange> }
pub struct GarbageCollected { freed_bytes: u64 }
// ConfigResolution, MigrationSummary, DoctorReport, ExecPlan are defined in their dto modules.

// R1: kind-erased terminal output, one newtype variant per producing JobKind,
// each wrapping that kind's payload struct. Additive (Q5).
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum JobOutput {
    ConfigResolution(ConfigResolution),
    Rebuilt(Rebuilt),
    Updated(Updated),
    GarbageCollected(GarbageCollected),
    Migrated(MigrationSummary),
    Scanned(DoctorReport),
    ExecPlan(ExecPlan),
}

#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Event {
    Log           { seq: EventSeq, ts: DateTime<Utc>, level: LogLevel, topic: String, message: String },
    Progress      { seq: EventSeq, ts: DateTime<Utc>, phase: String, done: u64, total: Option<u64> },
    Phase         { seq: EventSeq, ts: DateTime<Utc>, name: String, status: PhaseStatus },
    StateChange   { seq: EventSeq, ts: DateTime<Utc>, state: JobState },
    NixBuild      { seq: EventSeq, ts: DateTime<Utc>, drv: String, status: NixBuildStatus },
    HealthFinding { seq: EventSeq, ts: DateTime<Utc>, finding: FindingId },
}

pub struct EventStreamOpts {       // R14: since_seq dropped
    pub tail: Option<u32>,           // default 200 (Q2)
    pub follow: bool,                // default true
}
```

Adding `Event` variants is non-breaking. Renaming or removing one is breaking
(Q5).

Per R8, these variants split across persistence tiers: `Progress` is a throttled
in-memory aggregate (never persisted per-delta); the raw build firehose is
live-only `Log` (topic `build`) from the ~50-line ring and is **not** replayed
(`nix log <drv>` for history); `Phase`, `StateChange`, `NixBuild{drv}`,
`HealthFinding`, and `Log` for nix `Msg{Error,Warn}` / eval errors are the
durable, replayable tier.

### Machine + Generation Views

```rust
// R10: orthogonal axes; `health` is derived, not stored. No flat MachineStatus.
pub struct MachineView {
    pub id: MachineId,
    pub run_status: RunStatus,                    // Stopped | Running
    pub update_status: UpdateStatus,              // UpToDate | NeedsRebuild | UpdatesAvailable
    pub active_generation: Option<GenerationId>,  // None => creating or failed-create (R9)
    pub findings: Vec<Finding>,                   // active; health = health(&findings)
    pub busy_with: Option<JobId>,
    pub schema_version: u32,
    // P6 snapshot freshness:
    pub last_reconciled_at: Option<DateTime<Utc>>,
    pub last_reconcile_attempt_at: Option<DateTime<Utc>>,
    pub snapshot_stale: bool,
}

pub enum RunStatus { Stopped, Running }
pub enum UpdateStatus { UpToDate, NeedsRebuild, UpdatesAvailable }

// health is derived once in codchi-api; not a wire field of its own state.
// fn health(findings: &[Finding]) -> Severity  // worst active severity, Ok if empty

pub struct MachineDetail {
    pub view: MachineView,                        // carries active findings (R10)
    pub modules: Vec<ModuleSpec>,
    pub secrets: Vec<SecretKey>, // SecretKey { name: SecretName, description, has_value }; R4/R5/R12
    pub flake_lock_hash: String,
    pub generations: Vec<GenerationView>,
}

pub struct GenerationView {
    pub id: GenerationId,
    pub created_at: DateTime<Utc>,
    pub activated_at: Option<DateTime<Utc>>,
    pub flake_lock_hash: String,
    pub system_store_path: String,
    pub status: GenerationStatus, // Active, Inactive, Protected, Deleted
    pub source_job: JobId,
}

pub struct StoreGenerationView {
    pub id: StoreGenerationId,
    pub created_at: DateTime<Utc>,
    pub activated_at: Option<DateTime<Utc>>,
    pub flake_lock_hash: String,
    pub runtime_store_path: String,
    pub status: GenerationStatus, // shared enum: Active, Inactive, Protected, Deleted
}
```

Store generations are **read-only** in v1: `list_store_generations` exposes
them for diagnostics and tray display, but there is no
`activate_store_generation` endpoint. Store-generation activation and
garbage collection are internal to `codchi-server` and driven by the
reconciler / store manager.

### Health / Doctor / Findings

```rust
pub enum Severity { Info, Warning, Error, Critical }
pub enum Component { Server, Store, Machine, Job, Migration }

pub enum FindingCode {
    StoreUnavailable,            // "store.unavailable"
    PodmanContainerMissing,      // "podman.container_missing"
    PodmanMountMissing,          // "podman.mount_missing"
    PodmanGcrootMissing,         // "podman.gcroot_missing"
    WslDistroMissing,            // "wsl.distro_missing"
    WslRootfsMissing,            // "wsl.rootfs_missing"
    GenerationStorePathMissing,  // "generation.store_path_missing"
    ReconcileProbeFailed,        // "reconcile.probe_failed"
    CreateFailed,                // "create.failed"
}

pub struct Finding {
    pub id: FindingId,
    pub severity: Severity,
    pub component: Component,
    pub machine: Option<MachineId>,
    pub source_job: Option<JobId>,
    pub code: FindingCode,             // R15: explicit stable wire names
    pub message: String,               // user-facing, may evolve
    pub suggested_action: Option<String>,
    pub auto_fixable: bool,
    pub created_at: DateTime<Utc>,
}

pub struct DoctorReport {
    pub findings: Vec<Finding>,
    pub generated_at: DateTime<Utc>,
}
```

Stable `FindingCode` variants and their wire strings are the contract;
`message` may be edited freely. Adding a code extends this catalog (R15).

`auto_fixable` is **strictly defined**: a finding is `auto_fixable = true`
only if its repair cannot touch **user data** and cannot rewrite the stored
`flake.lock`. Repair *may* re-extract or replace system-file portions of a
distro/container (the store rootfs, packaged system files inside a machine
distro). It must not touch user data partitions, user-mounted volumes, home
directories, project files, secrets, or anything else the user owns.

The flag advertises that the server's bounded startup auto-repair (see P6)
and the user-invoked `doctor_fix` job are both safe to run for this finding.
After server startup, only `doctor_fix` ever consumes this flag — the
background reconciler is observe-only and never repairs.

## CI / Enforcement

- `cargo test -p codchi-api` runs serde roundtrip tests on every DTO and
  every `ApiError` variant.
- `cargo run -p codchi-api --bin gen-openapi > openapi.json` produces the
  committed snapshot. CI regenerates and diffs on every PR; uncommitted drift
  fails CI.
- `oasdiff breaking old-openapi.json new-openapi.json` classifies changes
  (see Q5). Additive changes pass under `/v1/`; breaking changes fail unless
  the PR bumps `API_VERSION` major and moves affected routes to `/v2/`.
- `MockCodchiService` lives in `codchi-api/src/testing.rs` and returns
  plausible data for every endpoint, so CLI and tray agents can develop
  against the trait without a running server.
- The error and finding-code catalogs above are authoritative. Adding a code
  requires a typed variant and contract stability case.

## Out of Scope for Phase 0

The following are deliberately *not* decided here. They are tracked for later
phases:

- Auth/transport details (Unix socket vs. localhost TCP, named pipe vs. TCP,
  local auth tokens, per-user socket permissions). See Phase 1 / Phase 17.
- SQLite schema. Internal to `codchi-server`. See Phases 3, 4.
- Platform driver internal trait. Internal to `codchi-server`. See Phases 7, 12.
- On-disk layout (JSONL paths, gcroot paths, temp dirs). See Phases 6, 9.
- Tray-specific UI state. The tray derives its view from API events and DTOs;
  there is no tray-shaped surface in `codchi-api`.

## Phase 0 Deliverables

With these decisions locked, Phase 0 produces:

1. `crates/codchi-api/` crate compiling with all DTOs, errors, IDs, `Event`
   variants, and the `CodchiService` trait.
2. Committed `openapi.json` reflecting the above.
3. `MockCodchiService` in-memory fake usable by downstream tracks.
4. CI checks: serde roundtrip per DTO, error/finding-code stability, OpenAPI
   snapshot, `oasdiff` breaking-change classification.

Phases 1, 2, 3, 4, 9, 14 may fan out in parallel once these deliverables land.
