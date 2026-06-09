# Context Glossary

Canonical terms for the Codchi codebase. Glossary only — no implementation
details. See `v1/` for target-architecture design docs.

## Terms

### Beta crates
The pre-v1, CLI-owned implementation, retired under `crates/beta/` with a
`beta-` package prefix (`beta-codchi`, `beta-codchi-server`, `beta-ipc`, …).
Frozen, read-only **reference** — excluded from the cargo workspace and not
expected to compile. Kept for orientation, especially the started Podman work.
Not to be confused with the active v1 crates of similar name.

### codchi-api
The active v1 contract crate: DTOs, error catalog, `Event` model, the
`CodchiService` semantic trait, and the typed `Endpoint` route catalog that is
the single source of truth for routing, the typed client, and OpenAPI. Depends
on neither `axum` nor `reqwest`.

### codchi-server
The active v1 daemon (new, Phase 1+). Owns state and platform reconciliation;
implements `CodchiService` directly via `ServerCore`. Distinct from the retired
`beta-codchi-server` skeleton under `crates/beta/`.

### codchi-cli
The active v1 CLI crate (new, Phase 1+). Package name `codchi-cli`; ships the
binary named `codchi` (the public compatibility surface). A `CodchiService`
client over HTTP. Distinct from the retired `beta-codchi` crate.

### Log source
A long-lived entity that emits a durable log stream: `Server` (the daemon),
`Store` (the store container), or `Machine(id)` (a machine container). Each
source has its own stream under R8 tiering. A source keeps logging across many
operations — store gc, machine init scripts, in-container service output — not
just during a single [[job]]. Client-introspectable via `stream_logs(source)` /
`query_logs(source)`. The CLI's own per-command terminal output is **not** a
source (it is client-side, not server-stored).

### Job
An operation against a [[log-source]] that touches external reality and has a
terminal state (`Succeeded`/`Failed`/`Cancelled`). A job's events are written
into its source's log, correlated by `job_id`. Therefore
`stream_job_events(job_id)` is the correlated *subset* of `stream_logs(source)`
for one operation — two lenses on one underlying log. Store startup is a
`Store`-subject job; the store's post-startup chatter is more `Store`-source log
with no job id.

### Job subject
The [[log-source]] a [[job]] acts on: `Machine(id) | Store | Server`. Replaces
the older machine-only coupling (the beta `JobView.machine: Option<MachineId>`).
Makes "what is the store/server currently doing" answerable via
`list_jobs(filter)`.

### topic
A sub-classifier *within* a source's log stream (e.g. `build`, `gc`), carried on
`Event::Log`. Not a source — a source can emit many topics.