# Jobs, Logs, and Doctor

## Job Model

Long-running mutating operations are jobs.

The authoritative `JobKind` catalog lives in
[phases/00-contract-decisions.md](phases/00-contract-decisions.md). Each job
carries a `subject: LogSource` (R11) — `Machine(id)`, `Store`, or `Server`.
Examples:

- init, duplicate, rebuild, update, delete (machine subject)
- activate generation (machine subject)
- garbage collection (store subject)
- store start, store recover (store subject, R11)
- migration, doctor scan, doctor fix (server subject)

Module and secret mutations are **sync**, not jobs (see Q4). Changing modules
changes the derived `ConfigurationStatus` to `NeedsRebuild`; the user starts a
rebuild job explicitly (Phase 4 MS11).

Conflicting jobs are rejected, not queued.

Example conflict response:

```json
{
  "code": "machine_busy",
  "message": "Machine foo is already running job job_123",
  "job_id": "job_123"
}
```

## Job States

Use clear end states:

```text
Queued
Running
CancelRequested
CleaningUp
Succeeded
Failed
Cancelled
```

Avoid using `NeedsRecovery` as a job state. Recovery should be represented as
health/recovery findings linked to a failed or crashed job.

## Cancellation

Explicit cancellation means immediate cleanup.

Rules:

- `Ctrl+C` in the CLI sends a cancel request.
- Cancelled jobs run operation-specific cleanup immediately.
- Cleanup must not delete user data.
- Failed or crashed jobs preserve useful produced artifacts for inspection,
  subject to the operation's explicit cleanup policy. In particular,
  `create_machine` follows
  [Phase 4 MS1](phases/04-machine-state.md#ms1--a-machine-is-the-intended-persistent-development-environment).
- If cleanup fails, create a recovery finding.

If the terminal is closed or the client disconnects without sending cancel, the
job continues.

## Reattach

Reattach means re-streaming status, progress, and logs for an existing job.

It does not mean additional interactive input. Jobs must have all required input
before they start.

Supported log replay:

```text
GET /v1/jobs/{id}/events?tail=200
GET /v1/jobs/{id}/events?tail=1000
GET /v1/jobs/{id}/events?tail=200&follow=false
```

Default replay should be a bounded tail, for example the last 200 events.

## Logs

Logs are a **debugging aid, not an audit trail**, and codchi does not re-store
what Nix already keeps.

**Logs are source-keyed (R11).** A log belongs to a long-lived **source** —
`Server`, `Store`, or `Machine(id)` — each with its own durable stream
(`GET /v1/logs/{source}`). A **job** is an operation against a source: it carries
a `subject: LogSource` and its events are written into that source's log,
correlated by `job_id`. So `stream_job_events(job_id)` is the job-correlated
*subset* of `stream_logs(source)` — two lenses over one store. The tiering below
describes a job's output; the same tiers apply to a source's own (job-less)
output (e.g. ongoing store-container chatter).

Job output is tiered (see **R8** in
[phases/00-contract-decisions.md](phases/00-contract-decisions.md)):

- **Aggregate progress** — every nix progress message is consumed to maintain a
  single in-memory counter (a superset of nix's progress, spanning codchi's
  phases); only throttled `Progress` snapshots are streamed. Deltas are never
  persisted.
- **Raw build firehose** — held in a small (~50-line) in-memory ring per running
  job for live tail / short reattach; **not** persisted. Historic build output
  is fetched via the persisted `drv` reference (`nix log <drv>`).
- **Relevant events** — the only durable tier: `Phase` boundaries, `StateChange`,
  `NixBuild{drv}` references, `HealthFinding`, and `Log` for nix `Msg{Error,Warn}`
  including **eval errors** (which predate any `drv` and so cannot come from
  `nix log`). The context that matters lives in these structured messages.

SQLite indexes the durable tier:

- source (`server` / `store` / `machine-<id>`)
- job id (when the line belongs to a job)
- job kind
- status
- timestamps
- log path
- last sequence number
- cancellation flag

The durable tier is written as JSONL on disk and indexed by SQLite. Events use
the tagged `Event` enum from
[phases/00-contract-decisions.md](phases/00-contract-decisions.md); the `source`
(and `job_id`, when applicable) are implied by the stream endpoint
(`GET /v1/logs/{source}` or `GET /v1/jobs/{id}/events`) rather than repeated per
line.

Example (durable tier):

```json
{"kind": "phase",     "seq": 1840, "ts": "2026-05-13T12:34:56.700Z", "name": "build", "status": "started"}
{"kind": "nix_build", "seq": 1841, "ts": "2026-05-13T12:34:56.750Z", "drv": "/nix/store/...-foo.drv", "status": "started"}
{"kind": "log",       "seq": 1842, "ts": "2026-05-13T12:34:56.789Z", "level": "error", "topic": "eval", "message": "..."}
```

Replay (`tail`; `since_seq` dropped by R14) returns this durable tier only; the
raw build firehose is live-only. Retention is a flat **30 days after job completion**
(R8); since the persisted set is tiny, no log rotation or global byte cap is
needed.

## Doctor

`codchi doctor` is a first-class v1 feature.

Purpose:

- health check the daemon, store, machines, jobs, migrations, and external
  artifacts
- list findings that cannot be automatically recovered
- provide safe remediation suggestions
- preserve user data above all else

Doctor should expose machine-readable output:

```text
codchi doctor --json
```

Doctor should support automatic fixes only for safe cases:

```text
codchi doctor --fix
```

Any fix that may affect user data or container filesystems should require an
explicit command or confirmation.

## Recovery Findings

Recovery findings are persistent records, potentially linked to a failed job.

They should contain:

- severity
- component
- machine id if applicable
- source job id if applicable
- stable code
- message
- suggested action
- whether automatic fix is available

Example:

```text
Store WSL rootfs appears corrupted.
Suggested repair:
  codchi store recover

Machine "foo" could not boot because the shared store is unavailable.
User data was not modified.
To export files:
  codchi recover foo --export <target.tar>
```
