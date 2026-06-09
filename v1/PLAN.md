# Codchi v1 Rough Plan

This is a rough delivery plan for moving Codchi from the current `server`-branch
skeleton to the v1 target described in `01-architecture.md` through
`05-testing.md`. It is not a schedule; estimates are person-days for a single
experienced Rust/Nix contributor familiar with this repo. Windows/WSL estimates
are the least predictable.

## Scope

v1 targets:

- CLI and tray clients.
- Linux/Podman and Windows/WSL platforms.
- HTTP/JSON internal API between clients and `codchi-server`.
- SQLite as the single source of truth for Codchi state.
- Jobs, generations, logs, doctor, and beta migration as first-class.
- LXD is not part of v1.

## Milestones

- **M0 — Contracts frozen.** `codchi-api` crate compiles with locked DTOs, error
  codes, service trait, OpenAPI snapshot, and in-memory mock. Downstream work
  can fan out.
- **M1 — Linux v1 alpha.** Phases 1–7 done. HTTP daemon, SQLite-owned state,
  jobs, generations, working Podman machine path. Fresh-install smoke passes.
- **M2 — Linux feature-complete.** Adds exec/session, logs/events, doctor, and
  beta migration on Linux (Phases 8–11).
- **M3 — Windows alpha.** WSL server backend and in-machine components running
  (Phases 12–13).
- **M4 — Release candidate.** CLI parity, tray, full test matrix, hardening
  (Phases 14–17).

## Phases

| # | Phase | Estimate | Notes |
|---|---|---:|---|
| 0 | Contract design (`codchi-api` crate) | 5–8 pd | DTOs, errors, service trait, OpenAPI snapshot, mock. Decisions locked in `phases/00-contract-decisions.md`. |
| 1 | HTTP API + Linux/Podman vertical slice | 12–18 pd | `axum` server, typed client, readiness, event stream, server-owned Podman store startup. CLI does `status` end-to-end. |
| 2 | `ServerCore` boundary | 5–8 pd | Move lifecycle/health out of handlers. Define internal service methods for list/init/rebuild/exec-prep/delete/jobs/doctor. |
| 3 | SQLite foundation | 6–10 pd | DB module, migrations, schema versioning, transaction helpers, persisted global config. No beta machine import yet. |
| 4 | Machine state in SQLite | 10–15 pd | Machines, modules, secrets, flake lock content, platform metadata. New v1 machines read/write via SQLite. Beta path stays separate. |
| 5 | Job system MVP | 12–18 pd | Job table, runner, conflict detection, cancel flags, state machine, bounded event streaming. Rebuild/init/delete become jobs on Linux first. |
| 6 | Build/update generation model | 12–20 pd | Temp flake dirs, SQLite-owned `flake.lock`, safe commit boundaries, generation records, active-generation pointer, gcroot/profile projection. Acceptance: failed update never advances lock or active generation. |
| 7 | Linux/Podman machines | 12–18 pd | Server-owned create/start/stop/delete, mounts, boot checks, rebuild/switch, shortcuts. Acceptance: Linux smoke (init, exec env, persistence, rebuild, delete). |
| 8 | Exec/session model | 8–14 pd | `prepare_exec`, session IDs, env merge, `codchi-session` inside machines. Native `podman exec` / `wsl.exe --exec` only; no HTTP PTY proxy. |
| 9 | Logs/events | 7–11 pd | Tiered logging (R8): in-memory aggregate progress counter, ~50-line raw-output ring, durable relevant-event JSONL with SQLite index. Tail replay + `since_seq` over the durable tier. Nix progress/eval parsing into event DTOs; build output referenced via `drv` (`nix log`). Flat 30-day retention. |
| 10 | Doctor + recovery findings | 8–14 pd | Persistent findings, `codchi doctor`, `--json`, safe `--fix`. Wire store/machine/generation/jobs/logs/migration checks. |
| 11 | Beta migration | 12–18 pd | Planner, dry-run JSON, backups, idempotency, old config/lock/platform import. Preserve old files. |
| 12 | Windows/WSL server backend | 18–30 pd | Server-owned store/machine lifecycle on WSL. `codchi-hostctl.exe`, boot spec flow, no independent fallback repair, structured boot failure reporting. |
| 13 | WSL boot/session components | 15–25 pd | `codchi-machine-init`, `codchi-machine-agent`, `codchi-login-shell`, WSL `codchi-session`. Boot failure dedup/throttle. |
| 14 | CLI parity pass | 10–16 pd | Route all public CLI commands through HTTP. Preserve behavior, JSON output, exit codes, prompt-before-job-start, `Ctrl+C` cancel, reattach. |
| 15 | Tray client | 8–14 pd | Re-enable tray as HTTP client. Daemon status, machines, jobs, health findings, notifications, basic actions. |
| 16 | Test suites + CI | 12–20 pd | Code-only migration tests, API contract / OpenAPI checks, Linux Podman smoke, Windows WSL fresh install, WSL upgrade, scheduled WSL with version metadata. |
| 17 | Hardening / release | 10–18 pd | Remove legacy paths, audit destructive cleanup, improve errors, docs, packaging, upgrade notes, release rehearsal. |

**Total rough range: 164–275 pd.**

## Acceptance Criteria

The hard release gates, not the only tests:

- **Phase 6:** A failed build/update does not advance the stored `flake.lock` or
  the active-generation pointer. Verified by injecting failures at each stage.
- **Phase 7:** Linux fresh-install smoke: init → `exec env` → persistence →
  rebuild → delete leaves no orphan containers, volumes, or gcroots.
- **Phase 11:** Beta migration is idempotent on identical inputs and never
  deletes beta config files.
- **Phase 13:** Repeated IDE/WSL launches into a broken machine produce at most
  one user-visible notification per `(machine, component, code)`.
- **Phase 16:** Windows WSL upgrade test from the previous release to the
  candidate build preserves a sentinel file inside the machine.

## Critical Path and Parallelization

Phase 0 gates everything. Phases 1, 2, 3, 4, 9, 14 can fan out as parallel
agent tracks against the frozen contract. Phases 6 and 7 need 3 and 5 first.
Phases 12 and 13 need 5, 6, 7 stable. Tray (15) and tests (16) trail the
producing phases, but their planning starts earlier.

`PLATFORM` and `STATE` are the two long lines. Keep them separated by the
`CodchiService` trait so platform work cannot contaminate state work and
vice versa.

## Agent-Era Adjustments

When most implementation is agent-driven the totals drop, but unevenly:

- Steps 1, 2, 3, 9, 14 compress to roughly 0.3–0.4x — plumbing-heavy, well-
  specified, easy to verify.
- Steps 6, 11 stay close to 0.7–1.0x — design and empirical work dominate.
- Steps 12, 13 stay close to 1.0x — WSL behavior requires real-machine probes
  agents cannot do.

This only works if Phase 0 lands first. Without a frozen contract, parallel
agents diverge and any time saved is paid back in integration debugging. Plan
for 20–30 pd of human review time across the project — this is not optional
and is not currently itemized in the per-phase estimates.

Pull contract/smoke/migration tests forward into the steps that produce them.
Phase 16 should only carry the WSL upgrade/fresh/scheduled suites by the time
it runs.

## Risks

- **WSL behavior drift.** WSL changes independently of Codchi. Schedule
  recurring WSL runs early (Phase 16 scheduled suite) so regressions are visible
  before release windows.
- **Beta state variance in the wild.** Step 11 fixtures will not catch
  everything. Plan for at least one real-machine migration dry-run pass before
  M4.
- **Contract drift under parallel agents.** OpenAPI CI must reject unannotated
  breaking changes from day one of Phase 0, not bolted on later.
- **Tray stalls on event-stream churn.** If Phase 9 isn't stable when Phase 15
  starts, tray work is wasted. Sequence accordingly.
- **Security/packaging undercount.** Socket permissions, optional local auth
  token, and per-binary packaging are bundled into Phase 17. If any of these
  turn out non-trivial, carve them out rather than letting them slip past
  release.
