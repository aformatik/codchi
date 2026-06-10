# Codchi v1 Migration Status

Tracks the current state of the v1 reimplementation. This file is expected to
change often. Stable phase definitions live in [PLAN.md](PLAN.md); locked
per-phase specs live under [phases/](phases/).

**Last updated:** 2026-06-10 (Phase 1 **C5 complete**: `codchi status` works end to end — the CLI dials the per-user socket and, if no daemon is reachable, spawns `codchi-server` detached and waits with a **bounded** readiness poll (`codchi-cli::daemon`); the wait's outer timeout is the A1 hang-forever guard. Closed the D7 wiring: `server_status` now overlays the real `LifecycleHandle` so readiness reports the daemon's true `Starting/Ready/Degraded`. clap surface with a default `Status` subcommand + `--json`. Verified by `tests/spawn.rs` (4 A1 cases) and a real auto-spawn smoke. All crates build; clippy clean workspace-wide; tests green: codchi-api 19, server 2, cli 9. Prior: C3+C4 typed HTTP transport over the socket against the mock.)

## Overall

The repo is on the `server` branch. The beta CLI-owned implementation and the
early remoc/Podman server skeleton are frozen under `crates/beta/` as reference.
The active v1 server and CLI are clean scaffolds on `codchi-api`; end-user
behavior has not yet been ported.

## Phase Status

| # | Phase | Status |
|---|---|---|
| 0 | Contract design (`codchi-api` crate) | Done — decisions locked + refined (R1–R12) in `phases/00-contract-decisions.md`. `codchi-api` provides the DTOs, IDs, error catalog, event model, semantic service trait, typed endpoint catalog, mock, and generated OpenAPI. It has 18 tests and remains independently gated for clippy, tests, and OpenAPI drift. |
| 1 | HTTP API + Linux/Podman vertical slice | In progress — **C0–C5 complete**. HTTP transport is end-to-end over the Unix socket (generic router + typed client, against the mock); `codchi status` auto-spawns the daemon with a bounded, non-hanging readiness wait (A1) and renders lifecycle + store + mock machines. The `STATE` side of the slice is done. Next on the critical path: **C6 real Podman store startup + lifecycle**, then **C7 source-log capture**; plus the independent **C8 rootless bind-mount probe**. |
| 2 | `ServerCore` boundary | Not started |
| 3 | SQLite foundation | Not started |
| 4 | Machine state in SQLite | Not started |
| 5 | Job system MVP | Not started |
| 6 | Build/update generation model | Not started |
| 7 | Linux/Podman machines | Not started |
| 8 | Exec/session model | Not started |
| 9 | Logs/events | Not started |
| 10 | Doctor + recovery findings | Not started |
| 11 | Beta migration | Not started |
| 12 | Windows/WSL server backend | Not started |
| 13 | WSL boot/session components | Not started |
| 14 | CLI parity pass | Not started |
| 15 | Tray client | Not started |
| 16 | Test suites + CI | Not started |
| 17 | Hardening / release | Not started |

## Notes

- **Active workspace:** `codchi-api`, `codchi-server`, `codchi-cli`,
  `codchi-shared`, and `codchi-container-utils`. `codchi-server` (axum/tokio over
  `UnixListener`) and `codchi-cli` (`hyper` typed client) carry the C3/C4
  transport; both expose a lib target so the cross-crate tests link them.
  `codchi-cli` adds the C5 `daemon` module (spawn + bounded `await_ready`), a
  clap command surface (default `status`, `--json`), and the `status` renderer.
  Readiness is real daemon state: the `server_status` handler overlays the
  `LifecycleHandle` (D7). `codchi-shared` owns the agreed socket path
  (`server_socket_path`).
- **Beta reference:** `beta-codchi`, `beta-codchi-server`, `beta-ipc`,
  `beta-shared`, `beta-codchi-gui`, and `beta-codchiw` live under
  `crates/beta/`, are workspace-excluded, and are not expected to compile.
- **Linux packaging/CI:** `packages.default` contains `codchi-server` and
  `codchi`; `store-podman-image` embeds `codchi-container-utils`'s `ndd`.
  Hermetic checks cover the API/OpenAPI contract, active server/CLI/shared
  crates, and formatting. Windows/WSL packaging remains deferred; the
  tag-triggered release workflow is unchanged and will fail until Phase 12+.
- Machine state is not yet server-owned; no active v1 machine workflow exists.
  The archived beta CLI remains the reference for the old direct-ownership path.
- Tray client is currently disabled.
