# Codchi v1 Migration Status

Tracks the current state of the v1 reimplementation. This file is expected to
change often. Stable phase definitions live in [PLAN.md](PLAN.md); locked
per-phase specs live under [phases/](phases/).

**Last updated:** 2026-06-10 (Phase 1 **C2 complete**: the six pre-v1 architecture crates are frozen under `crates/beta/beta-*`; the active workspace is exactly `codchi-api`, `codchi-server`, `codchi-cli`, `codchi-shared`, and `codchi-container-utils`; Linux packaging builds the new server+CLI and the Podman store image with `ndd`; active v1 crate, formatting, OpenAPI, and Nix checks are green)

## Overall

The repo is on the `server` branch. The beta CLI-owned implementation and the
early remoc/Podman server skeleton are frozen under `crates/beta/` as reference.
The active v1 server and CLI are clean scaffolds on `codchi-api`; end-user
behavior has not yet been ported.

## Phase Status

| # | Phase | Status |
|---|---|---|
| 0 | Contract design (`codchi-api` crate) | Done — decisions locked + refined (R1–R12) in `phases/00-contract-decisions.md`. `codchi-api` provides the DTOs, IDs, error catalog, event model, semantic service trait, typed endpoint catalog, mock, and generated OpenAPI. It has 18 tests and remains independently gated for clippy, tests, and OpenAPI drift. |
| 1 | HTTP API + Linux/Podman vertical slice | In progress — **C0–C2 complete**. The beta implementation is archived, all five active v1 crates build, and Linux packaging/store-image checks are active. Next: **C3 server skeleton + generic `mount<E>`**; C4 typed HTTP client can proceed in parallel. |
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
  `codchi-shared`, and `codchi-container-utils`. The server and CLI are minimal
  scaffolds depending on `codchi-api`; C3/C4 add their transport behavior.
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
