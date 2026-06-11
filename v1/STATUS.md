# Codchi v1 Migration Status

Tracks the current state of the v1 reimplementation. This file is expected to
change often. Stable phase definitions live in [PLAN.md](PLAN.md); locked
per-phase specs live under [phases/](phases/).

**Last updated:** 2026-06-11 (Phase 1 **C6 reworked to the self-contained store** per [phases/01-podman-store.md](phases/01-podman-store.md) S1–S11). The store is now **image-defined and fully self-contained**: a bare `podman run` of `store-podman-image` reaches "nix-daemon usable" with zero server involvement. Removed the entire provisioning subsystem — the host-written `flake.nix`, the in-container `git init`/`nix profile install`/`flake update`/`profile upgrade`, the `runtime` and runtime-`create-files` init stages, `PodmanStore::prepare`/`write_store_flake`, `CODCHI_FLAKE_URL`, and the `/config`+`/data` host binds. The store ships **static `nix` only** (git/openssh/coreutils dropped; S3); the thin PID-1 init is just filesystem → ssl → `exec nix daemon` (S5). `register` now creates `codchi-store` from the image with **only** the `/nix` named volume (S4/S6). The lifecycle skeleton is unchanged (`Arc<dyn Store>` seam, `Starting → Healthcheck → Ready/Degraded`, 15 s observe-only sentinel, `store.unavailable` finding/recovery). Image is **~14 MB compressed**. Verified end-to-end: image builds; manual `podman run` reaches a serving daemon that builds `nixpkgs#hello` (github fetch via built-in libgit2/libcurl, no `git` on PATH) and a forced-local derivation; `git`/`ssh`/`coreutils` confirmed absent; the real `codchi-server` drives the real image to `ready`/store `up` and `codchi status` reflects it. Workspace builds clean; server tests green (lifecycle 5, podman adapter 1, router 2). Also landed the **basic stdio-logging slice of C7**: a `tracing`/`tracing-subscriber` subscriber on stderr (`RUST_LOG`/`CODCHI_DEBUG`-controlled) emitting the daemon listen line + store bring-up/sentinel narrative, replacing the ad-hoc `eprintln!`s. Next: C7 proper (source-log capture → durable `Server`/`Store` logs + `stream_logs`), plus independent C8 bind-mount probe.)

## Overall

The repo is on the `server` branch. The beta CLI-owned implementation and the
early remoc/Podman server skeleton are frozen under `crates/beta/` as reference.
The active v1 server and CLI are clean scaffolds on `codchi-api`; end-user
behavior has not yet been ported.

## Phase Status

| # | Phase | Status |
|---|---|---|
| 0 | Contract design (`codchi-api` crate) | Done — decisions locked + refined (R1–R12) in `phases/00-contract-decisions.md`. `codchi-api` provides the DTOs, IDs, error catalog, event model, semantic service trait, typed endpoint catalog, mock, and generated OpenAPI. It has 18 tests and remains independently gated for clippy, tests, and OpenAPI drift. |
| 1 | HTTP API + Linux/Podman vertical slice | In progress — **C0–C6 complete**. HTTP transport is end-to-end over the Unix socket; `codchi status` auto-spawns with a bounded readiness wait and the daemon now brings up and monitors the real Podman store while machine data remains mocked. Next on the critical path: **C7 source-log capture**; plus the independent **C8 rootless bind-mount probe**. |
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
  C6 adds the `Store`/`PodmanStore` platform boundary, `StoreManager`, real
  startup and the 15 s sentinel; `server_status` overlays lifecycle, store
  status, findings summary, and startup error from this in-memory
  infrastructure snapshot. Per the store rework (S1–S6) `PodmanStore` is now
  provisioning-free: it loads/creates/starts `codchi-store` from the
  self-contained image with only the `/nix` named volume and health-probes the
  in-container Nix daemon. `codchi-shared` owns the agreed socket path, XDG
  host paths, fixed Podman resource names, and checked command helper.
- **Beta reference:** `beta-codchi`, `beta-codchi-server`, `beta-ipc`,
  `beta-shared`, `beta-codchi-gui`, and `beta-codchiw` live under
  `crates/beta/`, are workspace-excluded, and are not expected to compile.
- **Linux packaging/CI:** `packages.default` contains `codchi-server` and
  `codchi`; `store-podman-image` is the self-contained store image (static
  `nix` + bootstrap, embedding `codchi-container-utils`'s `ndd`), wrapped into
  `codchi-server` as `CODCHI_PODMAN_STORE_IMAGE`. `packages.store-podman` (now an
  empty runtime env after dropping `runtimePackages`) is retained only for S8
  closure measurement and can be dropped.
  Hermetic checks cover the API/OpenAPI contract, active server/CLI/shared
  crates, and formatting. Windows/WSL packaging remains deferred; the
  tag-triggered release workflow is unchanged and will fail until Phase 12+.
- **Logging (basic stdio slice of C7):** `codchi-server` now installs a global
  `tracing` subscriber (`logging::init`) writing human-readable logs to stderr;
  verbosity follows `RUST_LOG`, else `CODCHI_DEBUG` → `debug`, else `info`. The
  daemon's listen line and the store bring-up narrative (creating/starting,
  health-wait, ready, and sentinel down/recover transitions) are emitted as
  events; the ad-hoc `eprintln!`s are gone. C7 proper will add a second tracing
  layer at the same init point to fan `Server`/`Store` events into durable
  source logs + the `stream_logs` ring.
- Machine state is not yet server-owned; no active v1 machine workflow exists.
  The archived beta CLI remains the reference for the old direct-ownership path.
- Tray client is currently disabled.
