# Codchi v1 Migration Status

Tracks the current state of the v1 reimplementation. This file is expected to
change often. Stable phase definitions live in [PLAN.md](PLAN.md); locked
per-phase specs live under [phases/](phases/).

**Last updated:** 2026-06-11 (Phase 1 **C7 source-log capture landed** (impl + unit-tested; real-store NDJSON e2e proof still pending) and a **frozen-contract revision R14**. R14 drops `since_seq` resume and the `ResumeGapTooLarge` error from `EventStreamOpts` — over a per-user Unix socket there is no transient reconnect to resume, and a daemon restart drops the socket anyway, so `tail` + `follow` are the whole stream contract; `openapi.json` regenerated, the `oasdiff` break is intentional. C7 adds a server-owned `LogStore` (source-keyed: per-source in-memory ring + `broadcast` fan-out + append-only JSONL under `data_dir()/logs/`, no pruning until Phase 9). The `stream_logs` router branches `Server`/`Store` → the real `LogStore`, `Machine` → the mock (Phase 7). The `Server` source is fed by a `tracing` layer (existing `info!`/`warn!`/`error!` feed both stderr and the log, no duplicate call sites); the `Store` source by a new `Store::attach()` — a supervised `podman logs --follow` follower (`kill_on_drop`, generalizes to WSL's store subprocess) whose lines pass through `classify_store_line` (the v1 `parse_container_log`; full nix-JSON parsing deferred to Phase 6). `attach()` is spawned once after a successful start; post-startup the stream ending is observe-only (P6) — the sentinel owns lifecycle, no auto-restart. `tail`/`follow` (incl. bounded `follow:false`) proven by `tests/source_logs.rs` (5). Re-added `codchi-shared::data_dir()`/`logs_dir()` for the durable JSONL. Server tests: lifecycle 5, router 2, podman 1, source_logs 5; api 15+4; cli roundtrip 5. Still open in C7: the live-Podman proof that `stream_logs(Store)` carries **real** store-startup output. Previously: **C6 reworked to the self-contained store** per [phases/01-podman-store.md](phases/01-podman-store.md) S1–S11). The store is now **image-defined and fully self-contained**: a bare `podman run` of `store-podman-image` reaches "nix-daemon usable" with zero server involvement. Removed the entire provisioning subsystem — the host-written `flake.nix`, the in-container `git init`/`nix profile install`/`flake update`/`profile upgrade`, the `runtime` and runtime-`create-files` init stages, `PodmanStore::prepare`/`write_store_flake`, `CODCHI_FLAKE_URL`, and the `/config`+`/data` host binds. The store ships **static `nix` only** (git/openssh/coreutils dropped; S3); the thin PID-1 init is just filesystem → ssl → `exec nix daemon` (S5). `register` now creates `codchi-store` from the image with **only** the `/nix` named volume (S4/S6). The lifecycle skeleton is unchanged (`Arc<dyn Store>` seam, `Starting → Healthcheck → Ready/Degraded`, 15 s observe-only sentinel, `store.unavailable` finding/recovery). Image is **~14 MB compressed**. Verified end-to-end: image builds; manual `podman run` reaches a serving daemon that builds `nixpkgs#hello` (github fetch via built-in libgit2/libcurl, no `git` on PATH) and a forced-local derivation; `git`/`ssh`/`coreutils` confirmed absent; the real `codchi-server` drives the real image to `ready`/store `up` and `codchi status` reflects it. Workspace builds clean; server tests green (lifecycle 5, podman adapter 1, router 2). Also landed the **basic stdio-logging slice of C7**: a `tracing`/`tracing-subscriber` subscriber on stderr (`RUST_LOG`/`CODCHI_DEBUG`-controlled) emitting the daemon listen line + store bring-up/sentinel narrative, replacing the ad-hoc `eprintln!`s. Next: C7 proper (source-log capture → durable `Server`/`Store` logs + `stream_logs`), plus independent C8 bind-mount probe.)

## Overall

The repo is on the `server` branch. The beta CLI-owned implementation and the
early remoc/Podman server skeleton are frozen under `crates/beta/` as reference.
The active v1 server and CLI are clean scaffolds on `codchi-api`; end-user
behavior has not yet been ported.

## Phase Status

| # | Phase | Status |
|---|---|---|
| 0 | Contract design (`codchi-api` crate) | Done — decisions locked + refined (R1–R12) in `phases/00-contract-decisions.md`. `codchi-api` provides the DTOs, IDs, error catalog, event model, semantic service trait, typed endpoint catalog, mock, and generated OpenAPI. It has 18 tests and remains independently gated for clippy, tests, and OpenAPI drift. |
| 1 | HTTP API + Linux/Podman vertical slice | In progress — **C0–C6 complete; C7 implementation landed** (source-log capture + `stream_logs`, contract simplified by R14), with only the live-Podman NDJSON proof outstanding. HTTP transport is end-to-end over the Unix socket; `codchi status` auto-spawns with a bounded readiness wait; the daemon brings up and monitors the real Podman store and captures its output into a server-owned source log while machine data remains mocked. Remaining: C7 e2e proof + the independent **C8 rootless bind-mount probe**. |
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
- **Logging / source logs (C7):** `codchi-server`'s `logging::init` installs two
  `tracing` sinks, each with its own filter: a stderr console (`RUST_LOG`, else
  `CODCHI_DEBUG` → `debug`, else `info`) and a `ServerLogLayer` that fans
  `codchi_server` events at info+ into the `Server` source log — independent of
  console verbosity. The `LogStore` (in `AppState`) owns the `Server`/`Store`
  sources: each a per-run monotonic-`seq` ring + `broadcast` fan-out + append-only
  JSONL under `data_dir()/logs/` (no pruning until Phase 9). `stream_logs`
  backfills `tail` from the ring then live-follows the broadcast (deduped by
  `seq`); `follow:false` is a bounded replay. The `Store` source is fed by the
  `Store::attach()` follower (`podman logs --follow`, `kill_on_drop`) through
  `classify_store_line`. The router sends `Server`/`Store` to the `LogStore` and
  `Machine` to the mock service (Phase 7). Caveat for clients: a `follow:true`
  stream against the live server is infinite by design — `collect()` it only with
  `follow:false`.
- Machine state is not yet server-owned; no active v1 machine workflow exists.
  The archived beta CLI remains the reference for the old direct-ownership path.
- Tray client is currently disabled.
