# Codchi v1 Migration Status

Tracks the current state of the v1 reimplementation. This file is expected to
change often. Stable phase definitions live in [PLAN.md](PLAN.md); locked
per-phase specs live under [phases/](phases/).

**Last updated:** 2026-06-11 (Phase 1 **C7 source-log capture is complete**, including the live-Podman NDJSON proof. A fresh real `codchi-server` registered and started the self-contained store, reached `Ready` in about one second, and `stream_logs(Store)` returned genuine `nix-daemon` output from the startup health probe and sentinel through `Store::attach()` → `classify_store_line`. `stream_logs(Server)` returned the daemon lifecycle narrative from the tracing layer, both sources wrote matching durable JSONL, and bounded `follow:false` replay closed cleanly. The C7 implementation is a source-keyed `LogStore`: per-source in-memory ring + `broadcast` fan-out + append-only JSONL under `data_dir()/logs/`; `Server`/`Store` route to it while `Machine` remains mocked until Phase 7. Frozen-contract revision R14 reduced `EventStreamOpts` to `tail` + `follow`, with regenerated OpenAPI and an intentional `oasdiff` break. Unit/integration coverage includes five source-log tests; the active workspace tests and clippy were green when C7 landed. Remaining Phase 1 work: the independent **C8 rootless bind-mount probe** and the unchecked combined Definition of Done gates.)

## Overall

The repo is on the `server` branch. The beta CLI-owned implementation and the
early remoc/Podman server skeleton are frozen under `crates/beta/` as reference.
The active v1 server and CLI are clean scaffolds on `codchi-api`; end-user
behavior has not yet been ported.

## Phase Status

| # | Phase | Status |
|---|---|---|
| 0 | Contract design (`codchi-api` crate) | Done — decisions locked + refined (R1–R14) in `phases/00-contract-decisions.md`. `codchi-api` provides the DTOs, IDs, error catalog, event model, semantic service trait, typed endpoint catalog, mock, and generated OpenAPI. It remains independently gated for clippy, tests, and OpenAPI drift. |
| 1 | HTTP API + Linux/Podman vertical slice | In progress — **C0–C7 complete**. HTTP transport is end-to-end over the Unix socket; `codchi status` auto-spawns with a bounded readiness wait; the daemon brings up and monitors the real Podman store and captures real store output into durable source logs exposed through `stream_logs`, while machine data remains mocked. Remaining chunk: the independent **C8 rootless bind-mount probe**, plus the unchecked combined Definition of Done gates. |
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
