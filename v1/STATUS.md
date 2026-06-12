# Codchi v1 Migration Status

Tracks the current state of the v1 reimplementation. This file is expected to
change often. Stable phase definitions live in [PLAN.md](PLAN.md); locked
per-phase specs live under [phases/](phases/).

**Last updated:** 2026-06-12 (Phase 2, the `ServerCore` boundary, is **complete (SC1–SC9)**. `AppState` collapsed to `{ core: Arc<ServerCore> }`; `ServerCore` is the single `impl CodchiService`, owning the store-condition reader, the shutdown token, and the `LogStore`, with every still-unbacked domain delegating to one quarantined internal mock (SC2). The store condition is now a `StoreCondition` sum type advanced by a pure `step` reducer and published over a single-writer `watch` by the reshaped `StoreSupervisor` (SC5–SC7); `lifecycle`/`StoreStatus`/`startup_error`/`store.unavailable` are pure projections. Graceful shutdown (SC8) lands with SIGINT/SIGTERM → `CancellationToken`, `axum` `with_graceful_shutdown`, and a `oneshot`-gated ordered store teardown bounded by a timeout. Tests: 9 pure `step`/projection units + 7 supervisor invariants (D8 no-hang, P6 observe-only, no-orphans, SC8 stop, diagnosability); the SC2 gate passes (`roundtrip.rs` unmodified, `spawn.rs` one line). `cargo clippy -p codchi-server -p codchi-cli -p codchi-shared --all-targets -- -D warnings`, the crate tests, and `cargo fmt --check` all pass. Next: Phase 3, the SQLite foundation.)

## Overall

The repo is on the `server` branch. The beta CLI-owned implementation and the
early remoc/Podman server skeleton are frozen under `crates/beta/` as reference.
The Phase 1 transport and Linux store slice is complete. Machine workflows and
server-owned persistent state have not yet been ported.

## Phase Status

| # | Phase | Status |
|---|---|---|
| 0 | Contract design (`codchi-api` crate) | Done — decisions locked + refined (R1–R15) in `phases/00-contract-decisions.md`. `codchi-api` provides the DTOs, IDs, typed error/finding-code catalogs, event model, semantic service trait, typed endpoint catalog, mock, and generated OpenAPI. It remains independently gated for clippy, tests, and OpenAPI drift. |
| 1 | HTTP API + Linux/Podman vertical slice | **Done — C0–C8 complete.** Typed HTTP transport, bounded daemon spawn/readiness, real Podman store lifecycle, durable `Server`/`Store` source logs, live NDJSON proof, fresh-state packaged smoke, CI gates, and the rootless socket bind-mount probe all pass. Machine data remains mocked by design until the next phases. |
| 2 | `ServerCore` boundary | **Done — SC1–SC9.** `AppState` collapsed to `{ core }`; `ServerCore` is the single `impl CodchiService` (domain modules under `core/`), delegating unbacked domains to one internal mock. `StoreCondition` + pure `step` + single-writer `watch`, owned by the reshaped `StoreSupervisor`; lifecycle/store/findings/startup-error are projections. Graceful shutdown with a `oneshot`-gated ordered teardown. Real-podman start→ready→shutdown→stopped E2E lands as an `#[ignore]`d test. |
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
  C6 added the `Store`/`PodmanStore` platform boundary, real startup and the
  15 s sentinel. **Phase 2** reshaped the Phase-1 `StoreManager` +
  `InfrastructureSnapshot` into the single-writer `StoreSupervisor` over a
  `watch<StoreCondition>`; `server_status` is now the `ServerCore` projection of
  that condition (lifecycle/store/findings/startup-error), not a router overlay
  of a denormalized snapshot. Per the store rework (S1–S6) `PodmanStore` is now
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
- **Rootless socket transport (C8/T1):** Podman 5.8.2 was confirmed rootless
  with UID map `0 → host 1000`, subordinate IDs from host 100000. A host-owned
  `0600` Unix socket was bind-mounted directly at
  `/run/codchi/server.sock`. Both the default mapping (container root) and
  `--userns=keep-id` connected successfully; host peer credentials were
  `uid=1000`, `gid=100` in both cases. The in-machine agent must therefore run
  as an identity mapped to the host socket owner; an arbitrary subordinate UID
  cannot access a `0600` socket.
- Machine state is not yet server-owned; no active v1 machine workflow exists.
  The archived beta CLI remains the reference for the old direct-ownership path.
- Tray client is currently disabled.
