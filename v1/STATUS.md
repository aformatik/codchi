# Codchi v1 Migration Status

Tracks the current state of the v1 reimplementation. This file is expected to
change often. Stable phase definitions live in [PLAN.md](PLAN.md); locked
per-phase specs live under [phases/](phases/).

**Last updated:** 2026-06-15 (Phase 3, the SQLite foundation, is **complete (DB1–DB8)**. A `db` module in `codchi-server` provides the `Db` facade over `tokio-rusqlite` (single-writer actor): `open` sets WAL/foreign_keys/busy_timeout/synchronous pragmas, `migrate` runs `rusqlite_migration` over `PRAGMA user_version` with an owned too-new refusal guard, and synchronous `read`/`transaction` closures make "no txn across a shellout" structurally impossible. `0001_init.sql` creates the invariant `metadata` table (seeded once: install_uuid/data_dir/created_by_version/created_at) and the overrides-only KV `config` table (ships empty). `db::bring_up` opens+migrates synchronously before serve (DB-before-store) and hardens `data_dir` to `0700` / `state.db` to `0600`; on failure it degrades without wiping. The lifecycle/startup_error projection is now a **join** over a new `SchemaState` input plus the `StoreCondition` and shutdown flag (revises SC5); `server_status.schema` is read live from the DB. 12 db/projection tests added (migrate-from-empty, idempotent reopen, too-new refusal, config overrides-only roundtrip, metadata seed/preserve, permissions, lifecycle join); workspace clippy/test/fmt green. Its one `codchi-api` change was an R11 **removal** of the never-observed `ServerLifecycle::Migrating` variant + `SchemaStatus.migrating` field. Earlier: Phase 2, the `ServerCore` boundary, is **complete (SC1–SC9)**. `AppState` collapsed to `{ core: Arc<ServerCore> }`; `ServerCore` is the single `impl CodchiService`, owning the store-condition reader, the shutdown token, and the `LogStore`, with every still-unbacked domain delegating to one quarantined internal mock (SC2). The store condition is now a `StoreCondition` sum type advanced by a pure `step` reducer and published over a single-writer `watch` by the reshaped `StoreSupervisor` (SC5–SC7); `lifecycle`/`StoreStatus`/`startup_error`/`store.unavailable` are pure projections. Graceful shutdown (SC8) lands with SIGINT/SIGTERM → `CancellationToken`, `axum` `with_graceful_shutdown`, and a `oneshot`-gated ordered store teardown bounded by a timeout. Tests: 9 pure `step`/projection units + 7 supervisor invariants (D8 no-hang, P6 observe-only, no-orphans, SC8 stop, diagnosability); the SC2 gate passes (`roundtrip.rs` unmodified, `spawn.rs` one line). `cargo clippy -p codchi-server -p codchi-cli -p codchi-shared --all-targets -- -D warnings`, the crate tests, and `cargo fmt --check` all pass. A **Phase-7 design spike** also landed out-of-band — the Podman machine-container contract is locked and empirically verified ahead of implementation: Model A (boot the NixOS system's own `/init` off the shared store, no base image/tarball), a new `podman` NixOS driver, `--cap-add SYS_ADMIN` as the key boot arg, codchi-owned persistent rootfs, and a Podman×NixOS version-matrix regression suite (all in `phases/07-podman-machine.md` + `nix/tests/podman-machine-args.sh`); not yet wired into `codchi-server`. Next: Phase 4, machine state in SQLite.)

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
| 3 | SQLite foundation | **Done — DB1–DB8.** `codchi-server::db` provides the `Db` facade over `tokio-rusqlite` (single-writer actor) with WAL/foreign_keys/busy_timeout/synchronous pragmas; `rusqlite_migration` drives `PRAGMA user_version` (forward-only, numbered `.sql`) with an owned too-new refusal guard. `0001_init.sql` creates the invariant `metadata` table + the overrides-only KV `config` table (ships empty; deployment knobs stay env vars). Synchronous `read`/`transaction` closures make "no txn across a shellout" structural. `db::bring_up` opens+migrates synchronously before serve (DB-before-store), hardens `data_dir`→`0700` / `state.db`→`0600`, and degrades without wiping on failure; lifecycle/startup_error generalized to a projection-**join** over a new `SchemaState` input (revises SC5); `server_status.schema` read live. The one `codchi-api` change was an R11 **removal** of the never-observed `ServerLifecycle::Migrating` variant + `SchemaStatus.migrating` field. |
| 4 | Machine state in SQLite | Not started |
| 5 | Job system MVP | Not started |
| 6 | Build/update generation model | Not started |
| 7 | Linux/Podman machines | **Design spike done; not implemented.** Machine-container decisions M1–M13 locked in `phases/07-podman-machine.md`; the `podman` NixOS driver added (`nix/nixos/driver/podman/`, Model-B tarball gated to lxd/wsl); the `podman run` arg contract + a Podman×NixOS version matrix verified by `nix/tests/podman-machine-args.sh`. Server-side register/start/stop/exec/persistence wiring not started. |
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
- **Podman machine spike (Phase 7, design-only):** a hands-on bring-up locked the
  Linux machine-container contract before implementation — `phases/07-podman-machine.md`
  (M1–M13). A v1 machine is **Model A**: a NixOS config built in the store and run
  via `podman run … --cap-add SYS_ADMIN --rootfs <codchi-owned dir>
  <system>/init` off the shared store volume (read-only `subpath=store`) — no base
  image, no bootstrap tarball (that stays lxd/wsl). The new `podman` NixOS driver
  lives at `nix/nixos/driver/podman/`. `SYS_ADMIN` is the one non-obvious required
  arg (fixes dbus-broker + lets activation mount its own specialfs). Persistence =
  a codchi-owned rootfs dir, **not** a named-volume mountpoint (verified unreliable:
  podman doesn't ref-count it, `volume rm`/`prune` destroys a running machine). The
  `podman run` arg contract and a Podman×NixOS support matrix are guarded by
  `nix/tests/podman-machine-args.sh` (scheduled/manual; PASS 26/0/0 on NixOS
  25.05 + 26.11, podman 5.8.2). Not yet wired into `codchi-server`.
- Machine state is not yet server-owned; no active v1 machine workflow exists.
  The archived beta CLI remains the reference for the old direct-ownership path.
- Tray client is currently disabled.
