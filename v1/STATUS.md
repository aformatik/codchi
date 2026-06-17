# Codchi v1 Migration Status

Tracks the current state of the v1 reimplementation. This file is expected to
change often. Stable phase definitions live in [PLAN.md](PLAN.md); locked
per-phase specs live under [phases/](phases/).

**Last updated:** 2026-06-17 (Restored the `.#windows` dev shell + cross-compile for `x86_64-pc-windows-msvc`, dev-only and mock-backed: `codchi-server` uses a `MockStore` and `codchi-cli` dispatches against the in-process `MockCodchiService` on non-unix; the host transport/backend stay Phase 12. See the "Windows dev shell" note below. Earlier: Phase 4, machine state in SQLite, has landed its locked scope — durable schema + contract revision + pure read/derivation logic; no write path, which is born-at-commit in Phase 6. A follow-up grill refined four MS2/MS9/MS10/MS12 decisions: `run_status` is now the **raw container observation** `Option<RunStatus>` (`Absent|Stopped|Running`, `None`=not-yet-observed) with a server-derived `lifecycle{Creating,Reconciling,Absent,Stopped,Running}` rollup; `state_version` is **surfaced on the wire**; `MachineDetail.flake_lock_hash` is a **derived getter** over the active generation (no stored field); and `secret_values.description` is refreshed on every config eval for declared keys (frozen at obsolescence) via `refresh_secret_descriptions`, not snapshotted only when a key drops out. The first task was the **`codchi-api` contract revision** the Phase-4 decisions required (R11): renamed `MachineView.schema_version`→`state_version` and surfaced it on the wire (MS12/MS15); `UpdateStatus`→`ConfigurationStatus` (`Unbuilt|Applied|NeedsRebuild`, MS11) as the new `configuration_status` field; `run_status`→`Option<RunStatus>` + server-derived `lifecycle` rollup (MS2); `MachineDetail.flake_lock_hash`→derived getter (MS9); `ModuleSpec` lost its `name` (the canonical url is the whole identity, MS8); `SecretKey` gained a `status: SecretStatus{Declared,Obsolete}` (MS10); `clone_machine`→`duplicate_machine` returning `JobView<Duplicated>` with `JobKind::Duplicate`/route `/duplicate` (MS13, clone is now CLI-only git sugar); four error variants added (`CreateArtifactsRetained`, `SecretNotSet`, `JobNotTerminal`, `JobArtifactsNotFound`). Mock/CLI/server/OpenAPI snapshot updated; contract + workspace tests green. Migration **`0002_machines.sql`** adds `machines` (case-sensitive `id` PK + `machines_id_nocase` unique index for the ASCII-CI collision rule MS6; `state_version`; a nullable `active_generation_id` whose `REFERENCES generations(id)` constraint is deferred to Phase 6 — `foreign_keys=ON` resolves an FK parent at prepare time, so the forward reference to the not-yet-created table is impossible), `machine_modules` (`PRIMARY KEY(machine_id,position)` + `UNIQUE(machine_id,url)`, ON DELETE CASCADE), and `secret_values` (`PK(machine_id,key)`, ON DELETE CASCADE); `MAX_SCHEMA_VERSION`→2. `db::machines` provides the pure-SQL accessors (`list_machine_ids`, `get_machine_row`, `nocase_collision_exists`, `read/replace_modules`, secret-value read/set/delete + `refresh_secret_descriptions`) with constraint tests (NOCASE collision, duplicate-url rejection, ordered round-trip, FK cascade, description set/refresh). The pure domain logic lands with unit tests: `validate_modules` (MS8/MS14 — duplicate-url, nixpkgs cardinality, canonical-form/no-embedded-auth url guard; wired into the `create_machine`/`set_modules` request boundary), `configuration_status` (MS11), `lifecycle` (MS2 rollup), and the secrets MS10 derivation (`secret_list` four-state union + get/set/delete classifiers). The live read endpoints (`list_machines`/`get_machine` full-view assembly) and the secret/set_modules durable writes stay on the quarantined mock by design: they aggregate the active **generation** (Phase 6), the reconciler snapshot (Phase 5/7), and findings (Phase 10), and a machine row is born only at its first generation commit (MS1, Phase 6). 12 new server tests; clippy/test/OpenAPI-drift green. Earlier: Phase 3, the SQLite foundation, is **complete (DB1–DB8)**. A `db` module in `codchi-server` provides the `Db` facade over `tokio-rusqlite` (single-writer actor): `open` sets WAL/foreign_keys/busy_timeout/synchronous pragmas, `migrate` runs `rusqlite_migration` over `PRAGMA user_version` with an owned too-new refusal guard, and synchronous `read`/`transaction` closures make "no txn across a shellout" structurally impossible. `0001_init.sql` creates the invariant `metadata` table (seeded once: install_uuid/data_dir/created_by_version/created_at) and the overrides-only KV `config` table (ships empty). `db::bring_up` opens+migrates synchronously before serve (DB-before-store) and hardens `data_dir` to `0700` / `state.db` to `0600`; on failure it degrades without wiping. The lifecycle/startup_error projection is now a **join** over a new `SchemaState` input plus the `StoreCondition` and shutdown flag (revises SC5); `server_status.schema` is read live from the DB. 12 db/projection tests added (migrate-from-empty, idempotent reopen, too-new refusal, config overrides-only roundtrip, metadata seed/preserve, permissions, lifecycle join); workspace clippy/test/fmt green. Its one `codchi-api` change was an R11 **removal** of the never-observed `ServerLifecycle::Migrating` variant + `SchemaStatus.migrating` field. Earlier: Phase 2, the `ServerCore` boundary, is **complete (SC1–SC9)**. `AppState` collapsed to `{ core: Arc<ServerCore> }`; `ServerCore` is the single `impl CodchiService`, owning the store-condition reader, the shutdown token, and the `LogStore`, with every still-unbacked domain delegating to one quarantined internal mock (SC2). The store condition is now a `StoreCondition` sum type advanced by a pure `step` reducer and published over a single-writer `watch` by the reshaped `StoreSupervisor` (SC5–SC7); `lifecycle`/`StoreStatus`/`startup_error`/`store.unavailable` are pure projections. Graceful shutdown (SC8) lands with SIGINT/SIGTERM → `CancellationToken`, `axum` `with_graceful_shutdown`, and a `oneshot`-gated ordered store teardown bounded by a timeout. Tests: 9 pure `step`/projection units + 7 supervisor invariants (D8 no-hang, P6 observe-only, no-orphans, SC8 stop, diagnosability); the SC2 gate passes (`roundtrip.rs` unmodified, `spawn.rs` one line). `cargo clippy -p codchi-server -p codchi-cli -p codchi-shared --all-targets -- -D warnings`, the crate tests, and `cargo fmt --check` all pass. A **Phase-7 design spike** also landed out-of-band — the Podman machine-container contract is locked and empirically verified ahead of implementation: Model A (boot the NixOS system's own `/init` off the shared store, no base image/tarball), a new `podman` NixOS driver, `--cap-add SYS_ADMIN` as the key boot arg, codchi-owned persistent rootfs, and a Podman×NixOS version-matrix regression suite (all in `phases/07-podman-machine.md` + `nix/tests/podman-machine-args.sh`); not yet wired into `codchi-server`. Next: Phase 5 (job system) and Phase 6 (generation model), which together unblock the deferred live machine/secret endpoint wiring.)

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
| 4 | Machine state in SQLite | **Core done (MS1–MS15, locked scope).** `codchi-api` contract revised (R11): `schema_version`→wire `state_version`, `ConfigurationStatus`/`SecretStatus` reshaped, `run_status`→`Option<RunStatus>{Absent,Stopped,Running}` + server-derived `lifecycle` rollup (MS2), `ModuleSpec` name dropped, `flake_lock_hash`→derived getter, `clone`→`duplicate`, four error variants added; OpenAPI regenerated. `0002_machines.sql` adds `machines`/`machine_modules`/`secret_values` (`description NOT NULL`, refreshed per eval) with the MS6 NOCASE collision index + module ordering/duplicate + cascade constraints; `state_version` column (FK to generations deferred to Phase 6). `db::machines` accessors (incl. `refresh_secret_descriptions`) + pure `validate_modules`/`configuration_status`/`lifecycle`/`secret_list`+matrix logic, all unit-tested; validation wired into the create/set_modules boundary. Live full-`MachineView` reads + secret/set_modules durable writes stay seam-mocked pending the Phase-6 generation source by design (born-at-commit). |
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
- **Windows dev shell (dev-only):** the `.#windows` dev shell (`.envrc` →
  `use flake ".#windows"`) is restored so the `x86_64-pc-windows-msvc` target
  can be developed/typechecked under WSL. The shell now lets `cargo xwin`
  manage the MSVC SDK on demand and pins the *unwrapped* `clang-cl` (the bespoke
  pre-splatted/hermetic SDK + the cc-wrapped clang had rotted against the bumped
  nightly + the Phase-3 `rusqlite` C dep). The whole workspace cross-compiles
  warning-free. The actual Windows host backend stays Phase 12, so the build is
  mock-backed: `codchi-server` selects a no-op `MockStore` on non-unix (the host
  transport is a Phase-12 stub), and `codchi-cli` dispatches against the
  in-process `codchi_api::testing::MockCodchiService` instead of dialing a
  daemon — enough for `codchi.exe` to be exercised for client/UI dev. The
  hermetic Windows *package* build (`pkgs.codchi-windows`/`splatted`) still needs
  the xwin modernization and is deferred to Phase 12/13.
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
