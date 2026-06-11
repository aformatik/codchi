# Phase 1 — HTTP API + Linux/Podman Vertical Slice (locked decisions)

Freezes the inputs for Phase 1. Changes require an explicit revision of this
file (same rule as `00-contract-decisions.md`). Stable phase definition lives in
[../PLAN.md](../PLAN.md); progress lives in [../STATUS.md](../STATUS.md).

Phase goal (from PLAN): `axum` server, typed client, readiness, event stream,
server-owned Podman store startup; **CLI does `status` end-to-end**.

## Crate restructure

- **D1 — Retire pre-v1 *architecture* crates to `crates/beta/`.** The crates
  carrying the beta CLI-owned-state architecture move under `crates/beta/`: the
  `codchi-server` skeleton, `codchi` (CLI), `codchi-gui`, `codchiw`, `ipc`,
  `shared`. They are frozen, read-only **reference** — kept for orientation,
  especially the started Podman store work in the old `codchi-server`. They stay
  in the workspace `exclude` list.
- **D2 — Rename retired packages `beta-*`, don't compile.** Each moved package
  is renamed with a `beta-` prefix (`beta-codchi`, `beta-codchi-server`,
  `beta-ipc`, `beta-shared`, `beta-codchi-gui`, `beta-codchiw`). Directory names
  follow. They are **not expected to compile** (their workspace dep references
  are already dangling); read-only reference only. The `beta-` prefix removes
  the package-name collision with the new `codchi-server` and namespaces the
  prior generation.
- **D3 — New v1 crates build on `codchi-api`.** Create fresh
  `crates/codchi-server` and `crates/codchi-cli`, both depending on `codchi-api`
  from the start. The CLI package is `codchi-cli` but ships the binary named
  `codchi` (`[[bin]] name = "codchi"`) — the public compatibility surface is
  unchanged. The server package is `codchi-server`, binary `codchi-server`.
- **D4 — `shared`/`utils` are reclassified, not blindly retired.**
  - `shared` is beta infra → retired as `beta-shared`. A **fresh `codchi-shared`**
    crate is created, pulling forward only what v1 actually needs (e.g. path
    constants that contract with the store image, a command-runner helper). The
    architecture changed drastically, so the old shared grab-bag is left behind.
    `codchi-api` stays the **wire** contract; `codchi-shared` is non-wire common
    code.
  - `utils` is **not** beta architecture — it builds `ndd` ("nix don't
    deadlock"), a binary baked into the store image (`flake.nix`:
    `codchi-utils` → `store-podman-image`) that wraps `nix` inside the store
    container. It is a live build dependency of v1's real store startup. It
    **stays active**, renamed `codchi-container-utils` (binary stays `ndd`),
    still built into the image via its `default.nix`.
- **D5 — Active workspace members.** `members = ["codchi-api", "codchi-server",
  "codchi-cli", "codchi-shared", "codchi-container-utils"]`. All active crates
  keep the `codchi-` prefix (consistency chosen over brevity; `codchi-api` is
  not renamed). No half-active excluded crates at the top level.
  `codchi-gui`/`codchiw` are recreated fresh in their own phases (15 / 12).

## Transport

- **D6 — Host per-user Unix domain socket, chosen to extend to containers.**
  Phase 1 serves the HTTP/JSON API over a `UnixListener` (axum) at a per-user
  path (e.g. `$XDG_RUNTIME_DIR/codchi/server.sock`). The typed client dials the
  socket. Rationale: the API can control machines and read plaintext secrets, so
  a host-wide localhost TCP port is the wrong default; socket file permissions
  give per-user isolation, deferring a local auth token to Phase 17.
  - **Why a socket and not TCP, given containers must reach the server later:**
    the in-machine components (`codchi-machine-agent`, `codchi-session`, Phases
    8/13) are documented as reaching `codchi-server` from *inside* a Podman
    machine. A host Unix socket can be bind-mounted into machines later
    (`-v …/server.sock:/run/codchi/server.sock`), so the same socket serves both
    host CLI/tray and in-container agents — no second transport, no host-wide
    port. Most in-machine data is push (boot spec at container start, session
    id/env as `podman exec … codchi-session --session <id>` args); the live pull
    is the agent's health/policy connection.
  - **T1 (Phase 1 task) — rootless-podman bind-mount reachability probe.** Before
    the socket choice is load-bearing, verify a process inside a rootless Podman
    container can actually open a bind-mounted host socket under the userns UID
    mapping. Record the result here.

## Vertical slice scope

- **D7 — Slice proves all three catalog response shapes through the generic
  plumbing, with a real/mock split.** `status` end-to-end exercises: a
  readiness/lifecycle endpoint, `list_machines` (JSON body), and an NDJSON log
  stream — driven through the deferred generic `mount<E>` / `call<E>` helpers and
  `impl CodchiService for HttpClient` (doc 06). The goal is wiring correctness
  across JSON / NDJSON / empty shapes, not state correctness.
  - **Mock vs. real.** `AppState` holds `Arc<dyn CodchiService>`, set to
    `codchi_api::testing::MockCodchiService` in Phase 1 and swapped for the real
    `ServerCore` in Phase 2. Machine-**data** endpoints (`list_machines`, …)
    return mock data through that trait object.
  - **Readiness + Podman store startup are real, not mocked.** The Phase 1
    server genuinely starts/verifies the Podman store and drives the real
    `Starting → Healthcheck → Ready/Degraded` lifecycle; the readiness endpoint
    reports *that* (satisfying PLAN's "server-owned Podman store startup").
    Lifecycle/readiness is server-infrastructure state in `AppState`, **not** a
    `CodchiService` method (handlers stay thin per doc 01).
  - **NDJSON is proven by a *real* `stream_logs(Store)`** against actual
    store-container output (see D9), not a mock job — the store-log capture is
    real in Phase 1, so the streamed shape rides real data.
  - Note: `MockCodchiService` is `pub` (not `cfg(test)`), so the server can hold
    it at runtime. It is not yet behind a feature gate; gating it out of
    production builds is deferred (Phase 14/17).

## Daemon spawn & lifecycle

- **D8 — Client-initiated spawn, one spawn model across platforms.** The CLI
  dials the socket; on ENOENT/connection-refused it spawns `codchi-server`
  detached, polls for socket readiness with a **bounded timeout**, then retries
  the request. On timeout or `Degraded`, it surfaces a structured startup error
  — never blocks indefinitely. This is the model Windows mirrors in Phase 12
  (`codchi-hostctl.exe` spawns the server), so there is a single spawn/lifecycle
  model, not two.
  - **Lifecycle subset for Phase 1:** `Starting → Healthcheck → Ready/Degraded`.
    `Migrating` is skipped (no SQLite until Phase 3); `Stopping` is minimal.
  - **A1 (acceptance check) — hang-forever guard.** Stall/kill the server during
    startup and assert the client times out with a structured startup error
    rather than blocking, satisfying the doc's "clients must not hang forever
    when startup fails."
  - systemd socket activation is noted only as a *possible later Linux
    optimization*, not a second spawn model.

## Unified logging & jobs model

- **D9 — Logs are source-keyed; jobs are subject-tagged operations that write
  into a source's log.** Grounded in the beta (`master`) survey of what emits
  logs.
  - **Three log sources** (long-lived emitters): `Server` (daemon), `Store`
    (store container), `Machine(id)` (machine container). Each has a durable log
    stream under R8 tiering. The CLI's own per-command terminal output is
    client-side, **not** a server-stored source.
  - **A job is an operation against a source**, carrying an explicit
    `subject: Machine(id) | Store | Server`. Its events are written into that
    source's log, correlated by `job_id`.
  - **Two lenses on one log store:** `stream_logs(source)` = everything the
    source ever emitted (a job's output *and* ongoing chatter);
    `stream_job_events(job_id)` = the correlated subset for one operation. Store
    startup is a `Store`-subject job; post-startup store chatter is more
    `Store`-source log with no job id.
  - Rejected alternatives: a separate `LogSource` facility parallel to jobs
    (redundant); modeling the whole server/store process as one long-running
    "job" (a continuous process is not an operation with a terminal state — the
    beta shows store/machine emit far outside any single operation).
- **CR1 — This revises the frozen Phase 0 contract** (`phases/00-contract-decisions.md`),
  so it requires an explicit revision there, not a silent divergence:
  - `JobView.machine: Option<MachineId>` → `subject: JobSubject {Machine,Store,Server}`.
  - Add `list_jobs(filter)` to `CodchiService`.
  - Add source-scoped `stream_logs(source, EventStreamOpts)`;
    `stream_job_events(job_id)` becomes the job-correlated view over the same
    store. No separate bounded-query endpoint: `EventStreamOpts` already has
    `tail` / `since_seq` / `follow`, so `follow: false` does bounded tail/replay
    (finite NDJSON), same as `stream_job_events`.
  - Add store/server `JobKind`s (e.g. `StoreStart`, store recovery).
  - This is a breaking contract change, expected to trip the `oasdiff` gate;
    annotate it as an intentional Phase 0 revision.
  - `remoc` is dropped entirely (doc 01); `ipc` logging/health types retire with
    `beta-ipc`. Health/status notions come from `codchi-api`.

## Platform layer

- **D10 — `cfg` OS selection + an `Arc<dyn>` seam, single backend per OS.**
  With LXD dropped, v1 has exactly one backend per OS — Linux→Podman,
  Windows→WSL — compiled into separate binaries that never coexist. The only
  real variation axis is the target OS (compile-time), so dispatch is:
  - **OS selection at construction via `cfg`.** `#[cfg(unix)]` builds the Podman
    impls, `#[cfg(windows)]` builds the WSL impls. No runtime backend menu, so
    **no `Box<dyn>` for backend *selection*** (there is nothing to switch).
  - **Hold the platform seam as `Arc<dyn …Driver>` in `ServerCore`,** not viral
    generics (`ServerCore<S: Store, …>`). For a daemon dominated by
    `podman`/`nix`/`wsl.exe` subprocess I/O the vtable cost is noise, and `dyn`
    keeps the boundary ergonomic and trivially **mockable in tests** — mirroring
    the `AppState { Arc<dyn CodchiService> }` choice. Dynamic dispatch is used
    for the *test/mock seam*, not to model platforms. (`enum_dispatch` is
    unnecessary since impls are already `cfg`-split.)
  - **Keep the beta `Store` / `Machine` / `Host` trait decomposition.** It
    worked well across platforms in the beta; the rare platform-specific
    exception (e.g. WSL's boot-spec/`hostctl` flow vs. Podman) is left to its
    own future phase. Traits stay narrow and behavior-focused
    (`start`/`stop`/`status`/`logs`); platform-specific orchestration lives in
    the impls so the trait does not become a leaky union.
- **D11 — Port the beta Podman store forward, decoupled; don't link beta.**
  Bring the `Store` trait + `podman::StoreImpl` + the `server.rs::main` startup
  sequence from `beta-codchi-server` into the new `codchi-server` as **read
  reference, ported by hand**: swap `ipc`/`remoc` logging for D9 `Store`
  source-log capture + `codchi-api` events, map the old
  `ServerStatus::StoreInitializing/StoreStarting` onto the v1
  `Starting → Healthcheck → Ready/Degraded` lifecycle, and pull only the needed
  `shared::consts` / `CommandExt` into `codchi-shared`. **The `linux-lxd` impl is
  dropped** (out of v1 scope; Podman is its replacement — started but unfinished
  in beta, to be completed here). `beta-codchi-server` is never a dependency.
  - **Platform-naming seam: the `codchi-machine-` prefix lives *only* here.**
    `MachineId` is carried bare everywhere above the platform trait (Q1); the
    qualified resource name is derived at the seam by a single
    `machine_resource_name(&MachineId) -> String` (+ inverse
    `machine_id_from_resource(&str) -> Option<MachineId>`) in `codchi-shared`
    consts — the v1 successor to the beta's `consts::machine::machine_name`
    (`codchi-{name}`), now `codchi-machine-{id}`. Only the `podman` / `wsl`
    impls call it; `ServerCore`, the API, the CLI, SQLite, and the source logs
    never see the prefix. The store/server get their own fixed resource names
    (no machine prefix), so the `codchi-*` namespace is type-partitioned without
    the beta's `codchistore` collision dodge. The one deliberate exception is
    `ExecPlan.target`, which the **server** fills via `machine_resource_name` so
    the exec client never reconstructs the scheme. Because the prefix differs
    from the beta (`codchi-` → `codchi-machine-`), the beta→v1 migration phase
    owns renaming/recreating the underlying container/distro.

## Nix packaging & build wiring

- **D12 — Reactivate only what the Linux slice needs to build and run.**
  - **Repoint all nix crate references** to the new layout: `crates/default.nix`
    src regex + `cargoBuildFlags` → `-p codchi-server -p codchi-cli` (drop
    gui/codchiw); rename `codchi-utils` → `codchi-container-utils`.
  - **Reactivate the Podman store image** (`store-podman` /
    `store-podman-image`) and `codchi-container-utils` — real store startup
    (D7/D11) needs the image, which bakes in `ndd`.
  - **Expose `packages.default`** = new `codchi-server` + `codchi-cli`.
  - **Extend CI `checks`** to clippy + test `codchi-server`, `codchi-cli`,
    `codchi-shared` alongside the existing hermetic `codchi-api` + formatting +
    `oasdiff` gates.
  - **Defer (kept commented):** Windows packaging (`codchi-windows`,
    `store-wsl`, `machine-wsl`), tray (`codchi-gui`), and the tag-triggered
    `build-windows.yml` release workflow → Phases 12/13/15. The release workflow
    stays failing-by-design until then.
- **D13 — `crates/beta/` is referenced in exactly one place, negatively.**
  Nothing builds, lints, packages, or caches the beta crates.
  - **Treefmt excludes:** replace the seven per-crate lines (`crates/codchi/**`,
    `crates/codchiw/**`, `crates/codchi-server/**`, `crates/codchi-gui/**`,
    `crates/shared/**`, `crates/ipc/**`, `crates/utils/**`) with a single
    `crates/beta/**`. The new active crates come *out* of the exclude list and
    are formatted/linted like `codchi-api`.
  - **Cargo `exclude`:** keep `crates/beta/*` listed for explicit intent (guards
    against a future glob sweeping them into the workspace).
  - **Everywhere else** (product src regex, `cargoBuildFlags`, `populate-cache`,
    CI clippy/test): no beta references at all.

## Work breakdown — user stories & acceptance criteria

Chunks are mergeable units; see the dependency graph below. `[ ]` = todo,
`[x]` = done. Each story's acceptance criteria are the gate for that chunk.

### C0 — Contract revision (R11/CR1) — `[x]` done
*As a contributor, I want jobs subject-tagged and logs source-keyed in the
frozen contract, so server and clients share one model before fanning out.*
- [x] `LogSource`/`LogSourceKind`, `JobView.subject`, `JobKind::StoreStart`/`StoreRecover`, `JobFilter`.
- [x] `list_jobs` + `stream_logs` in `CodchiService` + endpoint catalog (`ROUTES` = 28).
- [x] mock + contract tests updated (10 green); `openapi.json` regenerated; `phases/00` R11 recorded.

### C1 — Typed path parameters (`codchi-api`) — `[x]` done
*As a contributor, I want each route's path params modeled as a typed value so
the router and client share one render/parse and can't drift.*
- [x] `Endpoint::Path` associated type (tuple of typed ids) added per route.
- [x] A render/parse trait: tuple → `PATH` template, and axum matched-params → tuple.
- [x] `LogSource` round-trips through the `{source}` segment (`server`/`store`/`machine-<id>`).
- [x] Unit test renders **and** parses every `ROUTES` entry's params.
- [x] Path-segment encoding via the `percent-encoding` crate (no hand-rolled
  encoder); render is infallible, `parse` takes the transport's matched values
  **in template order** (no name-keyed reorder).
- [x] Secret `{key}` segment is the validated `SecretName` newtype, not raw
  `String` — closes the last raw-`String` path param (contract revision **R12**
  in `phases/00`; wire-compatible, `oasdiff` unaffected).

### C2 — Crate restructure + nix repoint (D1–D5, D12–D13) — `[x]` done
*As a maintainer, I want the beta crates retired and the v1 crates scaffolded so
all new work builds on `codchi-api` with a clean, green workspace.*
- [x] Beta crates moved to `crates/beta/`, renamed `beta-*`, in cargo `exclude`.
- [x] `codchi-server`, `codchi-cli` (`[[bin]] codchi`), `codchi-shared`, `codchi-container-utils` scaffolded; `members` = the 5 active crates.
- [x] Nix repointed: store image builds, `packages.default` builds server+cli, `codchi-utils`→`codchi-container-utils`.
- [x] Treefmt: single `crates/beta/**` exclude; new crates linted/formatted.
- [x] `cargo build` + `nix flake check` green; all 18 `codchi-api` tests still pass.

### C3 — Server skeleton + generic `mount<E>` (D6, D7) — `[x]` done
*As a CLI/tray client, I want the daemon to serve the full typed API over a
per-user Unix socket so I can call any endpoint locally.*
- [x] axum over `UnixListener` at `$XDG_RUNTIME_DIR/codchi/server.sock` (`codchi-shared::server_socket_path`, `CODCHI_SOCKET` override).
- [x] `AppState { Arc<dyn CodchiService>` (= `MockCodchiService`) `+ LifecycleHandle }` — readiness is infra state, not a service method (D7); the handle is the C6 seam.
- [x] Generic per-shape `mount_json`/`mount_ndjson`/`mount_empty<E>` register every route through the trait object; path params validated via the C1 `PathParams::parse` off axum `RawPathParams` (no axum dep in `codchi-api`).
- [x] `router_covers_all_routes` asserts the live router mounts all 28 `ROUTES` (in-process `oneshot`, mirrors the OpenAPI-coverage test).
- [x] `GET /v1/server` returns `ServerStatus`; JSON / NDJSON / empty shapes all serialize (`response_shapes_serialize`, plus a real-binary curl smoke).

### C4 — Typed HTTP client (D6, doc 06) — `[x]` done
*As the CLI, I want a typed client implementing `CodchiService` over the socket
so command code calls semantic methods, not URLs.*
- [x] Generic `call_json`/`call_ndjson`/`call_empty<E>` over a per-request Unix-socket `hyper` http1 connection (`hyper-util` `TokioIo`; no `hyperlocal` dep).
- [x] `impl CodchiService for HttpClient` (one-liners) for every method.
- [x] Integration test: client ↔ C3 server round-trips JSON, NDJSON, empty, **and** the typed-error path; connection-refused surfaces a structured error (C5 hang-guard foundation).
- [x] Surfaced + fixed a latent contract bug: typed `JobView<O>` was undeserializable (spurious serde `O: Default` bound) — see **R13** in `phases/00`. Wire-neutral; `oasdiff` unaffected.

### C5 — Client-initiated spawn + `codchi status` + A1 (D8) — `[x]` done
*As a codchi user, I want `codchi status` to just work — starting the daemon if
needed and never hanging — so I can see daemon/store/machine state.*
- [x] CLI dials; spawns `codchi-server` detached on absent socket (`codchi-cli::daemon`, `CODCHI_SERVER_BIN` override → exe-sibling → `PATH`); bounded readiness poll (`await_ready` over `server_status`); fast-path skip when already reachable.
- [x] `codchi status` renders lifecycle + store state + (mock) machine list (clap surface, default subcommand; `--json` for the raw `ServerStatus` + machines).
- [x] **D7 wiring closed:** the `server_status` handler now overlays the real `LifecycleHandle` onto the service's `ServerStatus`, so readiness reports the daemon's true `Starting/Ready/Degraded` — the basis for a meaningful poll and the C6 seam.
- [x] **A1:** `await_ready` is bounded by an outer `tokio::time::timeout` (the load-bearing guard); stalled (`Starting`), absent-socket, and `Degraded` servers all resolve to a structured `StartupError` within the bound — acceptance test `tests/spawn.rs` (4 cases). Real auto-spawn smoke-tested end to end over a temp socket.

### C6 — Real Podman store startup + lifecycle (D10, D11) — `[x]` done
*As a codchi user, I want the daemon to own the Podman store lifecycle so my
machines have a running store without me managing containers.*
- [x] `Store` trait + `podman::PodmanStore` + startup sequence ported, decoupled from `ipc`/`remoc`/LXD, behind the `Arc<dyn>` seam.
- [x] Needed consts/`CommandExt` pulled into `codchi-shared`.
- [x] Server start drives the real `Starting → Healthcheck → Ready/Degraded` off actual store bring-up.
- [x] Store-down → `Degraded` + `store.unavailable` finding; readiness reflects real state.
- [x] No `linux-lxd` impl present in `codchi-server`.

### C7 — Source-log capture + `stream_logs` (D9, D14) — `[ ]` *(needs C3, C6)*
*As a codchi user/contributor, I want to introspect server and store logs so I
can see what the daemon and store are doing.*
- [ ] `Server` + `Store` source logs → append-only JSONL + in-memory ring.
- [ ] `stream_logs(Server|Store)` honors `tail` / `since_seq` / `follow` (incl. bounded `follow:false`).
- [ ] Store-container output captured (port `parse_container_log`) into the `Store` source log.
- [ ] `stream_logs(Store)` streams **real** store-startup output — the slice's true NDJSON proof.

### C8 — T1 rootless bind-mount probe (de-risking spike) — `[ ]` *(independent; do early)*
*As a contributor, I want to verify a rootless-podman container can reach a
bind-mounted host socket, so the Phase 8/13 in-machine transport is viable before
it's load-bearing.*
- [ ] Probe run; result (works / doesn't + constraints) recorded under **T1** above.

### Phase 1 Definition of Done
- [ ] On a fresh Linux/Podman host, `codchi status` auto-starts the daemon, brings up the **real** Podman store, and prints lifecycle + store + (mock) machine state.
- [ ] `stream_logs(Store)` shows real store-startup output; `stream_job_events` proven via mock.
- [ ] Hang-forever guard (A1) holds; T1 result recorded.
- [ ] CI clippy+tests green for `codchi-api`/`codchi-server`/`codchi-cli`/`codchi-shared`; `oasdiff` R11 break acknowledged.

### Dependency graph
```
C0 ✅
 ├─ C1 ✅─┐
 └─ C2 ✅─┴─ C3 ✅─┬─ C5 ✅ (status/spawn/A1)
        C4 ✅──────┘
        C3 ✅──── C6 (store) ──── C7 (logs)
C8 (probe) — anytime, early
```
Critical path: **C2 → C3 → C6 → C7**. C1 ∥ C2; C4 ∥ C3; C5 and C6→C7 split along
the `STATE` / `PLATFORM` seam once C3 lands. With C5 done, the `STATE` side of
the slice is complete; the remaining critical path is **C6 → C7** (real store +
source-log capture), plus the independent C8 probe.
