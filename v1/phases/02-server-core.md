# Phase 2 — The ServerCore Boundary (locked decisions)

Locks the structural boundary introduced in Phase 2: the `ServerCore` type that
implements `CodchiService` directly, the collapse of `AppState` to a single
`core` handle, and the state-ownership model for server-owned condition. Changes
to this file require an explicit revision (same rule as
`00-contract-decisions.md` and `01-podman-store.md`).

Grounded in the Phase-1 code actually on `server` (`crates/codchi-server`) and a
survey of the full beta command surface on `master` (`crates/beta/beta-codchi`),
so the "does it scale" claims below are honest about where the lean model hands
off to a heavier one.

## Background: what Phase 1 actually left

- `AppState` has **four** sibling fields — `service: Arc<dyn CodchiService>`
  (the mock), `lifecycle: LifecycleHandle`, `infrastructure:
  InfrastructureHandle`, `logs: LogStore` — even though `01-architecture.md`
  prescribes `AppState { core: Arc<ServerCore> }`. The doc described a type that
  does not exist yet.
- The `ServerStatusEp` handler in `router.rs` **hand-overlays**
  `infrastructure.snapshot()` + `lifecycle.current()` onto the mock's
  `server_status()`, and `StreamLogsEp` hand-branches `LogSource::Machine`
  (mock) vs `Server`/`Store` (`LogStore`). That orchestration lives in the
  router — the "thin handler" rule from `01-architecture.md` is violated.
- `InfrastructureSnapshot` is **four denormalized fields** kept consistent by
  hand: `store: StoreStatus`, `findings: Vec<Finding>`, `startup_error`. The
  `store.unavailable` finding and `startup_error` are *derivable* from the store
  state, so `store_down()`/`store_up()` mutate all of them in lockstep and can
  drift. Mutation is via **public** `store_up`/`store_down`/`store_recovering`
  methods over `Arc<RwLock>` with `.expect("lock poisoned")`.
- `StoreManager` is a **detached** `tokio::spawn(manager.run())` task that owns a
  clone of `AppState` and pokes those public mutators. Nothing holds a handle to
  it; it cannot be signalled.

## Core decisions

- **SC1 — `AppState` collapses to `{ core: Arc<ServerCore> }`.** `ServerCore` is
  the single `impl CodchiService`. It *owns* the server-owned state the router
  used to assemble: lifecycle/store condition (SC5), the findings projection
  (SC5), and the `LogStore`. The `ServerStatusEp` overlay and the `StreamLogsEp`
  source-branch **move into `ServerCore`**, so both handlers become one-liners.
  This realizes `01-architecture.md`'s prescribed shape; the doc was ahead of
  the code, not wrong.

- **SC2 — Phase 2 is a structural passthrough; no client-observable behavior
  change.** `ServerCore` serves the already-real bits (the `server_status`
  projection of SC5, the `Server`/`Store` source logs) and **delegates every
  still-unbacked method to an internal `mock: MockCodchiService` field**. CLI
  output is byte-identical to Phase 1. The mock stops being the top-level service
  and becomes an explicit, quarantined placeholder *inside* `ServerCore`, removed
  domain-by-domain as Phases 4/5/6/7 land.

- **SC3 — Thin trait dispatch over domain-grouped inherent methods.** Rust allows
  only one `impl CodchiService for ServerCore`, so that block is a thin
  dispatcher; the bodies are inherent `impl ServerCore` methods grouped into
  domain modules (`core/machines.rs`, `core/jobs.rs`, `core/store.rs`,
  `core/generations.rs`, `core/secrets.rs`, `core/exec.rs`, `core/doctor.rs`,
  `core/migration.rs`). For Phase 2 these are 1:1 with the trait and mostly
  `self.mock.<x>()`; each is the named seam its phase fills in. A lower-level
  internal vocabulary ("init = validate + insert row + enqueue job") emerges only
  when there is real work to decompose — not now. A **single** internal `mock`
  field backs all stubbed domains.

- **SC4 — Two orthogonal layers; Phase 2 introduces no new platform drivers.**
  The *domain/orchestration* layer (`ServerCore` + its domain modules) owns state
  and decides *what* to do. The *platform/mechanism* layer (the beta's
  `Store`/`Host`/`Machine`/`ShellDriver` traits, `#[cfg]`-selected per OS) is
  *stateless OS primitives* that know nothing of jobs, generations, or SQLite.
  They compose by **containment**: `ServerCore` holds drivers behind narrow `dyn`
  traits and calls them from domain methods. Platform-driver handles get pulled
  into `ServerCore` **with the phase that uses them** (machines/`Host`/`Machine`
  in Phase 7+), not now. In Phase 2 the only live driver (`Store`) stays under
  the supervisor (SC7). This is the `PLATFORM` vs `STATE` separation `PLAN.md`
  keeps apart via the `CodchiService` trait.

- **SC5 — Observed condition is a derived sum type, not denormalized fields.**
  Replace `InfrastructureSnapshot` with a single sum type:

  ```rust
  enum StoreCondition {
      Starting,
      Checking,
      Up       { since: DateTime<Utc> },
      Degraded { reason: String, since: DateTime<Utc> },
  }
  ```

  `lifecycle`, `StoreStatus`, `startup_error`, and the store finding are **pure
  projections** of this one value — no stored duplication, so they cannot drift,
  and illegal combinations (`Up` with a `startup_error`; `Degraded` with no
  reason) are **unrepresentable**. The `findings: Vec<Finding>` field and the
  find-or-push logic are **deleted**; in Phase 2 the only finding
  (`store.unavailable`) is `project_findings(&Degraded)`. Because `Degraded`
  carries `since`, the projected finding's `created_at = since` and its id is
  derived deterministically from its code — so the projection is **pure *and*
  stable** across reads (no regenerated ids/timestamps). This is the proof that
  `since`-in-the-variant is correct.

- **SC6 — Pure reducer + single-writer `watch` publish; no lock, no mailbox.**
  The transition logic is a pure, synchronous, unit-testable free function:

  ```rust
  fn step(now: &StoreCondition, outcome: ProbeOutcome) -> StoreCondition { … }
  ```

  The supervisor (SC7) owns `condition: StoreCondition` as a **plain field**
  (single task → zero write-side synchronization), advances it with `step`, and
  publishes via `watch::Sender`. Readers hold a `watch::Receiver` and project on
  read. There are **no exposed mutators** — only the supervisor holds the
  `Sender`, so "nobody else can mutate" is enforced by the borrow checker, not by
  convention. No `RwLock`, no `Message` enum, no `mpsc`/`oneshot` for this slice.
  The look-alike-actor TOCTOU race is impossible because read-decide-write
  happens in one place over an owned value.

- **SC7 — The supervisor owns the writer half; `core/store.rs` owns the reader
  half.** `StoreManager` is renamed/reshaped into the supervisor that owns
  `condition` + `watch::Sender` + `Arc<dyn Store>` + the sentinel loop (unifying
  the lifecycle loop with the *state it writes* — the "unify with `core/store.rs`"
  instinct, but only the writer half). `core/store.rs` (domain) holds a
  `watch::Receiver` and only reads/projects, preserving the SC4 layer boundary.
  This resolves "who owns `StoreManager`": no longer a detached task poking shared
  mutators, but a typed owner of its own state, constructed by `main` and handed
  the `watch::Receiver` that `ServerCore` keeps.

- **SC8 — Graceful shutdown is built now, not deferred.** `main` listens for
  **SIGINT and SIGTERM** (`tokio::signal`; unix-only — Windows is Phase 12) and
  trips a `CancellationToken`. `Stopping` is server-level, not a `StoreCondition`,
  so it forces the lifecycle projection to take a **second input** one phase early
  — the same multi-input shape Phase 3 extends for `Migrating`:

  ```rust
  fn lifecycle(shutdown: bool, store: &StoreCondition) -> ServerLifecycle {
      if shutdown { return ServerLifecycle::Stopping; }
      /* the 4-state store projection (SC5) */
  }
  ```

  The teardown is **ordered — drain clients before tearing down the
  infrastructure they depend on**:
    1. trip the token → lifecycle projects `Stopping` (in-flight `status` sees it);
    2. **drain HTTP** — `axum::serve(...).with_graceful_shutdown(token.cancelled())`;
    3. *(Phase 5 slot)* cancel/await in-flight **jobs** at safe boundaries —
       empty in Phase 2, but the slot exists;
    4. **supervisor teardown** — its loop `select!`s on `token.cancelled()`,
       stops the sentinel, detaches the log follower;
    5. **`store.stop()`** → `podman stop codchi-store` (SIGTERMs PID 1 so
       `nix-daemon` exits gracefully; the `/nix` volume persists — no data risk);
    6. exit, **bounded by an overall shutdown timeout** so a hung `podman stop`
       cannot wedge the daemon.

  Phase 2 implements 1, 2, 4, 5, 6; step 3 is a labelled gap. Stopping the store
  here is aligned, not churn-prone: the server is long-lived ("kept running after
  first use"), so it only stops on explicit stop / logout / system shutdown —
  exactly when a clean store teardown is wanted, and the next command respawns the
  server onto the fast `Stopped → start` path (SC-E covers running-machine
  interaction).

- **SC9 — Tests pin invariants, not mechanism.** The beta's pain was hangs,
  orphaned containers, silent failures, and state races — and v1's parallel-agent
  plan needs the `README` invariants defended by tests so a refactor can't quietly
  regress them. The Phase 2 surface:
    - **Pure unit:** the `step` matrix (incl. the *absent* edges, e.g. no
      `Degraded → Checking/Starting`); the projections (impossible-state asserts
      like `Up ⇒ startup_error == None`; stability: `created_at == since`,
      deterministic finding id).
    - **Supervisor over a fake `Store`, one test per invariant:** **D8** (an
      always-unhealthy store reaches `Degraded` within the deadline — a no-hang
      assertion); **P6** (the sentinel never calls `register`/`start` after
      startup, even while `Degraded`); **Store Authority / no-orphans** (idempotent
      restart: `Running → noop`, `Stopped → start`, never re-`register`; and SC8:
      cancel ⇒ `store.stop()` called, terminates within timeout); **diagnosability**
      (bring-up failure surfaces a structured reason via `startup_error` + the
      `store.unavailable` finding, and clears on recovery).
    - **Integration (few, high-leverage):** the **SC2 gate** — `codchi-cli`'s
      `roundtrip.rs` passes **byte-identical, unmodified**, and `spawn.rs` changes
      only its one lifecycle-forcing line (see the *Implementation outcome* note;
      proof the swap changed structure only; a rehearsal for Phase 14 parity); and
      the real-podman gated test extended to **start → ready → shutdown → container
      stopped** (the closest proxy to Phase 7's no-orphan acceptance).
    - **Not:** per-method mock-delegation tests (roundtrip covers it) or
      re-testing the contract/OpenAPI (already gated in `codchi-api`).

## The concurrency taxonomy (the reusable rule)

Phase 2 establishes that the primitive is chosen by **access-shape**, not by
habit. Future phases pick from this table; the SC5/SC6 model is *one row*, and
mis-applying it (e.g. `watch` for jobs) is the failure mode to avoid.

| State shape | Primitive | Status |
|---|---|---|
| Single-writer, observed, ephemeral **condition** | sum type + pure `step` + `watch` publish + read-time projection | **this phase** (store); machine-runtime later |
| Append-only **event stream** | `broadcast` + ring + JSONL | done (Phase 1 `LogStore`) |
| Durable **source of truth** | SQLite | Phase 3/4 |
| Long-running, cancellable **work** w/ progress | task + channels (cancel flag, event emit) | Phase 5 (jobs) |
| Multi-writer **registry** (pull-read) | lock-backed `handle(&self, Msg) -> Reply` reducer, or mpsc-actor | Phase 7/10 (persistent findings) |

## Scope / non-goals (Phase 2)

- **No SQLite, no real machine/job/generation/secret/doctor/migration logic** —
  all delegate to the internal mock (SC2).
- **No new platform drivers** — `Host`/`Machine` do not arrive here (SC4).
- **No API/CLI response change** (SC2 passthrough). The one *behavior* added is
  process-lifecycle, not API surface: graceful shutdown (SC8). A client never
  observes it except as a transient `Stopping` lifecycle.
- **Findings-as-projection is a Phase-2-only simplification** (SC5), valid only
  because the single finding is store-derived. See SC-B.
- The store is **not yet a job** — the supervisor is the interim stand-in for
  what becomes a `Store`-subject job in Phase 5 (SC-C).

## How this scales (and where it stops)

The lean SC5/SC6 model is the **singleton, simplest instance** of patterns that
generalize across v1 — but only as part of the taxonomy above:

- **`server_status` is the singleton of a general projection-join**:
  `durable (SQLite) ⋈ observed (watch) ⋈ in-flight (job)`. `MachineView` for N
  machines is the same join at scale — it already carries all three tributaries
  (`active_generation`/`schema_version`; `run_status`/`last_reconciled_at`/
  `snapshot_stale`; `busy_with`). The projection discipline is the backbone of
  the whole machine surface, not a store-only trick.
- **The pure `step` reducer is the reusable core of every state machine** —
  store condition, per-machine runtime status, the job lifecycle. Side effects
  live in the task that *calls* `step`.
- **Do not over-generalize "no DB".** The store is condition-only because it has
  no durable per-instance definition (image-defined, `01-podman-store.md` S1).
  Machines have durable definitions, so they add the SQLite tier *under* the same
  projection. (SC-A)
- **Jobs do not use `watch`** — they are the task+channel row, by design. Store
  start/recover become `Store`-subject jobs in Phase 5; the supervisor and the
  condition `watch` coexist with them (job = the operation + its events;
  condition = the resulting observed state — the Job vs Log-source duality in
  `CONTEXT.md`). (SC-C)

## Open items / next branches

- **SC-A — N-machine reconciler shape.** The single-writer `watch<StoreCondition>`
  generalizes to either one reconciler publishing `watch<Map<MachineId, RuntimeStatus>>`
  or per-machine ownership. An explicit **Phase 7** decision; the store pattern
  informs but does not pre-decide it.
- **SC-B — Persistent multi-source findings registry.** Full v1 findings are
  durable and multi-writer (doctor scans, failed migrations, machine corruption):
  `derived-projections ∪ persisted-facts (SQLite)`. The multi-writer-registry row
  of the taxonomy. **Phase 7/10.**
- **SC-C — Store start/recover as `Store`-subject jobs.** Deferred to **Phase 5**;
  the SC7 supervisor is the interim driver.
- **SC-D — Heavy domain modules extracting into sub-service structs.** The
  rejected (for Phase 2) "sub-service types with their own state" option may earn
  its place once a module (likely jobs or machines) has real state. Revisit
  per-phase; SC3's module grouping does not preclude it.
- **SC-E — Shutdown vs running machines.** SC8 unconditionally `store.stop()`s on
  shutdown, which is safe in Phase 2 (no machines). Once machines can be running
  (Phase 7), stopping the store out from under them must be deliberate — drain/stop
  machines first, or refuse. **Target: Phase 7.** (Phase 5 separately fills SC8
  step 3: draining in-flight jobs at safe boundaries before store teardown.)

## Impact on the current `server` code

Concretely, Phase 2:

- **Deletes** `InfrastructureHandle`/`InfrastructureSnapshot` and the
  `store_up`/`store_down`/`store_recovering`/`store_unavailable` mutators.
- **Adds** `StoreCondition` + `step` + the projection functions
  (`lifecycle`/`store_status`/`startup_error`/`store_findings`).
- **Reshapes** `StoreManager` into the SC7 supervisor (owns `condition` +
  `watch::Sender` + `Arc<dyn Store>` + loop); drops the detached-task-pokes-shared
  -handle model.
- **Introduces** `ServerCore` (owns the `watch::Receiver`, the `LogStore`, the
  lifecycle projection, and the internal `mock`) and the `core/` domain modules
  (SC3).
- **Collapses** `AppState` to `{ core }` and shrinks the `ServerStatusEp` and
  `StreamLogsEp` handlers to one line each (SC1).
- **Preserves** `main`'s construction order: `LogStore` → `logging::init` →
  `watch::channel(Starting)` → `ServerCore` (holding the `watch::Receiver`) →
  spawn supervisor (holding the `Sender`) → serve.
- **Adds** signal handling (SC8): a `CancellationToken` tripped by SIGINT/SIGTERM,
  `axum` `with_graceful_shutdown`, the supervisor's `select!` on the token, the
  ordered teardown ending in `store.stop()`, and an overall shutdown timeout.

## Implementation outcome (Phase 2 landed)

The decisions above are implemented as specified, with these concrete
realizations recorded so code and spec stay in sync (per the agent rules):

- **SC9 / SC2 gate — `spawn.rs` adapts one line, not zero.** SC1+SC8 delete the
  `state.lifecycle.set()` mutator `spawn.rs` used to force a mock daemon's
  reported lifecycle. There is no lifecycle *setter* anymore (SC6), so the test
  now seeds the underlying `StoreCondition` instead, via a new test seam
  `AppState::with_mock_lifecycle(ServerLifecycle)` (the inverse of the SC5
  projection for the `Ready`/`Degraded`/`Starting`/`Healthcheck` it covers;
  `Migrating`/`Stopping` have no Phase-2 condition and panic). `roundtrip.rs`
  remains **byte-identical, unmodified**. This is the chosen resolution of the
  locked-spec tension between SC9's "unmodified" and SC1/SC8 — keep SC1/SC8
  clean, adapt the test.
- **SC6 — `step` takes an explicit `now: DateTime<Utc>`.** The spec signature
  `step(now: &StoreCondition, outcome)` elides the timestamp; to keep the reducer
  pure (no `Utc::now()` inside), the landed signature is
  `step(current: &StoreCondition, outcome: ProbeOutcome, now: DateTime<Utc>)`.
  `Starting`/`Checking` are published directly by the supervisor's bring-up (not
  by `step`), so `step`'s codomain is `{Up, Degraded}` — the absent edges hold.
- **SC5 — `Starting`/`Checking` project to `StoreState::Recovering`** (with
  `last_checked_at: None`); `Up`/`Degraded` project to `Up`/`Down`. The initial
  `watch` value is `Starting`, so there is no `Unknown` window.
- **SC8 — store teardown is gated behind a `oneshot` "drained" signal.** The
  supervisor's `select!` on the token stops the *sentinel* and detaches the
  follower immediately, but `store.stop()` waits on a gate `main` releases only
  *after* `axum`'s graceful shutdown returns — so step 5 genuinely follows step 2
  rather than racing it. `tokio-util`'s `CancellationToken` is added as a
  dependency for the shutdown token.
- **SC3 — the domain modules** landed as `core/{condition,store,server,machines,
  secrets,generations,exec,jobs,logs,doctor,migration}.rs`, each an inherent
  `impl ServerCore` block the thin `impl CodchiService` dispatches into.
- **Real-podman E2E:** the gated **start → ready → shutdown → container
  stopped** test landed as `tests/podman_supervisor.rs`, `#[ignore]`d so the
  hermetic `cargo test` gate is unaffected (run with `-- --ignored`). It is the
  in-tree proxy for Phase 7's no-orphan acceptance.
