# Context Glossary

Canonical terms for the Codchi codebase. Glossary only — no implementation
details. See `v1/` for target-architecture design docs.

## Terms

### Beta crates
The pre-v1, CLI-owned implementation, retired under `crates/beta/` with a
`beta-` package prefix (`beta-codchi`, `beta-codchi-server`, `beta-ipc`, …).
Frozen, read-only **reference** — excluded from the cargo workspace and not
expected to compile. Kept for orientation, especially the started Podman work.
Not to be confused with the active v1 crates of similar name.

### codchi-api
The active v1 contract crate: DTOs, error catalog, `Event` model, the
`CodchiService` semantic trait, and the typed `Endpoint` route catalog that is
the single source of truth for routing, the typed client, and OpenAPI. Depends
on neither `axum` nor `reqwest`.

### codchi-server
The active v1 daemon (new, Phase 1+). Owns state and platform reconciliation;
implements `CodchiService` directly via `ServerCore`. Distinct from the retired
`beta-codchi-server` skeleton under `crates/beta/`.

### codchi-cli
The active v1 CLI crate (new, Phase 1+). Package name `codchi-cli`; ships the
binary named `codchi` (the public compatibility surface). A `CodchiService`
client over HTTP. Distinct from the retired `beta-codchi` crate.

### ServerCore
The single orchestration type inside [[codchi-server]] that implements
`CodchiService` directly (the HTTP client implements the same trait over the
wire). It owns server-owned state and *holds* the stateless platform drivers,
calling them from domain-grouped internal methods — the `STATE`/`PLATFORM`
boundary. Distinct from the [[store-supervisor]], which it reads from but does
not contain. The router becomes a thin adapter over it. Spec:
`v1/phases/02-server-core.md`.

### Store supervisor
The single task that owns the [[store-container]]'s observed runtime
[[store-condition]] and the periodic health sentinel. It is the **sole writer**
of that condition (holds the `watch` sender; [[ServerCore]] holds a receiver),
so store status has no exposed mutators. Reshaped from the Phase-1
`StoreManager`. A *supervisor* (mechanism/lifecycle), not a domain method.

### Store condition
The observed, ephemeral runtime state of the [[store-container]] as a single sum
type (`Starting`/`Checking`/`Up`/`Degraded`), owned by the [[store-supervisor]].
`StoreStatus` and the `store.unavailable` finding are **pure projections** of it
— never stored separately — so illegal combinations are unrepresentable. The
server `lifecycle` and `startup_error` are **not** projections of the store
condition alone; they are a projection-**join** over every server subsystem
condition (DB/schema bring-up, the store condition, the shutdown flag, later the
reconciler), of which the store condition is one input. The server has more
moving parts than the store, so its headline lifecycle cannot hinge on the store
alone. Distinct from durable state (which lives in SQLite) and from a [[job]]
(which is an operation, not a state).

### Log source
A long-lived entity that emits a durable log stream: `Server` (the daemon),
`Store` (the store container), or `Machine(id)` (a machine container). Each
source has its own stream under R8 tiering. A source keeps logging across many
operations — store gc, machine init scripts, in-container service output — not
just during a single [[job]]. Client-introspectable via `stream_logs(source)` /
`query_logs(source)`. The CLI's own per-command terminal output is **not** a
source (it is client-side, not server-stored).

### Job
An operation against a [[log-source]] that touches external reality and has a
terminal state (`Succeeded`/`Failed`/`Cancelled`). A job's events are written
into its source's log, correlated by `job_id`. Therefore
`stream_job_events(job_id)` is the correlated *subset* of `stream_logs(source)`
for one operation — two lenses on one underlying log. Store startup is a
`Store`-subject job; the store's post-startup chatter is more `Store`-source log
with no job id.

### Job subject
The [[log-source]] a [[job]] acts on: `Machine(id) | Store | Server`. Replaces
the older machine-only coupling (the beta `JobView.machine: Option<MachineId>`).
Makes "what is the store/server currently doing" answerable via
`list_jobs(filter)`.

### Job artifact
External diagnostic material produced by a [[job]], owned and cleaned
independently from the job's [[machine]]. A retained diagnostic flake workspace
is a job artifact; SQLite job or machine rows are not.

### Machine
A named, persistent NixOS development environment defined by desired
configuration and retaining user data across rebuilds. A durable machine row
always has an active generation (it is born only at its first successful
generation); a machine still being created exists only as an in-flight [[job]],
not a durable row. Its [[platform realization]] is separate and may be absent
(stopped/never-started) while the machine still exists.

### Platform realization
The runtime artifact through which a [[machine]] executes, such as a Podman
container/rootfs or WSL distro. Its current presence and runtime state are owned
by the platform, not by durable Codchi state.

### topic
A sub-classifier *within* a source's log stream (e.g. `build`, `gc`), carried on
`Event::Log`. Not a source — a source can emit many topics.

### Store container
The single per-user Podman container (`codchi-store`) that runs `nix-daemon` and
holds the shared Nix store all [[machine|machines]] build against. Only
`codchi-server` ever starts it (Store Authority). Distinct from a *machine
container*, which runs a user's environment. See `v1/phases/01-podman-store.md`.

### Store image
The Nix-built, **fully self-contained** image that *defines* the [[store-container]]:
bootstrap + all runtime tools, all static, baked at codchi-build time. In v1 the
store has no separate provisioning step — the image is the whole definition, and a
store **update** is a recreation of the container from a new image. Its path is
baked into `codchi-server` (`CODCHI_PODMAN_STORE_IMAGE`), so dev == release.

### Bootstrap
The minimal static layer of the [[store-image]] sufficient to run `nix` and
`nix-daemon` (`nix-everything-static`, `busybox`, `ndd`, `/sbin/init`, `/etc`).
Historically (beta) the bootstrap was shipped and the **runtime** was fetched
separately; in v1 they are merged into one [[store-image]].

### Store runtime
The toolchain the store needs to build machines and serve `nix-daemon`: full
`nix`, `git`, `openssh`, `coreutils`. In the beta this was a `nix profile`
installed at container start from a github flake; in v1 it is **static and baked
into the [[store-image]]**, never fetched. Not to be confused with a Nix
"runtime closure".

### Provisioning
(Retired concept.) The beta's in-container self-install of the [[store-runtime]]
(`git init` + `nix profile install`/`upgrade` from a host-written `flake.nix`).
Deleted in v1 (`v1/phases/01-podman-store.md` S2): the store is image-defined, so
there is nothing to provision at runtime.

### Schema migration
The forward evolution of the [[codchi-server]] SQLite schema: an ordered set of
numbered DDL steps applied at daemon startup, tracked by `PRAGMA user_version`.
A purely internal database-shape concern (Phase 3). Distinct from [[beta
migration]] — these never touch user data, only table definitions.
_Avoid:_ the bare word "migration" for this; always qualify as *schema
migration*.

### Beta migration
The detection and import of a user's pre-v1 [[beta crates|beta]] installation
(host `config.toml`, per-machine `config.json`, `flake.lock`, container
metadata) into the v1 SQLite state, surfaced on the API as
`migration_plan`/`migration_run` (Phase 11). A data-import operation over real
user state; it preserves and backs up beta files, never deletes them. Distinct
from a [[schema migration]].
_Avoid:_ the bare word "migration" for this; always qualify as *beta
migration*.

### State migration
The forward evolution of a [[machine]]'s or [[store-image|store]]'s **realized
state/content** across codchi upgrades — mount scheme, init format, on-disk store
layout, and similar facts that are not SQLite rows. **Per-machine / per-store**:
each carries a stored state-format version (`state_version`, Phase 4 MS15),
unlike the **global** [[schema migration]] of the database shape. New machines
start at the current baseline (≥1); **beta-migrated machines are version 0** —
they predate the v1 state format, so [[beta migration]] sets that baseline — and
post-v1 state migrations bump the version. So the field has three writers (create,
beta migration, state migrations) and lets a migration tell a beta (0) from a v1
(≥1) machine. Distinct from the removed per-machine `schema_version` wire field.
_Avoid:_ the bare word "migration" for this; always qualify as *state
migration*.
