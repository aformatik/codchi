# Phase 4 — Machine State in SQLite (locked decisions)

Locks the durable machine model that Phases 5–7 consume: machine identity,
desired configuration, secrets, flake-lock state, and the boundary between
SQLite state and platform artifacts. Changes to resolved decisions in this file
require an explicit revision.

Grounded in the frozen Phase-0 API contract, the Phase-3 SQLite foundation, the
beta implementation on `master`, and the target platform model in
`07-podman-machine.md`.

## Decisions

### MS1 — A machine is the intended persistent development environment

A machine is not its container or WSL distro. It is the durable, named
development environment that owns desired NixOS configuration and persistent
user data across rebuilds.

In the normal client flow the selected configuration was discovered through
`resolve_config`, but that is convenience, not a gate (MS14): the service is the
authority and revalidates all pure request invariants on the submitted list —
machine id, case-insensitive collision, canonical module URLs, duplicate
entries, ordering, and nixpkgs-source cardinality — treating the input as
untrusted.

**A machine row is born only at its first successful generation** — the MS9
commit transaction — and never before. There is no durable generation-less
("creating" or "failed-create") row: every machine row in SQLite has an active
generation. This is the reliability invariant of the whole phase: because the
only durable mutation is an atomic generation commit (MS9), a crash during
create or update can never land durable state in a partial form. "Creating" is
an in-flight **job**, not a durable machine; a create that fails or is
interrupted simply leaves no machine — retry is a fresh `create`.

Consequences (see also R9):

- A *creating* machine is visible only as a **synthesized** read-model view
  unioned from the in-flight create job (active_generation `None` ⇒ this
  synthesized in-flight create, R10), not a stored row.
- `keep_on_fail` no longer retains a phantom machine row; it controls whether the
  failed **create job** retains its artifacts (workspace + partial platform
  resources) for inspection. The "failed-create machine" concept is removed.
- The machine-id namespace is **{committed machine rows} ∪ {retained
  failed-create jobs}**: a `create` is rejected if the id is a committed machine,
  a running create job, or a terminal-failed create job that still has retained
  artifacts (`CreateArtifactsRetained`, R9 / Phase 5). This is what stops a new
  `create foo` from overwriting a retained failed-create's `codchi-machine-foo`
  platform resources. Workspaces are keyed by job-id and never collide.
- Generation-less or unbacked residue (build succeeded but the commit crashed,
  or an interrupted create) is cleaned by two generic Phase-5 sweeps, not by a
  machine-state rule: job recovery marks dead `running` jobs interrupted, and
  platform GC removes any `codchi-machine-*` resource that is neither a committed
  machine nor owned by a retained failed-create job.

Platform resources are derived realizations of the machine identity. Their
absence does not by itself mean that the machine definition is absent.

### MS2 — SQLite stores no observed platform state

The Phase-4 schema stores **no** machine runtime status, reconciliation
timestamps, or last-known platform snapshot. The platform driver is the sole
authority for whether a machine realization is absent, stopped, or running;
copying that observation into durable machine state would create a second source
of truth. Runtime presence/liveness is the in-memory reconciler snapshot, which
resets on daemon restart.

The `run_status` semantics that snapshot exposes (`Reconciling | Absent |
Stopped | Running`, with integrity failures represented as findings rather than a
runtime `Failed` state) are part of the wire contract and are specified in the
reconcile section of [00-contract-decisions.md](00-contract-decisions.md); the
reconciler that produces them is implemented in the job/platform phases (Phase
5/7), not here. This revises Phase-0 P6/R10 rather than using `Stopped` as a
placeholder or persisting stale observations.

### MS3 — Diagnostic workspaces belong to jobs *(relocated)*

The diagnostic flake workspace is a job artifact, not machine state, so its
ownership, retention, debug, and cleanup are specified outside this phase: the
workspace artifact and its lifecycle in
[06-generation-model.md](06-generation-model.md) (GM-W1), and the job-side API in
[05-job-system.md](05-job-system.md) (JS-D1/JS-D2). With the simplified model
(MS1) there is no failed-create *machine*: a retained failed create is wholly
job-scoped, so its workspace and partial platform resources are cleaned together
by `delete_job_artifacts` on that create job (Phase 5 JS-D3).

### MS4 — Retained workspaces live in store backing storage *(relocated)*

The workspace path `/nix/var/codchi/jobs/<job-id>/flake/` and its persistence on
the store's `/nix` backing storage are a build-model concern, relocated to
[06-generation-model.md](06-generation-model.md) (GM-W2).

### MS5 — Job diagnostics are explicit API capabilities *(relocated)*

`JobView.has_diagnostic_workspace`, the `prepare_job_debug` /
`delete_job_artifacts` endpoints, `JobDebugPlan`, and the typed failure matrix
key off the job table and operate on jobs, not machines. Relocated to
[05-job-system.md](05-job-system.md) (JS-D1/JS-D2), with the durable-flag (P6)
and exec-plan-reuse caveats recorded there. These two endpoints take the typed
route catalog from 28 to 30.

### MS6 — Machine spelling is preserved; collisions are case-insensitive

SQLite stores the original `MachineId` spelling as the case-sensitive primary
key. Reads, path lookup, foreign keys, events, and platform-name derivation all
use that exact spelling: a request for `foo` does not address a stored machine
named `Foo`.

Creation and migration additionally enforce ASCII case-insensitive uniqueness,
as required by Phase-0 P4 and beta compatibility. The schema uses a unique
index over `id COLLATE NOCASE`, while retaining the ordinary case-sensitive
primary key. Therefore `Foo` prevents creation of `foo` without normalizing or
silently redirecting either name.

### MS7 — Module order is durable desired configuration

The order of `CreateMachineRequest.modules`, `SetModulesRequest.modules`, and
`MachineDetail.modules` is semantically significant and round-trips exactly.
SQLite stores an explicit per-machine position and renders the generated NixOS
module list in that order.

Changing only the order changes desired configuration and therefore derives
`ConfigurationStatus::NeedsRebuild`. Beta's `HashMap` iteration order is not
imported as meaningful configuration; Phase 11 must choose and document one
deterministic order when converting beta machines.

`set_modules` is **whole-list replace** (PUT, not PATCH): the request carries the
entire desired ordered list, never a partial diff. This is the only model
consistent with MS8 (no per-module addressing handle to target a partial change)
and MS11 (status compares the complete canonical list). Writing the identical
list is a no-op; any difference derives `NeedsRebuild`. Consequently beta's
granular `module add` / `module set` / `module delete` become **client-side CLI
sugar** — `get_machine` → splice the resolved entry → `set_modules(full_list)` —
not API operations; no granular module endpoints exist. Concurrency is
last-writer-wins on the whole set; on a single-user per-user socket where config
mutation is rare and conflicts with a running rebuild are rejected (not queued),
this is an accepted v1 deferral, not an oversight — optimistic concurrency
(etag/version guard) is revisited only if needed.

### MS8 — Modules have no user-visible names

`ModuleSpec` contains only the complete Codchi flake URL, including its module
attribute, and whether that source supplies nixpkgs:

```rust
pub struct ModuleSpec {
    pub url: String,
    pub is_nixpkgs_source: bool,
}
```

Beta's module names were mutable map keys used to address individual CLI
mutations and as generated flake-input aliases. The v1 API replaces the whole
ordered module list, so those names are not durable domain identity. Phase 11
discards each beta handle after converting its value and `nixpkgs_from`
reference into the ordered `ModuleSpec` list.

Generated `flake.nix` assigns private input aliases derived from the canonical
source URL without its `#attr` fragment. The alias must be a **deterministic,
injective function of that canonical source URL**: the same canonical source
yields the same alias (so its modules share one input and one lock node — what
this decision wants), and two *distinct* canonical sources yield *distinct*
aliases (so two different flakes are never silently merged into one input/lock
node). Injectivity therefore rests on canonicalization being well-defined — the
`.git` suffix, trailing slash, default-branch, and auth-stripping questions are
the `resolve_config` canonicalization authority (MS14), not the alias scheme.

These aliases are generated artifacts: neither stored in SQLite nor exposed
through the API, so they need not be pretty — reordering modules or changing only
the selected module attribute does not rename the input. The **concrete
derivation** (e.g. a sanitized repo-name prefix plus a short hash of the
canonical URL as the disambiguator) is a generated-`flake.nix` detail and is
locked in **Phase 6**; this decision asserts only the injectivity invariant.

The canonical form of `ModuleSpec.url` itself — standard Nix flake-ref (not
beta's bespoke format), auth excluded, `#attr` split off — and its
parser/normalizer choice are tracked in
[../todo/flake-url-canonical-form.md](../todo/flake-url-canonical-form.md) and
locked with the Phase-6 normalizer.

At most one module may set `is_nixpkgs_source = true`; zero selects Codchi's
default nixpkgs. Violations are `ApiError::Validation`.

Exact duplicate canonical module URLs are also invalid. Distinct module
attributes from the same canonical source remain valid and share the generated
flake input described above. Duplicate rejection happens at the service
boundary with `ApiError::Validation`; SQLite does not silently deduplicate or
reorder the request.

### MS9 — Flake lock state commits only with a successful generation

A machine has no durable `flake.lock` until its first successful generation:
there is no stored lock for an in-flight create (which has no row at all, MS1)
or a retained failed create (which is a job, not a machine).

For create, rebuild, and update, candidate lock content remains in the job
workspace while external work runs. On success, one SQLite transaction stores
the lock content and hash, inserts the generation, and — for create — births the
machine row and sets, or for rebuild/update advances, the active-generation
pointer. Failure before that transaction leaves durable state unchanged. A
retained failed-create job's workspace may therefore contain a candidate
`flake.lock` that is useful for debugging but is not machine state.

The API represents this honestly:

```rust
pub flake_lock_hash: Option<String>
```

It is `None` exactly when the machine has never committed a successful
generation.

### MS10 — Secret declarations commit with generations; obsolete values survive

The secret schema describes the active generation, while plaintext values are
durable user-supplied state. A successful create/rebuild transaction replaces
the declared schema together with the generation commit:

- values for keys still declared are preserved;
- newly declared keys start unset;
- keys no longer declared become obsolete rather than being deleted;
- build failure leaves both declarations and values unchanged.

Obsolete values are not projected into the machine environment and do not
satisfy start-time required-secret checks. SQLite therefore stores declaration
membership separately from secret values instead of cascading value deletion
when a declaration disappears.

Obsolete entries remain visible through `list_secrets` with an explicit typed
status, so clients can warn that the value is dangling:

```rust
pub enum SecretStatus {
    Declared,
    Obsolete,
}

pub struct SecretKey {
    pub name: SecretName,
    pub description: String,
    pub has_value: bool,
    pub status: SecretStatus,
}
```

**Data model.** Declaration membership and descriptions come from the active
generation's schema; plaintext values are a separate, durable user-state table
keyed by `(machine, key)`. `list_secrets` is the **union** of {active-generation
declared keys} ∪ {keys with a stored value}, and the pair `(declared?,
has_value?)` derives the entry:

| declared? | has_value? | status | description source |
|---|---|---|---|
| yes | yes | `Declared` | active generation schema |
| yes | no | `Declared` (unset) | active generation schema |
| no | yes | `Obsolete` | last-declared description (below) |
| no | no | — (does not exist) | — |

**Obsolete description.** When a key drops out of the schema, its
last-declared `description` is **snapshotted onto the value row** so an obsolete
entry still shows what the dangling value was, rather than a generic placeholder.

`get_secret(machine, key)`:

- *obsolete* → allowed; returns the value so the user can inspect it before
  deleting;
- *declared but `has_value == false`* → there is no plaintext to return →
  typed `SecretNotSet { machine, key }` (so the CLI can say "declared, set it"
  rather than "no such secret");
- *unknown* (neither declared nor holding a value) → `ApiError::Validation`,
  consistent with `set_secret`'s unknown-key rejection.

`set_secret` rejects obsolete and unknown keys with `ApiError::Validation`;
values cannot be created or changed outside the committed declaration schema. If
a later successful generation declares the same key again, its retained value is
reattached automatically and the entry becomes `Declared`.

`delete_secret` accepts both declared and obsolete keys:

- deleting a **declared** key's value removes the value only; the declaration
  (from the active generation's schema) remains, so the entry becomes `Declared`
  + `has_value == false` (unset), re-settable via `set_secret`;
- deleting an **obsolete** value removes the dormant value row outright (the
  value was the entire reason the entry existed) and clears the
  `secret.obsolete_value` finding.

Deleting the machine cascades over all secret values, including obsolete ones.

Every obsolete value produces a persistent, deduplicated
`secret.obsolete_value` doctor finding scoped to the machine and key. The
finding clears when the value is deleted or the key becomes declared again. It
is not auto-fixable because deleting a user-supplied secret requires explicit
user intent.

### MS11 — Configuration status is a pure desired-versus-active projection

`ConfigurationStatus` replaces `UpdateStatus`:

```rust
pub enum ConfigurationStatus {
    Unbuilt,
    Applied,
    NeedsRebuild,
}
```

It is never stored. Every machine read derives it in this exact order:

```text
active_generation is None
    => Unbuilt

active generation's configuration snapshot == current desired configuration
    => Applied

otherwise
    => NeedsRebuild
```

The authoritative desired configuration is currently the ordered canonical
`Vec<ModuleSpec>` from MS7/MS8. Its comparison includes each canonical URL,
list position, and `is_nixpkgs_source` value. Every generation stores that full
immutable configuration snapshot; an optional hash may accelerate comparison
but is not a second source of truth.

Consequences:

- `Unbuilt` is derivable only on the **synthesized** in-flight create view
  (active_generation `None`, MS1); no durable machine row is ever `Unbuilt`.
- Successful create/duplicate/rebuild/update commit a generation snapshot matching
  the desired configuration and therefore derive `Applied`.
- `set_modules` derives `NeedsRebuild` only when the canonical ordered list
  actually changes; writing the same list is a no-op.
- Reordering modules, changing a URL or attribute, or changing the nixpkgs
  source derives `NeedsRebuild`.
- A failed create leaves no machine. Failed rebuild/update leaves the previous
  generation and desired configuration untouched, so the prior derived result
  remains.
- Activating another generation recalculates against that generation's
  snapshot: it is `Applied` when equal and `NeedsRebuild` when different.
- Secret values/status, runtime status, findings, busy jobs, stored/candidate
  locks, and unseen remote input revisions do not participate.

Any future durable field that changes generated NixOS configuration must be
added to both desired configuration and the generation snapshot. Otherwise it
cannot affect `ConfigurationStatus`.

**Update-availability is deliberately excluded** and beta's `UpdatesAvailable`
is dropped from this axis. "A newer revision of a remote input exists" is not a
desired-vs-active fact — it requires *fetching remote flake inputs*, which is
external reality and would violate the P6 no-probe-on-read rule if folded into a
status read. It is therefore **out of scope for Phase 4**. The intended later
shape is a **separate, in-memory field** (like the MS2 reconciler snapshot — not
durable SQLite state) refreshed by a **scheduled background job** (e.g. once per
day) that fetches inputs and compares; never a passively-maintained durable
field and never part of `ConfigurationStatus`.

### MS12 — Durable machine-row fields; platform metadata is derived

The durable machine row is deliberately thin: the `MachineId` (MS6), the ordered
desired modules (MS7/MS8), the secret values (MS10), and the nullable committed
`flake.lock` content/hash (MS9), plus the active-generation pointer. Almost
everything else is derived rather than stored:

- **`created_at` is derived, not stored.** A machine is born at its first
  generation commit (MS1), so its creation time *is* that generation's
  activation timestamp; storing a second copy would only risk drift.
- **Per-machine state version, not a schema version.** *Schema* migration is
  **global** — one DB schema version (Phase 3, PRAGMA `user_version`); there is
  no `MachineView.schema_version`. Separately, each machine carries a stored
  state-format version (`state_version`, MS15) for **state migration** —
  migrating realized state/content (mounts, init format, store layout) across
  codchi upgrades. New machines start at the current baseline (≥1);
  **beta-migrated machines are 0** (set by beta migration; they predate the v1
  state format); state migrations bump it. It is distinct from the removed
  `schema_version` and from the global DB schema version.
- **Deterministic list order.** `list_machines` returns machines **ascending by
  `MachineId`** (the case-sensitive stored spelling, MS6), with synthesized
  in-flight-create views (MS1) interleaved by their target id. Matches beta's
  name sort.

Platform metadata stays derived, with existing authorities:

- backend: the daemon host platform (`Podman` on Linux, `WSL` on Windows);
- platform resource name: `codchi-machine-<MachineId>`;
- Podman writable root: `<data-dir>/machines/<MachineId>/rootfs`;
- WSL machine location: the WSL driver's deterministic machine path;
- runtime presence/liveness: the in-memory reconciler snapshot from MS2;
- active system store path: the active generation;
- resolved NixOS release and similar build facts: immutable generation
  metadata produced by evaluation.

Persisting copies would create aliases that can drift from the naming, path,
generation, or platform authorities. Platform drivers receive durable machine
identity and generation state, derive their expected resources, then reconcile
against external reality.

### MS13 — Duplication (not clone); state copy is trivial, filesystem copy is Phase 7

Two beta commands were conflated under the v1 name `clone` and are now
un-conflated (Phase-0 Q1):

- **`clone`** is the beta git convenience — `create_machine` + `git clone` into
  the new machine (+ optional local-URL rewrite via `set_modules`). It copies
  **no** machine state, is CLI-orchestrated over existing operations, and is not
  a server job. Nothing in this phase concerns it.
- **`duplicate`** (renamed from `clone_machine`, beta `Duplicate`/`dup`) is the
  real machine-duplication job and the only one with a state-copy story.

Duplication is **not a rename** (Phase-0 Q1): it yields a second independent
machine with a fresh generation history, not the same machine under a new name.

The **state** copy is trivial and the only Phase-4-relevant part: into a new
`MachineId`, copy the source's desired ordered modules, its committed
`flake.lock`, and its declared+obsolete secret values; then proceed exactly like
`create` — born-at-commit (MS1), reproducing the copied lock without updating
inputs, recording its own first generation. It copies no source generation
history, jobs, findings, or runtime observations, and the source is never
touched.

The hard parts are **deferred to Phase 7** (platform), same scope rule as MS3–5:

- the **persistent user filesystem copy** (`lxc copy` / podman-volume copy / WSL
  export-import — beta's `duplicate_container`);
- **source-consistency**: v1 requires the source **stopped** and fails fast if it
  is running (no snapshot/quiesce machinery); snapshot-while-running is a later
  enhancement.

`duplicate` is itself **not on the create/update/server-lifecycle reliability
path** and may be implemented whenever Phase 7 lands; Phase 4 only fixes its
durable state-copy shape so the schema accommodates it.

### MS14 — Configuration mutations are validated server-side; there is no resolution token

`resolve_config` stays a pure **discovery** job (the frozen R3 contract): given a
flake url, it fetches and evaluates the flake and returns the available module
attrs, whether a `nixpkgs` input is present, and the canonical codchi flake-url
form. It is the single authority for everything that requires Nix evaluation.

`create_machine` and `set_modules` carry a plain ordered `Vec<ModuleSpec>` — no
opaque token. The server treats every submitted list as untrusted and
re-validates the full pure-invariant set itself (machine id, case-insensitive
collision where applicable, canonical-form URLs, duplicate entries, ordering,
nixpkgs-source cardinality) before inserting a machine/job or mutating desired
state. A token would only gate "did the client call `resolve_config` first," but
both `resolve_config` and the create/rebuild job evaluate the same flake, so the
gate proves nothing the re-validation (and the job's own eval of module attrs)
does not already enforce. Dropping it also removes salt storage, replay/expiry,
and a typed mismatch error from the design.

Division of labor (the rule that keeps business logic on the server):

- **Eval-bound, `resolve_config` only:** which module attrs exist, nixpkgs-input
  presence, canonicalization of a raw url into the codchi flake-url form.
- **Pure, `set_modules`/`create_machine` (sync, honoring P6):** canonical-form
  check (reject non-canonical urls rather than canonicalizing — that authority
  lives in `resolve_config`), duplicate-url rejection, ordering, nixpkgs
  cardinality. `set_modules` cannot confirm a module attr *exists* in the flake
  (that needs eval); a bogus attr fails the subsequent rebuild job's eval, as in
  beta.

Because `set_modules` re-validates the complete list, the CLI's `module
add/set/delete` are client-side read-modify-write sugar (MS7) with **zero
authority**: no client can persist a configuration the server would not have
produced itself.

Duplicate, rebuild, and update operate on already-committed configuration and
never re-resolve modules.

### MS15 — Concrete Phase-4 schema and the Phase-6 boundary

Phase 4 ships these durable tables plus the pure read/derivation logic over
them; it does **not** ship the create/duplicate/rebuild write path (that needs
the generation commit, Phase 6) or the job table (Phase 5).

- **`machines`** — `id TEXT PRIMARY KEY` (case-sensitive spelling, MS6); a
  `UNIQUE INDEX ON machines(id COLLATE NOCASE)` for the case-insensitive
  collision rule (MS6); `active_generation_id` FK to the (Phase-6) generations
  table; `state_version INTEGER NOT NULL` — the per-machine *state*-format
  version for state migration (MS12), set to the current baseline (≥1) by create
  and to 0 by beta migration. No lock column, no `created_at`, no
  `schema_version` (that is the global DB version), no platform/runtime columns —
  all of those are derived (MS9/MS12).
- **`machine_modules`** — `(machine_id FK, position INTEGER, url TEXT,
  is_nixpkgs_source BOOLEAN)`, `UNIQUE(machine_id, position)` and
  `UNIQUE(machine_id, url)` (ordering + duplicate rejection, MS7/MS8), read
  ordered by `position`.
- **`secret_values`** — `(machine_id FK, key, plaintext,
  last_declared_description)` keyed by `(machine, key)` (MS10). There is no
  stored declarations table: declaration membership and live descriptions for
  *declared* keys are projected from the active generation's schema; the stored
  `last_declared_description` only backs *obsolete* entries (MS10).

Boundary calls:

- **The flake lock is derived, not a `machines` column.** Each generation stores
  its own `flake.lock` content/hash; the machine's current lock *is* the active
  generation's lock, so `MachineDetail.flake_lock_hash` reads from the active
  generation (`None` only on the synthesized creating view). MS9's "the machine's
  stored lock" therefore means "the active generation's lock."
- **The `generations` table, lock/config-snapshot storage, and the atomic
  birth/commit transaction are Phase 6.** Phase 4 defines the `active_generation_id`
  FK *shape* but cannot create a machine row (birth = the Phase-6 commit). Phase
  4's deliverable is the tables, the collision/ordering constraints, and the
  `ConfigurationStatus` / `list_secrets` / request-validation derivations.

## `codchi-api` contract revision (first implementation task)

The Phase-4 decisions above are the explicit `codchi-api` revisions R11 requires,
but the frozen contract crate **has not been edited yet** — it still carries the
pre-grill shape. Applying these edits (plus the `testing.rs` mock and the OpenAPI
snapshot) is the first implementation task of Phase 4, before any SQLite work.
Locked divergences, with the frozen-code anchor:

- **Drop `MachineView.schema_version: u32`** (`dto/machine.rs:51`,
  `testing.rs:73`). Schema migration is global (MS12); the per-machine version
  that survives is the **`state_version` column** on the `machines` table
  (MS15) — a DB column, *not* re-added as a wire field unless a later phase needs
  it. This is the "keeping schema_version" answer: the *name* dies, the
  *per-machine versioning idea* lives on as `state_version`.
- **Fix `MachineView.active_generation` doc comment** (`dto/machine.rs:42`):
  `None ⇒ creating or failed-create (R9)` → `None ⇒ synthesized in-flight-create
  view only` — there is no durable failed-create row (R9 revised, MS1).
- **`clone_machine` → `duplicate_machine`** returning `JobView<Duplicated>`
  (`service.rs:37`, `testing.rs:151/158`); `JobKind::Clone` → `Duplicate`
  (`dto/job.rs:48`); `Cloned` → `Duplicated` (`dto/machine.rs:83/86`);
  `CloneMachineEp` `/clone` → `DuplicateMachineEp` `/duplicate`
  (`endpoints.rs:423`). `clone` is a CLI-only git convenience, not an endpoint
  (00-contract Q1).
- **Add error variants**: `CreateArtifactsRetained { machine, job }`,
  `SecretNotSet { machine, key }`, `JobNotTerminal`, `JobArtifactsNotFound`
  (00-contract error catalog; MS10/JS-D2/JS-D3).
- **Add routes** `prepare_job_debug` and `delete_job_artifacts` (JS-D2) — these
  land with the **Phase 5** job system, listed here only so the contract bump is
  tracked in one place.

Open (not yet locked — do **not** edit blindly):

- **`ModuleSpec.name`** (`dto/module.rs:12`) and its doc comment still describe
  beta's bespoke `FlakeUrl` format. Whether `name` stays as a wire field, and the
  canonical `url` format, are pending the flake-URL normalizer decision
  (`v1/todo/flake-url-canonical-form.md`, MS8/MS14). Settle that before touching
  `ModuleSpec`.
- **Whether `state_version` surfaces on the wire** at all. MS15 only mandates the
  column; default to DB-only until a consumer needs it.

## Acceptance criteria

- `machines` / `machine_modules` / `secret_values` exist with the MS6 `COLLATE
  NOCASE` unique index, module `(machine_id, position)` + `(machine_id, url)`
  constraints, and `state_version` (MS15).
- `ConfigurationStatus` derives correctly without a build: `Unbuilt` only on a
  synthesized in-flight-create view; `Applied`/`NeedsRebuild` by config-snapshot
  compare (MS11).
- `list_secrets` derives the four `(declared?, has_value?)` states, and
  `get_secret` / `set_secret` / `delete_secret` honor the MS10 matrix including
  `SecretNotSet` and obsolete-key handling.
- Request validation rejects at the boundary (id rules, NOCASE collision,
  canonical-form URLs, duplicate URLs, ordering, nixpkgs cardinality) with
  `ApiError::Validation`.
- No create/duplicate write path and no job/generation tables — those land in
  Phases 5/6; Phase 4 ships schema + pure read/derivation only.
