# Phase 3 — SQLite Foundation (locked decisions)

Locks the database foundation: how `codchi-server` opens, migrates, and talks to
SQLite, where the file lives, what the first migration creates, and how DB
bring-up integrates with the startup/lifecycle model. Changes to this file
require an explicit revision (same rule as `00-contract-decisions.md`,
`01-podman-store.md`, and `02-server-core.md`).

Grounded in the Phase-1/2 code on `server` (`crates/codchi-server`) and the beta
config model on `master` (`crates/beta/beta-shared/src/config`,
`crates/beta/beta-codchi/src/config`).

## Scope

Phase 3 delivers the **mechanism**, not a populated settings surface:

- A `db` module in `codchi-server`: the `Db` handle, the migration runner, and
  per-table accessors.
- Schema versioning and a forward-only migration framework.
- The first migration (`0001_init.sql`) creating exactly two tables: `metadata`
  and `config`.
- DB open + migrate wired into daemon startup, before store start.
- Real `SchemaStatus` in `server_status`.

**Out of scope** (later phases own their own migrations): machine, module,
secret, job, generation, finding, and log-index tables (Phases 4/5/6/9/10); the
beta-machine import (Phase 11, the *beta migration* — distinct from the *schema
migrations* this phase introduces); any actual user-facing config setting (the
`config` table ships empty; tray/WSL knobs arrive in Phases 15/12).

Phase 3's **only** `codchi-api` change is a deliberate **removal** (an R11
revision): the never-observed `ServerLifecycle::Migrating` variant and the
`SchemaStatus.migrating` field are deleted, because the daemon migrates
synchronously before it serves (DB8). No endpoints, no error variants, and no new
finding variants are added.

## Decisions

### DB1 — Access layer: `rusqlite` via `tokio-rusqlite`, single writer

SQLite access is synchronous `rusqlite`, bridged to the async runtime by
**`tokio-rusqlite`** — one `Connection` on a dedicated background thread, driven
by `conn.call(|c| …).await`. This is the database-actor model: the actor thread
*is* the serialization point, so there is exactly **one writer** and `SQLITE_BUSY`
cannot arise intra-process. A read pool is **not** introduced; DB traffic is tiny
and infrequent (the heavy work — Nix/Podman/builds — is not in SQLite, and job
event firehoses are JSONL-on-disk per R8). Swapping to a read pool later is a
localized change behind the `Db` facade.

`codchi-api` keeps its ban on `rusqlite`; the DB layer lives only in
`codchi-server`.

Rejected: `sqlx` (its compile-time query checks need a DB or committed offline
cache at build time, which fights the hermetic Nix build); a hand-rolled
`Mutex<Connection>` + `spawn_blocking` (re-implements `tokio-rusqlite`).

### DB2 — Terminology: *schema migration* ≠ *beta migration*

The bare word "migration" is overloaded. **Schema migration** = forward DB-schema
evolution, this phase. **Beta migration** = importing a user's pre-v1 install
(Phase 11). They share no code and no meaning. Both terms are in `CONTEXT.md`;
always qualify the word.

### DB3 — Global config lives in SQLite; no editable config file

v1 keeps the "SQLite is the single source of truth" line. There is **no
hand-editable config file** (beta's `config.toml` is retired; beta migration
imports its values). The split is:

- **Deployment / bootstrap knobs** (`data_dir`, socket path) → **environment
  variables** (`codchi-shared::paths`, e.g. `CODCHI_DATA_DIR`). This is the
  declarative escape hatch a NixOS/home-manager user sets via the environment.
- **User preferences** (tray autostart, future toggles) → the SQLite `config`
  table, mutated through the API when their consuming phases land.

Considered and rejected: a `nix.conf`/`containers.conf`-style server-read
editable file. It would buy declarative file-management, but codchi's global
config is tiny and the genuinely-declarative values (data_dir) are already env
vars, so a second config authority the server must reconcile isn't worth it.

### DB4 — Schema version = `PRAGMA user_version`; runner = `rusqlite_migration` + a too-new guard

- The schema-migration counter is **`PRAGMA user_version`** — the SQLite-native,
  transaction-safe integer (the Nix store DB pattern). No `schema_version` table.
- The runner is **`rusqlite_migration`** (drives `user_version`, runs pending
  steps in order, and handles SQLite's nasty cases — most `ALTER TABLE`s require
  the 12-step table-rebuild with `foreign_keys = OFF` *during* the migration and
  a `foreign_key_check` after; buying the tested handling now pays off in Phase
  4+). Migrations are **forward-only** (no down-migrations in v1) and **numbered
  `.sql` files** included via `include_str!` (clean diffs, readable DDL). Each
  step is applied in a transaction — all-or-nothing, so a mid-migration failure
  rolls back and leaves data intact.
- On top of the runner we own a **too-new guard**: if `user_version` exceeds the
  highest known migration (the user downgraded codchi), the server **refuses to
  operate** — never auto-downgrades, never wipes. See DB8 for how that refusal
  surfaces.

### DB5 — Two tables, two patterns: invariant `metadata`, key-value `config`

`0001_init.sql` creates two key-value-shaped tables that serve two different
real-world roles:

- **`metadata`** — the *invariant store* (the Podman `DBConfig` pattern):
  durable install facts validated on open — `install_uuid`, the `data_dir` the
  DB was created under, `created_by_version`, `created_at`. Small, fixed,
  server-managed; the user never edits it. On open the daemon seeds it on a fresh
  DB and validates it on an existing one (the `data_dir` field is informational —
  relocating the data dir is legitimate — not a hard gate). This is also where
  the per-store and per-machine **domain** versions used by beta-migration
  detection will live later; those are distinct from `user_version`.
- **`config`** — the *overrides-only key-value store* (the Postgres-GUC /
  Firefox-prefs pattern): `config(key TEXT PRIMARY KEY, value TEXT NOT NULL)`,
  storing **only** values that differ from the code-side default. A typed
  `GlobalConfig` struct in Rust holds the **registry + defaults**; a missing key
  reads as its default and the default is never written. New toggle = a new key +
  a code default, **no schema migration**. Validation is **on write** (the
  future `set_*` API rejects bad values, mirroring R4's schema-checked
  `set_secret`), so a bad value never lands; dead/obsolete keys are swept by a
  `doctor` check (Phase 10). Ships empty in Phase 3.

Why not typed single-row columns for `config`: codchi's config set is small but
*sparse, optional, and version-evolving*, and we want evolving defaults +
"did the user override this?" semantics with no migration per knob — exactly the
KV/registry case. Typed columns are the right shape for `metadata` (small/fixed/
required), which is why the two tables use different patterns deliberately.

### DB6 — File: `data_dir()/state.db`, `0600` (dir `0700`)

The DB is `data_dir()/state.db` — the same tree as `logs/`, resolved from
`CODCHI_DATA_DIR`/`XDG_DATA_HOME` (no chicken-and-egg, since `data_dir` is
env/XDG and never DB-stored). Created on first start. WAL mode adds sidecars
`state.db-wal` / `state.db-shm` beside it.

Because the DB holds **plaintext secrets** (R4: plaintext for v1; access control
*is* the file/transport boundary), the file is **`0600`** and `data_dir` is
**`0700`** — a hard requirement, mirroring the `0600` socket (C8), not
best-effort. Single relocation knob is the existing `CODCHI_DATA_DIR`; there is
no separate `CODCHI_DB_PATH`.

WAL consequence to record for Phases 10/11: backups and beta-migration copies
must `wal_checkpoint(TRUNCATE)` (or copy all three files) — a plain `cp state.db`
is not a valid snapshot.

### DB7 — `Db` facade: closure helpers + plain accessors; the txn-safety invariant is structural

`codchi-server/src/db/`:

- `mod.rs` — the `Db` handle (`Clone`, wrapping a `tokio_rusqlite::Connection`):
  `open` (sets `journal_mode=WAL`, `foreign_keys=ON`, `busy_timeout`,
  `synchronous=NORMAL` via a `call` at open), `migrate`, and two async helpers:
  - `read<T>(impl FnOnce(&Connection) -> rusqlite::Result<T>)`
  - `transaction<T>(impl FnOnce(&Transaction) -> rusqlite::Result<T>)` — begins,
    runs, commits on `Ok` / rolls back on `Err` (rusqlite's `Transaction` also
    rolls back on `Drop`).
- `migrations/0001_init.sql` (+ the migration list).
- `config.rs`, `metadata.rs` — plain accessor **functions** over `&Connection` /
  `&Transaction`. No `XxxRepo` structs yet; **later phases introduce repository
  structs only when a domain's logic earns them.**

`tokio-rusqlite`'s `call` closures are `'static + Send`, so accessors take
**owned** args (move the key/value in), not borrows.

**The invariant is enforced structurally, not by discipline.** Because the
closures are *synchronous* and run inside the actor, there is no `.await` point
inside a transaction — so the R-constraint "never hold a write transaction across
a Podman/WSL/Nix shellout" becomes *impossible to express*, not merely
discouraged.

`rusqlite::Error` maps to `ApiError::Internal { message }`, never leaking SQL
text or secret values into the wire error or logs.

`ServerCore` gains a `db: Db` field; `main` opens + migrates the DB **before**
constructing `ServerCore::new(...)`, so a non-migrated DB can never reach the
core.

### DB8 — Startup integration: sync-migrate-before-serve; lifecycle becomes a projection-join

The architecture lifecycle is `Starting → Migrating → Healthcheck → Ready →
Degraded → Stopping`, and "open/migrate SQLite; *immediately* verify/start the
store" — DB before store. Phase-3 schema migration is a handful of `CREATE
TABLE`s (sub-millisecond), and store start has **no dependency on DB content**
yet (no machine/store tables are read at boot until Phase 4).

- **Sync open+migrate in `main`, between `bind` and `serve`, DB-before-store.** A
  client connecting mid-migration simply waits in the listen backlog until
  `serve()` accepts — fine for sub-ms DDL.
- **No `Migrating` lifecycle.** Because migration is synchronous-before-serve, a
  client can never observe a mid-migration daemon, so `ServerLifecycle::Migrating`
  and `SchemaStatus.migrating` are **removed** from `codchi-api` (an R11
  revision), not merely deferred — a never-reachable state is worse than no
  state. If a future phase introduces a slow async migration/backfill, it
  re-introduces the variant *and* the `watch<SchemaState>`-projected-live machine
  together, as a deliberate addition.
- **`SchemaStatus`** is read from the DB: `current = user_version`,
  `required = max known migration` (equal in steady state).
- **Failure handling — and the SC5 revision.** A DB-open / migration / too-new
  failure must surface as `Degraded` + a structured `startup_error`, and **never**
  wipe data. That failure is **not** a `StoreCondition`, so smuggling it through
  the store path would lie (a schema problem showing as `store.unavailable`).
  Instead the lifecycle/`startup_error` projection takes a **second input** — a
  small `SchemaState { Ready | Failed(ApiError) }` — alongside `StoreCondition`
  and the shutdown flag. This **revises SC5**: `lifecycle`/`startup_error` are a
  projection-**join** over all subsystem conditions, not a projection of
  `StoreCondition` alone (`StoreStatus` and `store.unavailable` stay
  store-only). See the forward note in `02-server-core.md`.

  ```text
  lifecycle = Stopping            if shutdown
            = Degraded            if SchemaState::Failed   (startup_error set)
            = <StoreCondition projection>   otherwise
  ```

  Too-new and migration failures map to the **existing** `ApiError::Internal`
  (no new error variant; `ApiError::SchemaMigrationRequired` exists but targets
  the opposite "DB behind binary" case, which Phase 3 auto-resolves by running
  migrations). **No `schema.*` `FindingCode`** is emitted in Phase 3 — findings
  are Phase 10's domain; the dedicated finding + an explicit too-new error
  variant are deferred to Phase 10 as an R11 revision when `doctor` wires schema
  checks. The `startup_error` message carries the detail meanwhile.

## Tests

Following the established style — pure-function units for the projection,
integration tests for `Db` against in-memory (`:memory:`) by default, temp-file
where the filesystem is the point. No `#[ignore]` (no Podman/real-store
dependency).

1. **Migrate-from-empty** → `user_version == max`, both tables present.
2. **Idempotent reopen** → opening an already-migrated DB is a no-op, no error,
   version unchanged.
3. **Too-new refusal** (temp-file reopen) → `user_version > max` ⇒ structured
   startup error, DB **unmodified**, **not** wiped (the data-safety gate).
4. **Config roundtrip** → set override → read back; unset key → code default;
   the default value is **not** written (overrides-only invariant).
5. **Metadata on open** → fresh DB seeds `install_uuid`/`created_by_version`;
   reopen preserves them.
6. **Lifecycle projection** (pure-fn) → `SchemaState::Failed` ⇒ `Degraded` +
   `startup_error`, composed with `StoreCondition` and shutdown.
7. **Permissions** (temp-file, Unix) → created file is `0600`, `data_dir` is
   `0700`.

## Cross-doc impact

- `codchi-api` (R11 revision) — `ServerLifecycle::Migrating` and
  `SchemaStatus.migrating` **removed**; `openapi.json` regenerated. The only
  contract change in Phase 3.
- `01-architecture.md` — lifecycle list drops `Migrating`, with a note on why.
- `02-server-core.md` SC5 — generalized to a projection-join (forward note
  added there; DB8 here is the authority).
- `CONTEXT.md` — added *Schema migration* / *Beta migration*; refined *Store
  condition* so lifecycle no longer hinges on the store alone.
- `PLAN.md` "persisted global config" — honestly reframed as the
  config-persistence *foundation* (table + accessor), empty of real settings in
  Phase 3. PLAN itself is unchanged (it is the stable plan; this spec records the
  reframing).
- `02-state-and-generations.md` — its "global config" bullet remains accurate
  (SQLite owns global config); DB3 adds the "no editable file / env for
  deployment knobs" refinement.
