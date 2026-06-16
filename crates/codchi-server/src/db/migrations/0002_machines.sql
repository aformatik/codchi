-- 0002_machines — Phase 4 machine state (MS6/MS7/MS8/MS10/MS12/MS15).
--
-- The durable machine model: identity + desired configuration + secret values.
-- Deliberately thin (MS12): no `created_at`, no `flake.lock` column, no
-- platform/runtime columns, no per-machine `schema_version` — all derived. A
-- machine is *born at its first generation commit* (MS1), which is the Phase-6
-- write path; Phase 4 ships these tables plus the pure read/derivation logic
-- only, so nothing inserts a `machines` row yet.
--
-- All tables are STRICT (TEXT/INTEGER enforced by SQLite, not convention). The
-- `is_nixpkgs_source` flag is stored as INTEGER 0/1 (STRICT has no BOOLEAN).

-- A machine: the case-sensitive id (MS6), the per-machine state-format version
-- for *state* migration (MS12/MS15 — distinct from the global DB schema version;
-- 0 for beta-migrated machines, >=1 otherwise), and the active-generation
-- pointer (MS15). The column is a plain nullable INTEGER here: the actual
-- `REFERENCES generations(id)` constraint is added in Phase 6, when the
-- `generations` table and the atomic birth/commit transaction land. (It cannot
-- be declared now — with `foreign_keys=ON` SQLite resolves an FK parent at
-- statement-prepare time, so any insert into `machines` would fail against the
-- not-yet-created parent, even with a NULL key.) No `machines` row exists until
-- Phase 6 regardless: a machine is born only at its first generation commit
-- (MS1), so Phase 4 ships the read/derivation logic and the module/secret-value
-- writes over an already-born machine, not the birth itself.
CREATE TABLE machines (
    id                   TEXT PRIMARY KEY NOT NULL,
    state_version        INTEGER NOT NULL,
    active_generation_id INTEGER
) STRICT;

-- Case-insensitive collision rule (MS6, Phase-0 P4): a stored `Foo` blocks
-- creation of `foo`. The ordinary primary key stays case-sensitive (exact
-- spelling round-trips for reads, paths, FKs, events); this extra index enforces
-- ASCII case-insensitive uniqueness without normalizing or silently redirecting
-- either name.
CREATE UNIQUE INDEX machines_id_nocase ON machines (id COLLATE NOCASE);

-- Ordered desired modules (MS7/MS8). `position` is the semantically significant
-- order and round-trips exactly; `url` is the module's whole durable identity
-- (modules have no name, MS8). `PRIMARY KEY (machine_id, position)` gives the
-- ordering-uniqueness constraint; `UNIQUE (machine_id, url)` rejects duplicate
-- canonical URLs per machine (MS8). Cascades when the machine is deleted.
CREATE TABLE machine_modules (
    machine_id        TEXT NOT NULL REFERENCES machines(id) ON DELETE CASCADE,
    position          INTEGER NOT NULL,
    url               TEXT NOT NULL,
    is_nixpkgs_source INTEGER NOT NULL,
    PRIMARY KEY (machine_id, position),
    UNIQUE (machine_id, url)
) STRICT;

-- Durable, user-supplied secret *values* (MS10). Declaration membership for
-- *declared* keys is still projected from the active generation's secret schema,
-- not stored here. `description` is the key's last-declared description, kept on
-- the value row and **refreshed on every config eval** for keys that are still
-- declared, then frozen once the key drops out of the schema — so an *obsolete*
-- value still shows what it was instead of a generic placeholder. It is set when
-- the value row is created (set_secret) and is NOT NULL. Cascades over all
-- values, including obsolete ones, when the machine is deleted.
CREATE TABLE secret_values (
    machine_id  TEXT NOT NULL REFERENCES machines(id) ON DELETE CASCADE,
    key         TEXT NOT NULL,
    plaintext   TEXT NOT NULL,
    description TEXT NOT NULL,
    PRIMARY KEY (machine_id, key)
) STRICT;
