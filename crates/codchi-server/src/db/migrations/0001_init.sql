-- 0001_init — the first schema migration (Phase 3, DB5).
--
-- Creates exactly two key/value-shaped tables that serve two different roles
-- (see v1/phases/03-sqlite-foundation.md DB5):
--
--   * `metadata` — the invariant store: durable, server-managed install facts
--     (install_uuid, the data_dir the DB was created under, created_by_version,
--     created_at) seeded once on a fresh DB. Never user-edited.
--   * `config`   — the overrides-only KV store: holds ONLY user preferences that
--     differ from the code-side default. Ships empty; tray/WSL knobs arrive in
--     later phases without any schema migration (a new key + a code default).
--
-- Both are STRICT so the TEXT typing is enforced by SQLite, not by convention.

CREATE TABLE metadata (
    key   TEXT PRIMARY KEY NOT NULL,
    value TEXT NOT NULL
) STRICT;

CREATE TABLE config (
    key   TEXT PRIMARY KEY NOT NULL,
    value TEXT NOT NULL
) STRICT;
