//! Global config — the overrides-only KV store (DB5).
//!
//! `config(key, value)` stores **only** values that differ from the code-side
//! default (the Postgres-GUC / Firefox-prefs pattern). A typed accessor reads
//! its key, substituting the compiled-in default when the row is absent, and
//! writes through only a non-default value — resetting to the default *deletes*
//! the row. So the default is never persisted, and evolving a default or adding
//! a knob needs no schema migration (a new key + a new code default).
//!
//! Phase 3 ships the **mechanism** with an empty registry: no user-facing
//! setting exists yet (beta's `tray.autostart` / WSL knobs land in Phases
//! 12/15, which add typed getters/setters over these functions). Validation is
//! **on write** in those phases (mirroring R4's schema-checked `set_secret`), so
//! a bad value never lands.
//!
//! Accessors are plain functions over `&Connection` / `&Transaction` (no `Repo`
//! struct yet, DB7) and take **owned** args because `tokio-rusqlite`'s `call`
//! closures are `'static` (DB7).

use rusqlite::{Connection, OptionalExtension, Transaction};

/// Read the raw override for `key`, or `None` when the key is absent (the caller
/// substitutes the code default — the read half of the overrides-only pattern).
pub fn get_override(conn: &Connection, key: String) -> rusqlite::Result<Option<String>> {
    conn.query_row("SELECT value FROM config WHERE key = ?1", [key], |r| {
        r.get(0)
    })
    .optional()
}

/// Persist `value` for `key`, honoring the overrides-only invariant (DB5): a
/// value equal to `default` *clears* the override (so the default is never
/// stored); any other value is written through. This is the single write path
/// the future typed `set_*` accessors build on.
pub fn put(tx: &Transaction, key: String, value: &str, default: &str) -> rusqlite::Result<()> {
    if value == default {
        clear_override(tx, key)
    } else {
        set_override(tx, key, value.to_owned())
    }
}

/// Unconditionally store a raw override (upsert). Prefer [`put`], which enforces
/// the overrides-only invariant; this exists for callers that have already
/// excluded the default.
pub fn set_override(tx: &Transaction, key: String, value: String) -> rusqlite::Result<()> {
    tx.execute(
        "INSERT INTO config(key, value) VALUES(?1, ?2) \
         ON CONFLICT(key) DO UPDATE SET value = excluded.value",
        (key, value),
    )?;
    Ok(())
}

/// Remove an override so the key reverts to its code default (the reset path).
pub fn clear_override(tx: &Transaction, key: String) -> rusqlite::Result<()> {
    tx.execute("DELETE FROM config WHERE key = ?1", [key])?;
    Ok(())
}
