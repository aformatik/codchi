//! Install metadata — the invariant store (DB5; the Podman `DBConfig` pattern).
//!
//! Durable, server-managed install facts seeded once on a fresh DB and left
//! untouched thereafter: `install_uuid`, the `data_dir` the DB was created
//! under, `created_by_version`, and `created_at`. Small, fixed, never
//! user-edited — distinct from the [`config`](super::config) overrides table.
//!
//! Seeding is idempotent: [`seed_or_validate`] writes the fields only on a fresh
//! DB and preserves them on reopen. The `install_uuid` and `created_*` facts are
//! immutable; the `data_dir` field is **informational** (relocating the data dir
//! is legitimate, DB5) and is therefore not a hard gate. The per-store and
//! per-machine *domain* versions used by beta-migration detection (Phase 11)
//! will join these rows later; those are distinct from `PRAGMA user_version`.

use chrono::Utc;
use rusqlite::{Connection, OptionalExtension, Transaction};
use uuid::Uuid;

pub const KEY_INSTALL_UUID: &str = "install_uuid";
pub const KEY_DATA_DIR: &str = "data_dir";
pub const KEY_CREATED_BY_VERSION: &str = "created_by_version";
pub const KEY_CREATED_AT: &str = "created_at";

/// Read a metadata field, or `None` if unset.
pub fn get(conn: &Connection, key: &str) -> rusqlite::Result<Option<String>> {
    conn.query_row("SELECT value FROM metadata WHERE key = ?1", [key], |r| {
        r.get(0)
    })
    .optional()
}

/// Seed the invariant fields on a fresh DB; on an already-seeded DB this is a
/// no-op (DB5). Detection keys on `install_uuid`: once present, the facts are
/// immutable and nothing is overwritten — a relocated `data_dir` is accepted as
/// legitimate, not rejected.
pub fn seed_or_validate(tx: &Transaction, data_dir: &str) -> rusqlite::Result<()> {
    if get(tx, KEY_INSTALL_UUID)?.is_some() {
        return Ok(());
    }
    set(tx, KEY_INSTALL_UUID, &Uuid::now_v7().to_string())?;
    set(tx, KEY_DATA_DIR, data_dir)?;
    set(tx, KEY_CREATED_BY_VERSION, env!("CARGO_PKG_VERSION"))?;
    set(tx, KEY_CREATED_AT, &Utc::now().to_rfc3339())?;
    Ok(())
}

/// Insert a metadata field. Only ever called for the one-shot seed, so a plain
/// `INSERT` is sufficient.
fn set(tx: &Transaction, key: &str, value: &str) -> rusqlite::Result<()> {
    tx.execute(
        "INSERT INTO metadata(key, value) VALUES(?1, ?2)",
        (key, value),
    )?;
    Ok(())
}
