//! Machine-state accessors over the Phase-4 schema (`0002_machines.sql`): the
//! `machines` / `machine_modules` / `secret_values` tables (MS6/MS7/MS8/MS10/
//! MS15).
//!
//! These are the **pure SQL** seam — synchronous closures run inside the DB
//! actor (DB7); no derivation or validation lives here (that is domain logic in
//! [`crate::core`]). A machine row is born only at its first generation commit
//! (MS1), which is the Phase-6 write path, so nothing here inserts a `machines`
//! row outside tests; Phase 4 ships the reads plus the module/secret-value
//! writes that operate on an already-born machine.

use codchi_api::dto::ModuleSpec;
use codchi_api::ids::MachineId;
use rusqlite::{Connection, Transaction, params};

/// A durable `machines` row (MS12/MS15) — deliberately thin: identity, the
/// per-machine state-format version, and the active-generation pointer. Every
/// other machine fact is derived (MS12).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MachineRow {
    pub id: MachineId,
    /// Per-machine *state*-format version for state migration (MS12/MS15).
    pub state_version: i64,
    /// FK into the Phase-6 `generations` table; `None` is never a committed row
    /// in practice (born-at-commit, MS1), only a not-yet-Phase-6 fixture.
    pub active_generation_id: Option<i64>,
}

/// A stored secret *value* row (MS10). Declaration membership for *declared* keys
/// comes from the active generation, not this table. `description` is the key's
/// last-declared description, refreshed on every config eval while the key stays
/// declared and frozen once it drops out — so it is the live description for a
/// stored declared value and the salvaged one for an *obsolete* value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StoredSecret {
    pub key: String,
    pub plaintext: String,
    pub description: String,
}

/// All committed machine ids, ascending by the case-sensitive stored spelling
/// (MS12 deterministic order; matches beta's name sort). In Phase 4 this is the
/// committed-rows half of `list_machines`; the synthesized in-flight-create
/// views (MS1) are unioned in by the Phase-5 job layer.
pub fn list_machine_ids(conn: &Connection) -> rusqlite::Result<Vec<MachineId>> {
    let mut stmt = conn.prepare("SELECT id FROM machines ORDER BY id ASC")?;
    let rows = stmt.query_map([], |r| r.get::<_, String>(0).map(MachineId))?;
    rows.collect()
}

/// Read a single machine row by its exact (case-sensitive) id (MS6), or `None`
/// if no committed machine has that spelling.
pub fn get_machine_row(conn: &Connection, id: &MachineId) -> rusqlite::Result<Option<MachineRow>> {
    conn.query_row(
        "SELECT id, state_version, active_generation_id FROM machines WHERE id = ?1",
        params![id.0],
        |r| {
            Ok(MachineRow {
                id: MachineId(r.get(0)?),
                state_version: r.get(1)?,
                active_generation_id: r.get(2)?,
            })
        },
    )
    .map(Some)
    .or_else(|e| match e {
        rusqlite::Error::QueryReturnedNoRows => Ok(None),
        other => Err(other),
    })
}

/// Whether another machine collides case-insensitively with `id` (MS6, P4). The
/// `machines_id_nocase` unique index makes this an index probe. `exclude` skips a
/// machine's own row (so a rename-in-place would not collide with itself; v1 has
/// no rename, but `duplicate` and migration reuse this).
pub fn nocase_collision_exists(
    conn: &Connection,
    id: &MachineId,
    exclude: Option<&MachineId>,
) -> rusqlite::Result<bool> {
    let exclude = exclude.map(|m| m.0.as_str()).unwrap_or("");
    conn.query_row(
        "SELECT EXISTS(SELECT 1 FROM machines \
         WHERE id = ?1 COLLATE NOCASE AND id <> ?2)",
        params![id.0, exclude],
        |r| r.get::<_, bool>(0),
    )
}

/// The machine's ordered desired modules (MS7/MS8), read by `position`.
pub fn read_modules(conn: &Connection, id: &MachineId) -> rusqlite::Result<Vec<ModuleSpec>> {
    let mut stmt = conn.prepare(
        "SELECT url, is_nixpkgs_source FROM machine_modules \
         WHERE machine_id = ?1 ORDER BY position ASC",
    )?;
    let rows = stmt.query_map(params![id.0], |r| {
        Ok(ModuleSpec {
            url: r.get(0)?,
            is_nixpkgs_source: r.get::<_, bool>(1)?,
        })
    })?;
    rows.collect()
}

/// Whole-list replace of a machine's desired modules (MS7 `set_modules` is PUT,
/// not PATCH). Deletes the existing rows and re-inserts `modules` in order, so
/// `position` always matches the request order. Must run inside a transaction;
/// the caller has already validated the list (no duplicate urls, ordering,
/// nixpkgs cardinality — MS8/MS14).
pub fn replace_modules(
    tx: &Transaction,
    id: &MachineId,
    modules: &[ModuleSpec],
) -> rusqlite::Result<()> {
    tx.execute(
        "DELETE FROM machine_modules WHERE machine_id = ?1",
        params![id.0],
    )?;
    let mut stmt = tx.prepare(
        "INSERT INTO machine_modules (machine_id, position, url, is_nixpkgs_source) \
         VALUES (?1, ?2, ?3, ?4)",
    )?;
    for (position, m) in modules.iter().enumerate() {
        stmt.execute(params![id.0, position as i64, m.url, m.is_nixpkgs_source])?;
    }
    Ok(())
}

/// All stored secret values for a machine (MS10). The caller unions these with
/// the active generation's declared keys to derive `list_secrets`.
pub fn read_secret_values(
    conn: &Connection,
    id: &MachineId,
) -> rusqlite::Result<Vec<StoredSecret>> {
    let mut stmt = conn.prepare(
        "SELECT key, plaintext, description FROM secret_values \
         WHERE machine_id = ?1 ORDER BY key ASC",
    )?;
    let rows = stmt.query_map(params![id.0], |r| {
        Ok(StoredSecret {
            key: r.get(0)?,
            plaintext: r.get(1)?,
            description: r.get(2)?,
        })
    })?;
    rows.collect()
}

/// A single stored secret value (MS10), or `None` if unset.
pub fn get_secret_value(
    conn: &Connection,
    id: &MachineId,
    key: &str,
) -> rusqlite::Result<Option<StoredSecret>> {
    conn.query_row(
        "SELECT key, plaintext, description FROM secret_values \
         WHERE machine_id = ?1 AND key = ?2",
        params![id.0, key],
        |r| {
            Ok(StoredSecret {
                key: r.get(0)?,
                plaintext: r.get(1)?,
                description: r.get(2)?,
            })
        },
    )
    .map(Some)
    .or_else(|e| match e {
        rusqlite::Error::QueryReturnedNoRows => Ok(None),
        other => Err(other),
    })
}

/// Set (insert or replace) a declared key's value (MS10). The caller has already
/// confirmed the key is declared by the active generation and passes its current
/// `description`, which is stored on the row (and refreshed here so a freshly-set
/// value carries the live description, MS10/MS15).
pub fn set_secret_value(
    tx: &Transaction,
    id: &MachineId,
    key: &str,
    plaintext: &str,
    description: &str,
) -> rusqlite::Result<()> {
    tx.execute(
        "INSERT INTO secret_values (machine_id, key, plaintext, description) \
         VALUES (?1, ?2, ?3, ?4) \
         ON CONFLICT(machine_id, key) DO UPDATE SET \
            plaintext = excluded.plaintext, description = excluded.description",
        params![id.0, key, plaintext, description],
    )?;
    Ok(())
}

/// Refresh the stored `description` of every value row whose key is still
/// declared by the just-committed generation (MS10). Run as part of a config eval
/// (create/rebuild/update): each `(key, description)` pair updates an existing
/// value row only — declared-but-unset keys have no row, and obsolete keys (not
/// in `declared`) are left untouched so their last-declared description is frozen.
pub fn refresh_secret_descriptions(
    tx: &Transaction,
    id: &MachineId,
    declared: &[(String, String)],
) -> rusqlite::Result<()> {
    let mut stmt =
        tx.prepare("UPDATE secret_values SET description = ?3 WHERE machine_id = ?1 AND key = ?2")?;
    for (key, description) in declared {
        stmt.execute(params![id.0, key, description])?;
    }
    Ok(())
}

/// Delete a secret value (MS10). Used for both a declared key (the declaration
/// survives in the generation schema; the entry becomes unset) and an obsolete
/// value (the row is the entire entry). Returns whether a row was removed.
pub fn delete_secret_value(tx: &Transaction, id: &MachineId, key: &str) -> rusqlite::Result<bool> {
    let n = tx.execute(
        "DELETE FROM secret_values WHERE machine_id = ?1 AND key = ?2",
        params![id.0, key],
    )?;
    Ok(n > 0)
}

#[cfg(test)]
pub(crate) mod fixtures {
    //! Born-at-commit is Phase 6, so tests need a way to materialize a committed
    //! machine row directly. These insert fixtures bypass the (Phase-6) commit
    //! transaction to exercise the Phase-4 schema + accessors in isolation.
    use super::*;

    /// Insert a bare committed machine row (active_generation_id NULL, so the FK
    /// to the not-yet-existent Phase-6 `generations` table is never exercised).
    pub fn insert_machine(tx: &Transaction, id: &str, state_version: i64) -> rusqlite::Result<()> {
        tx.execute(
            "INSERT INTO machines (id, state_version, active_generation_id) VALUES (?1, ?2, NULL)",
            params![id, state_version],
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::fixtures::insert_machine;
    use super::*;
    use crate::db::Db;

    fn module(url: &str, nixpkgs: bool) -> ModuleSpec {
        ModuleSpec {
            url: url.to_owned(),
            is_nixpkgs_source: nixpkgs,
        }
    }

    async fn migrated() -> Db {
        let db = Db::open_memory().await.unwrap();
        db.migrate().await.unwrap();
        db
    }

    /// MS6: the case-insensitive unique index is the **backstop** — it rejects a
    /// raw `foo` insert once `Foo` exists (surfacing as the generic DB
    /// `Internal`, since it is defense-in-depth, not the user-facing path; the
    /// boundary check that returns `Validation` lives in `core::machines`,
    /// `reject_if_id_taken`). It also preserves the exact stored spelling, and the
    /// `nocase_collision_exists` probe the boundary uses agrees with the index.
    #[tokio::test]
    async fn nocase_index_rejects_case_variant() {
        let db = migrated().await;
        db.transaction(|tx| insert_machine(tx, "Foo", 1))
            .await
            .unwrap();

        let err = db
            .transaction(|tx| insert_machine(tx, "foo", 1))
            .await
            .unwrap_err();
        assert!(matches!(err, codchi_api::error::ApiError::Internal { .. }));

        // The exact spelling is preserved and is the only row.
        let ids = db.read(list_machine_ids).await.unwrap();
        assert_eq!(ids, vec![MachineId("Foo".to_owned())]);

        // The collision probe agrees, and excluding the row itself clears it.
        let foo = MachineId("foo".to_owned());
        assert!(
            db.read(move |c| nocase_collision_exists(c, &foo, None))
                .await
                .unwrap()
        );
        let exact = MachineId("Foo".to_owned());
        assert!(
            !db.read(move |c| nocase_collision_exists(c, &exact.clone(), Some(&exact)))
                .await
                .unwrap()
        );
    }

    /// MS7/MS8: modules round-trip in order; `replace_modules` is whole-list PUT;
    /// `UNIQUE(machine_id, url)` rejects a duplicate canonical url.
    #[tokio::test]
    async fn modules_round_trip_in_order_and_reject_duplicates() {
        let db = migrated().await;
        db.transaction(|tx| insert_machine(tx, "m", 1))
            .await
            .unwrap();

        let wanted = vec![module("a", false), module("b", true), module("c", false)];
        let w = wanted.clone();
        db.transaction(move |tx| replace_modules(tx, &MachineId("m".to_owned()), &w))
            .await
            .unwrap();

        let got = db
            .read(|c| read_modules(c, &MachineId("m".to_owned())))
            .await
            .unwrap();
        assert_eq!(got, wanted, "order and contents round-trip exactly");

        // Whole-list replace shrinks and reorders.
        let next = vec![module("c", false), module("a", true)];
        let n = next.clone();
        db.transaction(move |tx| replace_modules(tx, &MachineId("m".to_owned()), &n))
            .await
            .unwrap();
        let got = db
            .read(|c| read_modules(c, &MachineId("m".to_owned())))
            .await
            .unwrap();
        assert_eq!(got, next);

        // Duplicate url within one replace is a constraint violation.
        let dup = vec![module("x", false), module("x", true)];
        let err = db
            .transaction(move |tx| replace_modules(tx, &MachineId("m".to_owned()), &dup))
            .await
            .unwrap_err();
        assert!(matches!(err, codchi_api::error::ApiError::Internal { .. }));
    }

    /// MS10: deleting a machine cascades over its modules and secret values.
    #[tokio::test]
    async fn delete_machine_cascades_modules_and_secrets() {
        let db = migrated().await;
        db.transaction(|tx| {
            insert_machine(tx, "m", 1)?;
            replace_modules(tx, &MachineId("m".to_owned()), &[module("a", false)])?;
            set_secret_value(tx, &MachineId("m".to_owned()), "TOKEN", "v", "api token")?;
            Ok(())
        })
        .await
        .unwrap();

        db.transaction(|tx| {
            tx.execute("DELETE FROM machines WHERE id = 'm'", [])?;
            Ok(())
        })
        .await
        .unwrap();

        let mods = db
            .read(|c| read_modules(c, &MachineId("m".to_owned())))
            .await
            .unwrap();
        let secs = db
            .read(|c| read_secret_values(c, &MachineId("m".to_owned())))
            .await
            .unwrap();
        assert!(
            mods.is_empty() && secs.is_empty(),
            "FK cascade removed children"
        );
    }

    /// MS10: set is upsert; get reads back; delete removes and reports it.
    #[tokio::test]
    async fn secret_value_set_get_delete() {
        let db = migrated().await;
        db.transaction(|tx| insert_machine(tx, "m", 1))
            .await
            .unwrap();
        let m = MachineId("m".to_owned());

        db.transaction({
            let m = m.clone();
            move |tx| set_secret_value(tx, &m, "TOKEN", "first", "old desc")
        })
        .await
        .unwrap();
        db.transaction({
            let m = m.clone();
            move |tx| set_secret_value(tx, &m, "TOKEN", "second", "new desc")
        })
        .await
        .unwrap();

        let got = db
            .read({
                let m = m.clone();
                move |c| get_secret_value(c, &m, "TOKEN")
            })
            .await
            .unwrap()
            .unwrap();
        assert_eq!(got.plaintext, "second", "upsert overwrites the value");
        assert_eq!(
            got.description, "new desc",
            "upsert refreshes the description"
        );

        let removed = db
            .transaction({
                let m = m.clone();
                move |tx| delete_secret_value(tx, &m, "TOKEN")
            })
            .await
            .unwrap();
        assert!(removed);
        let removed_again = db
            .transaction(move |tx| delete_secret_value(tx, &m, "TOKEN"))
            .await
            .unwrap();
        assert!(!removed_again, "second delete is a no-op");
    }

    /// MS10: a config eval refreshes descriptions of still-declared value rows and
    /// leaves obsolete (no-longer-declared) rows frozen at their last description.
    #[tokio::test]
    async fn refresh_descriptions_updates_declared_and_freezes_obsolete() {
        let db = migrated().await;
        db.transaction(|tx| insert_machine(tx, "m", 1))
            .await
            .unwrap();
        let m = MachineId("m".to_owned());

        db.transaction({
            let m = m.clone();
            move |tx| {
                set_secret_value(tx, &m, "KEEP", "v1", "keep v1 desc")?;
                set_secret_value(tx, &m, "DROP", "v2", "drop v1 desc")
            }
        })
        .await
        .unwrap();

        // A later generation still declares KEEP (with a new description) but no
        // longer declares DROP.
        db.transaction({
            let m = m.clone();
            move |tx| {
                refresh_secret_descriptions(
                    tx,
                    &m,
                    &[("KEEP".to_owned(), "keep v2 desc".to_owned())],
                )
            }
        })
        .await
        .unwrap();

        let secs = db.read(move |c| read_secret_values(c, &m)).await.unwrap();
        let by: Vec<_> = secs
            .iter()
            .map(|s| (s.key.as_str(), s.description.as_str()))
            .collect();
        assert_eq!(
            by,
            vec![("DROP", "drop v1 desc"), ("KEEP", "keep v2 desc")],
            "declared key refreshed; obsolete key frozen at last description"
        );
    }
}
