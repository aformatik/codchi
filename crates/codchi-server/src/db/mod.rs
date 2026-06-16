//! SQLite foundation — the `Db` handle, migration runner, and accessors (DB1–DB8).
//!
//! Access is synchronous [`rusqlite`] bridged to the async runtime by
//! [`tokio_rusqlite`]: one `Connection` on a dedicated background thread, driven
//! by `conn.call(|c| …).await` (DB1). That actor thread *is* the serialization
//! point, so there is exactly **one writer** and `SQLITE_BUSY` cannot arise
//! intra-process; no read pool is introduced (DB traffic is tiny — the heavy
//! work is Nix/Podman/builds, not SQLite).
//!
//! The transaction-safety invariant ("never hold a write transaction across a
//! Podman/WSL/Nix shellout") is enforced **structurally** (DB7): the
//! [`Db::read`] / [`Db::transaction`] closures are *synchronous* and run inside
//! the actor, so there is no `.await` point inside a transaction — the violation
//! is not merely discouraged, it is impossible to express.
//!
//! `rusqlite::Error` maps to [`ApiError::Internal`], never leaking SQL text or
//! secret values onto the wire (DB7).

pub mod config;
pub mod machines;
pub mod metadata;

use std::path::Path;

use codchi_api::dto::SchemaStatus;
use codchi_api::error::ApiError;
use rusqlite::{Connection, Transaction};
use rusqlite_migration::{M, Migrations};
use tracing::error;

use crate::core::SchemaState;

/// The highest schema version this build knows how to produce — the number of
/// entries in [`migrations`]. `PRAGMA user_version` equals this in steady state
/// (DB4/DB8). Bumped by one with each new numbered `.sql` migration.
pub const MAX_SCHEMA_VERSION: u32 = 2;

/// The forward-only migration list (DB4). Numbered `.sql` files included via
/// `include_str!` for clean diffs and readable DDL; each step runs in its own
/// transaction (all-or-nothing). The runner drives `PRAGMA user_version`.
fn migrations() -> Migrations<'static> {
    Migrations::new(vec![
        M::up(include_str!("migrations/0001_init.sql")),
        M::up(include_str!("migrations/0002_machines.sql")),
    ])
}

/// The async SQLite handle (DB7). `Clone` (the underlying connection is an
/// `Arc`-backed actor handle); held in [`ServerCore`](crate::ServerCore).
#[derive(Clone)]
pub struct Db {
    conn: tokio_rusqlite::Connection,
    /// The `data_dir` label recorded in `metadata` when the DB is first seeded
    /// (informational only — relocating the data dir is legitimate, DB5).
    data_dir_label: String,
}

/// The outcome of opening + migrating the DB at startup, consumed by `main` to
/// construct [`ServerCore`](crate::ServerCore). On a catastrophic open failure
/// `db` is `None` but the daemon still serves a `Degraded` status so
/// `codchi doctor`/`status` work (DB8).
pub struct DbStartup {
    pub db: Option<Db>,
    pub schema: SchemaState,
    /// `PRAGMA user_version` captured after bring-up — the `current` reported by
    /// `server_status` when the live DB cannot be read (failed/absent).
    pub current: u32,
}

impl Db {
    /// Open the on-disk DB at `path`, applying the connection pragmas (DB7):
    /// `journal_mode=WAL`, `foreign_keys=ON`, a `busy_timeout`, and
    /// `synchronous=NORMAL`. Does **not** migrate — see [`Db::migrate`].
    pub async fn open(path: &Path) -> Result<Self, ApiError> {
        let label = path
            .parent()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| path.display().to_string());
        let conn = tokio_rusqlite::Connection::open(path)
            .await
            .map_err(open_err)?;
        Self::from_conn(conn, label).await
    }

    /// Open an in-memory DB (tests). Distinct connection per call.
    pub async fn open_memory() -> Result<Self, ApiError> {
        let conn = tokio_rusqlite::Connection::open_in_memory()
            .await
            .map_err(open_err)?;
        Self::from_conn(conn, ":memory:".to_owned()).await
    }

    async fn from_conn(
        conn: tokio_rusqlite::Connection,
        data_dir_label: String,
    ) -> Result<Self, ApiError> {
        conn.call(|c| -> rusqlite::Result<()> {
            // WAL is a no-op on `:memory:` (stays memory); the others apply
            // everywhere. `busy_timeout` is belt-and-braces — the single-actor
            // model already precludes intra-process `SQLITE_BUSY` (DB1).
            c.pragma_update(None, "journal_mode", "WAL")?;
            c.pragma_update(None, "foreign_keys", "ON")?;
            c.pragma_update(None, "busy_timeout", 5_000)?;
            c.pragma_update(None, "synchronous", "NORMAL")?;
            Ok(())
        })
        .await
        .map_err(open_err)?;
        Ok(Self {
            conn,
            data_dir_label,
        })
    }

    /// Bring the schema to [`MAX_SCHEMA_VERSION`] and seed/validate the invariant
    /// `metadata` (DB4/DB5/DB8). Synchronous-before-serve: `main` calls this
    /// between `bind` and `serve`.
    ///
    /// Guards against a **too-new** DB (the user downgraded codchi): if
    /// `user_version` exceeds the highest known migration the server refuses to
    /// operate — it never auto-downgrades and never wipes (DB4). Each migration
    /// step is transactional, so a mid-migration failure rolls back and leaves
    /// data intact (DB4).
    pub async fn migrate(&self) -> Result<(), ApiError> {
        let current = self.user_version().await?;
        if current > MAX_SCHEMA_VERSION {
            return Err(ApiError::internal(format!(
                "on-disk schema version {current} is newer than this codchi build \
                 supports (max {MAX_SCHEMA_VERSION}); refusing to operate to avoid \
                 data loss — upgrade codchi or restore a matching data directory"
            )));
        }
        self.conn
            .call(|c| migrations().to_latest(c))
            .await
            // The detail is our own DDL (no secrets/user data), so it is safe to
            // surface in the startup_error message (DB8).
            .map_err(|e| ApiError::internal(format!("schema migration failed: {e}")))?;

        let label = self.data_dir_label.clone();
        self.transaction(move |tx| metadata::seed_or_validate(tx, &label))
            .await
    }

    /// `PRAGMA user_version` — the live schema version (DB4).
    pub async fn user_version(&self) -> Result<u32, ApiError> {
        self.read(|c| c.pragma_query_value(None, "user_version", |r| r.get(0)))
            .await
    }

    /// Run a synchronous read closure on the actor thread (DB7). The closure
    /// takes owned args and borrows the connection; there is no `.await` inside
    /// it, so a transaction can never straddle a shellout.
    pub async fn read<T, F>(&self, f: F) -> Result<T, ApiError>
    where
        T: Send + 'static,
        F: FnOnce(&Connection) -> rusqlite::Result<T> + Send + 'static,
    {
        self.conn.call(move |c| f(&*c)).await.map_err(query_err)
    }

    /// Run a synchronous closure inside a transaction (DB7): begins, runs,
    /// commits on `Ok` / rolls back on `Err` (rusqlite's `Transaction` also
    /// rolls back on `Drop`).
    pub async fn transaction<T, F>(&self, f: F) -> Result<T, ApiError>
    where
        T: Send + 'static,
        F: FnOnce(&Transaction) -> rusqlite::Result<T> + Send + 'static,
    {
        self.conn
            .call(move |c| -> rusqlite::Result<T> {
                let tx = c.transaction()?;
                let out = f(&tx)?;
                tx.commit()?;
                Ok(out)
            })
            .await
            .map_err(query_err)
    }
}

/// Open the DB, migrate it, and capture the schema condition for the lifecycle
/// projection-join (DB8). `main`'s single entry point into the DB layer. Never
/// wipes data on failure — a failure surfaces as [`SchemaState::Failed`].
///
/// Enforces the DB6 permission requirement as a hard gate (the DB holds
/// plaintext secrets per R4): the `data_dir` is `0700` and `state.db` is `0600`,
/// mirroring the `0600` socket (C8). On Unix a hardening failure degrades; on
/// other targets (Windows, Phase 12) hardening is a no-op.
pub async fn bring_up(path: &Path) -> DbStartup {
    let failed = |error: ApiError| DbStartup {
        db: None,
        schema: SchemaState::Failed(error),
        current: 0,
    };

    // The data_dir must exist and be `0700` before the DB file is created, so
    // the file (and its WAL sidecars) is never momentarily world-readable.
    if let Some(parent) = path.parent()
        && let Err(error) = harden_dir(parent)
    {
        error!(%error, "cannot prepare the data directory; serving in a degraded state");
        return failed(open_err(error));
    }

    let db = match Db::open(path).await {
        Ok(db) => db,
        Err(error) => {
            // Catastrophic: the DB file could not even be opened (e.g. a bad
            // data_dir permission or a corrupt file). Serve `Degraded`.
            error!(%error, "cannot open the state database; serving in a degraded state");
            return failed(error);
        }
    };

    if let Err(error) = harden_file(path) {
        error!(%error, "cannot secure the state database file; serving in a degraded state");
        return failed(open_err(error));
    }
    match db.migrate().await {
        Ok(()) => {
            let current = db.user_version().await.unwrap_or(MAX_SCHEMA_VERSION);
            DbStartup {
                db: Some(db),
                schema: SchemaState::Ready,
                current,
            }
        }
        Err(error) => {
            error!(%error, "schema migration failed; serving in a degraded state");
            // The failed migration rolled back, so `user_version` is unchanged;
            // report it (the too-new value is the informative one here).
            let current = db.user_version().await.unwrap_or(0);
            DbStartup {
                db: Some(db),
                schema: SchemaState::Failed(error),
                current,
            }
        }
    }
}

/// Create `dir` (and parents) and enforce `0700` (DB6). The data_dir is the
/// real access-control boundary for the plaintext-secret DB and its WAL
/// sidecars — the same reasoning as the `0700` runtime dir / `0600` socket.
#[cfg(unix)]
fn harden_dir(dir: &Path) -> std::io::Result<()> {
    use std::os::unix::fs::PermissionsExt;
    std::fs::create_dir_all(dir)?;
    std::fs::set_permissions(dir, std::fs::Permissions::from_mode(0o700))
}

/// Enforce `0600` on the just-created `state.db` (DB6).
#[cfg(unix)]
fn harden_file(path: &Path) -> std::io::Result<()> {
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o600))
}

/// Non-Unix (Windows, Phase 12): just ensure the directory exists; ACL-based
/// hardening lands with the rest of the Windows port.
#[cfg(not(unix))]
fn harden_dir(dir: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dir)
}

#[cfg(not(unix))]
fn harden_file(_path: &Path) -> std::io::Result<()> {
    Ok(())
}

/// Map a connection-open / pragma failure to a wire error without leaking
/// internals. Generic over the error type because `Connection::open` yields a
/// bare `rusqlite::Error` while `call` yields the `tokio_rusqlite` wrapper.
fn open_err<E: std::fmt::Display>(err: E) -> ApiError {
    error!(%err, "state database open failed");
    ApiError::internal("failed to open the state database")
}

/// Map a runtime query failure to a wire error. Generic message + a local log:
/// rusqlite errors never embed bound parameter values, but the wire error stays
/// opaque regardless (DB7).
fn query_err<E: std::fmt::Display>(err: E) -> ApiError {
    error!(%err, "database error");
    ApiError::internal("database error")
}

/// The schema versions reported by `server_status`. `required` is always
/// [`MAX_SCHEMA_VERSION`]; `current` is the live `user_version`, falling back to
/// `fallback` when the DB is absent/failed (DB8).
pub fn schema_status(current: u32) -> SchemaStatus {
    SchemaStatus {
        current,
        required: MAX_SCHEMA_VERSION,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Migrate-from-empty (test 1): a fresh DB reaches `user_version == max`
    /// with both tables present.
    #[tokio::test]
    async fn migrate_from_empty_reaches_max_with_both_tables() {
        let db = Db::open_memory().await.unwrap();
        db.migrate().await.unwrap();
        assert_eq!(db.user_version().await.unwrap(), MAX_SCHEMA_VERSION);

        let tables: Vec<String> = db
            .read(|c| {
                let mut stmt = c.prepare(
                    "SELECT name FROM sqlite_master WHERE type='table' \
                     AND name IN ('metadata','config') ORDER BY name",
                )?;
                let rows = stmt.query_map([], |r| r.get::<_, String>(0))?;
                rows.collect()
            })
            .await
            .unwrap();
        assert_eq!(tables, vec!["config".to_owned(), "metadata".to_owned()]);
    }

    /// Idempotent reopen (test 2): migrating an already-migrated DB is a no-op,
    /// no error, version unchanged. (`:memory:` cannot be reopened, so this
    /// re-runs `migrate` on the same connection — the same idempotency claim.)
    #[tokio::test]
    async fn second_migrate_is_a_noop() {
        let db = Db::open_memory().await.unwrap();
        db.migrate().await.unwrap();
        db.migrate().await.unwrap();
        assert_eq!(db.user_version().await.unwrap(), MAX_SCHEMA_VERSION);
    }

    /// Config roundtrip (test 4): set override → read back; unset key → `None`
    /// (caller substitutes the code default); writing the default value does not
    /// persist a row (the overrides-only invariant, DB5).
    #[tokio::test]
    async fn config_overrides_only() {
        let db = Db::open_memory().await.unwrap();
        db.migrate().await.unwrap();

        // Unset key reads as absent.
        let got = db
            .read(|c| config::get_override(c, "tray.autostart".to_owned()))
            .await
            .unwrap();
        assert_eq!(got, None);

        // A non-default value is stored and read back.
        db.transaction(|tx| config::put(tx, "tray.autostart".to_owned(), "false", "true"))
            .await
            .unwrap();
        let got = db
            .read(|c| config::get_override(c, "tray.autostart".to_owned()))
            .await
            .unwrap();
        assert_eq!(got, Some("false".to_owned()));

        // Resetting to the default removes the row — the default is never stored.
        db.transaction(|tx| config::put(tx, "tray.autostart".to_owned(), "true", "true"))
            .await
            .unwrap();
        let got = db
            .read(|c| config::get_override(c, "tray.autostart".to_owned()))
            .await
            .unwrap();
        assert_eq!(got, None);
    }

    /// Metadata on open (test 5): a fresh DB seeds `install_uuid` /
    /// `created_by_version`; a second `migrate` preserves them (immutable facts).
    #[tokio::test]
    async fn metadata_seeded_once_and_preserved() {
        let db = Db::open_memory().await.unwrap();
        db.migrate().await.unwrap();

        let uuid1 = db
            .read(|c| metadata::get(c, metadata::KEY_INSTALL_UUID))
            .await
            .unwrap();
        assert!(uuid1.is_some(), "install_uuid seeded");
        let version = db
            .read(|c| metadata::get(c, metadata::KEY_CREATED_BY_VERSION))
            .await
            .unwrap();
        assert_eq!(version.as_deref(), Some(env!("CARGO_PKG_VERSION")));

        // Re-seeding preserves the original uuid (facts are immutable).
        db.migrate().await.unwrap();
        let uuid2 = db
            .read(|c| metadata::get(c, metadata::KEY_INSTALL_UUID))
            .await
            .unwrap();
        assert_eq!(uuid1, uuid2);
    }

    /// Permissions (test 7, temp-file, Unix): `bring_up` creates the data_dir
    /// `0700` and the `state.db` file `0600` — a hard gate, the DB holds
    /// plaintext secrets (DB6).
    #[cfg(unix)]
    #[tokio::test]
    async fn bring_up_hardens_dir_and_file_permissions() {
        use std::os::unix::fs::PermissionsExt;

        let tmp = tempfile::tempdir().unwrap();
        // A nested data_dir so `harden_dir` is the thing that creates it.
        let data_dir = tmp.path().join("codchi");
        let path = data_dir.join("state.db");

        let startup = bring_up(&path).await;
        assert!(
            matches!(startup.schema, SchemaState::Ready),
            "bring-up succeeds on a writable temp dir"
        );

        let dir_mode = std::fs::metadata(&data_dir).unwrap().permissions().mode();
        assert_eq!(dir_mode & 0o777, 0o700, "data_dir is 0700");
        let file_mode = std::fs::metadata(&path).unwrap().permissions().mode();
        assert_eq!(file_mode & 0o777, 0o600, "state.db is 0600");
    }

    /// Bring-up integration (DB8): a fresh temp DB reaches `Ready` with
    /// `current == max`, the lifecycle-join input the daemon serves.
    #[tokio::test]
    async fn bring_up_fresh_db_is_ready_at_max() {
        let tmp = tempfile::tempdir().unwrap();
        let startup = bring_up(&tmp.path().join("state.db")).await;
        assert!(matches!(startup.schema, SchemaState::Ready));
        assert_eq!(startup.current, MAX_SCHEMA_VERSION);
        assert!(startup.db.is_some());
    }

    /// Too-new refusal (test 3, temp-file): a `user_version` above the highest
    /// known migration yields a structured startup error and leaves the DB
    /// **unmodified** — never wiped (the data-safety gate, DB4).
    #[tokio::test]
    async fn too_new_db_refuses_without_wiping() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("state.db");

        // Bring up a normal DB and write a sentinel row, then force a too-new
        // user_version to simulate a downgrade.
        {
            let db = Db::open(&path).await.unwrap();
            db.migrate().await.unwrap();
            db.transaction(|tx| config::put(tx, "sentinel".to_owned(), "keep", "x"))
                .await
                .unwrap();
            let bump = MAX_SCHEMA_VERSION + 5;
            db.read(move |c| c.pragma_update(None, "user_version", bump))
                .await
                .unwrap();
        }

        // Reopen: migrate must refuse, and the sentinel must survive.
        let db = Db::open(&path).await.unwrap();
        let err = db.migrate().await.unwrap_err();
        assert!(matches!(err, ApiError::Internal { .. }));
        assert!(db.user_version().await.unwrap() > MAX_SCHEMA_VERSION);
        let sentinel = db
            .read(|c| config::get_override(c, "sentinel".to_owned()))
            .await
            .unwrap();
        assert_eq!(sentinel, Some("keep".to_owned()), "data not wiped");
    }
}
