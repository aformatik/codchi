//! `codchi-server` — the v1 daemon.
//!
//! Phase 1 (C3) serves the full typed `codchi-api` contract over a per-user Unix
//! domain socket (D6). Phase 2 (SC1–SC8) introduces [`ServerCore`], the single
//! `impl CodchiService`: it owns the server-owned state (the store-condition
//! reader, the shutdown flag, the [`LogStore`]) and delegates every
//! still-unbacked domain to an internal mock. The store lifecycle is owned by
//! the [`StoreSupervisor`], the sole writer of the observed
//! [`StoreCondition`].
//!
//! [`CodchiService`]: codchi_api::CodchiService

pub mod core;
pub mod db;
pub mod logging;
pub mod logs;
pub mod mount;
pub mod platform;
pub mod router;
pub mod state;
pub mod supervisor;

pub use core::{ProbeOutcome, SchemaState, ServerCore, StoreCondition, step};
pub use db::{Db, MAX_SCHEMA_VERSION};
pub use logs::LogStore;
#[cfg(unix)]
pub use platform::PodmanStore;
#[cfg(not(unix))]
pub use platform::MockStore;
pub use platform::{Store, StoreError, StoreLogStream, StorePlatformStatus};
pub use router::build_router;
pub use state::AppState;
pub use supervisor::{StoreSupervisor, StoreSupervisorConfig};
