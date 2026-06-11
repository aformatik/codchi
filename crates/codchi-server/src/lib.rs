//! `codchi-server` — the v1 daemon.
//!
//! Phase 1 (C3) serves the full typed `codchi-api` contract over a per-user Unix
//! domain socket (D6). The HTTP plumbing is generic over the [`Endpoint`] catalog
//! ([`mount`]); the routes are wired to a [`CodchiService`] trait object held in
//! [`AppState`] — [`MockCodchiService`] for machine data in Phase 1 (D7), the
//! real `ServerCore` from Phase 2 on. Store lifecycle and health are already
//! real server infrastructure, maintained by [`store_manager`].
//!
//! [`Endpoint`]: codchi_api::Endpoint
//! [`CodchiService`]: codchi_api::CodchiService
//! [`MockCodchiService`]: codchi_api::testing::MockCodchiService

pub mod lifecycle;
pub mod logging;
pub mod mount;
pub mod platform;
pub mod router;
pub mod state;
pub mod store_manager;

pub use lifecycle::LifecycleHandle;
#[cfg(unix)]
pub use platform::PodmanStore;
pub use platform::{Store, StoreError, StorePlatformStatus};
pub use router::build_router;
pub use state::{AppState, InfrastructureHandle, InfrastructureSnapshot};
pub use store_manager::{StoreManager, StoreManagerConfig};
