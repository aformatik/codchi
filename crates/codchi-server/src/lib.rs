//! `codchi-server` — the v1 daemon.
//!
//! Phase 1 (C3) serves the full typed `codchi-api` contract over a per-user Unix
//! domain socket (D6). The HTTP plumbing is generic over the [`Endpoint`] catalog
//! ([`mount`]); the routes are wired to a [`CodchiService`] trait object held in
//! [`AppState`] — [`MockCodchiService`] in Phase 1 (D7), the real `ServerCore`
//! from Phase 2 on. Readiness/lifecycle is server-infrastructure state in
//! [`AppState`] (the [`lifecycle`] seam), not a service method.
//!
//! [`Endpoint`]: codchi_api::Endpoint
//! [`CodchiService`]: codchi_api::CodchiService
//! [`MockCodchiService`]: codchi_api::testing::MockCodchiService

pub mod lifecycle;
pub mod mount;
pub mod router;
pub mod state;

pub use lifecycle::LifecycleHandle;
pub use router::build_router;
pub use state::AppState;
