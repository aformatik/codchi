//! `codchi-api` — the v1 wire contract shared by `codchi-server` and its
//! clients (CLI, tray).
//!
//! This crate is the single source of truth for the HTTP API: the DTOs, the
//! stable [`error::ApiError`] catalog, the [`events::Event`] stream model, the
//! [`service::CodchiService`] trait, the [`service::ROUTES`] URL mapping, and
//! OpenAPI generation. It holds no transport, storage, or platform code — see
//! the crate rules in `v1/phases/00-contract-decisions.md`.
//!
//! Downstream code can develop against [`testing::MockCodchiService`] without a
//! running server.

pub mod dto;
pub mod endpoints;
pub mod error;
pub mod events;
pub mod ids;
pub mod openapi;
pub mod service;
pub mod testing;
pub mod version;

// Flat re-exports of the most-used items.
pub use endpoints::{Endpoint, Method, ROUTES, ResponseShape, Route, route};
pub use error::ApiError;
pub use events::{Event, EventStreamOpts, LogLevel, NixBuildStatus, PhaseStatus};
pub use ids::{EventSeq, FindingId, GenerationId, JobId, MachineId, StoreGenerationId};
pub use service::{CodchiService, EventStream};
pub use version::{API_PREFIX, API_VERSION};

pub use dto::*;
