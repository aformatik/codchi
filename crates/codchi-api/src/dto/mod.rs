//! Data-transfer objects for the v1 contract.
//!
//! Every type here derives `Debug, Clone, Serialize, Deserialize, JsonSchema`.
//! Field names are `snake_case`; variant-bearing enums are internally tagged;
//! status-style enums are plain string enums. Response types never use
//! `deny_unknown_fields` — clients must ignore unknown fields (Q5).

pub mod config;
pub mod doctor;
pub mod exec;
pub mod generation;
pub mod job;
pub mod machine;
pub mod migration;
pub mod module;
pub mod secret;
pub mod server;

pub use config::*;
pub use doctor::*;
pub use exec::*;
pub use generation::*;
pub use job::*;
pub use machine::*;
pub use migration::*;
pub use module::*;
pub use secret::*;
pub use server::*;
