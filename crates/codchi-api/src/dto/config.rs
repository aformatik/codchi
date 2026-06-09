//! Config-resolution DTOs (R3).
//!
//! Module resolution — which module(s)? following which nixpkgs? — is
//! discoverable only by fetching and evaluating a flake. It is a read-only job
//! whose typed output is [`ConfigResolution`].

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::dto::module::ModuleSpec;

/// Request to resolve a flake url into its available modules.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct ResolveConfigRequest {
    /// Flake url to fetch and evaluate (codchi flake-url form, the `#attr` part
    /// may be omitted — resolution discovers the available attrs).
    pub url: String,
}

/// Typed output of `resolve_config` and the matching [`crate::dto::job::JobOutput`]
/// variant.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct ConfigResolution {
    /// Modules discovered in the flake.
    pub available_modules: Vec<ModuleSpec>,
    /// Whether the flake exposes a `nixpkgs` input that machines may follow.
    pub nixpkgs_input_present: bool,
}
