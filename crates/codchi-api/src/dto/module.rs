//! NixOS module references attached to a machine.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// A single module a machine pulls in. `url` is the codchi flake-url form
/// (`<scheme>://<host>/<repo>?<query>#<attr>`, see beta `FlakeUrl`); `name` is
/// the local handle used in CLI/config.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct ModuleSpec {
    /// Local handle for the module (the map key in beta `MachineConfig`).
    pub name: String,
    /// Codchi flake-url including the module attribute path.
    pub url: String,
    /// Whether this module supplies the machine's `nixpkgs` input
    /// (beta `nixpkgs_from`).
    #[serde(default)]
    pub is_nixpkgs_source: bool,
}
