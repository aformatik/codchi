//! NixOS module references attached to a machine.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// A single module a machine pulls in (Phase 4 MS8). The complete canonical
/// Codchi flake URL — including its `#attr` module attribute path — is the
/// module's whole durable identity; modules have **no** user-visible name (beta's
/// mutable map-key handles are not domain identity). The canonical URL form and
/// its normalizer are tracked in `v1/todo/flake-url-canonical-form.md` and locked
/// with the Phase-6 generated-`flake.nix` normalizer.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct ModuleSpec {
    /// Canonical Codchi flake URL including the module attribute path. The whole
    /// durable identity of the module (MS8).
    pub url: String,
    /// Whether this module supplies the machine's `nixpkgs` input
    /// (beta `nixpkgs_from`). At most one module per machine may set this (MS8).
    #[serde(default)]
    pub is_nixpkgs_source: bool,
}
