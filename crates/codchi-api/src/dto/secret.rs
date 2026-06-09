//! Secret DTOs (Q3, R4, R5).
//!
//! Secrets are NixOS-declared values: each key is declared by the machine's
//! `codchi.secrets.env` config with a `description`, cached into SQLite during
//! build/eval. Keys are listable; values are readable. All declared secrets are
//! required and enforced at machine *start* (R5).

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// A declared secret key. Values are never carried on this type.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct SecretKey {
    /// Declared key name (matches the `codchi.secrets.env` attribute).
    pub name: String,
    /// Human-readable description from the NixOS declaration.
    pub description: String,
    /// Whether a value is currently stored for this key.
    pub has_value: bool,
}

/// Body for `set_secret` — the plaintext value (the key is in the path).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct SetSecretRequest {
    /// Plaintext secret value. Never logged; never placed on event streams.
    pub value: String,
}
