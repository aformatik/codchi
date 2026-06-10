//! Secret DTOs (Q3, R4, R5).
//!
//! Secrets are NixOS-declared values: each key is declared by the machine's
//! `codchi.secrets.env` config with a `description`, cached into SQLite during
//! build/eval. Keys are listable; values are readable. All declared secrets are
//! required and enforced at machine *start* (R5).

use std::fmt;
use std::str::FromStr;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::ApiError;

/// A declared secret key name — the `codchi.secrets.env` attribute (R4, P3/P4).
///
/// Newtype around `String` so the key never appears raw in the contract and
/// carries its own path-segment validation. Like [`MachineId`](crate::MachineId)
/// validation is *not* enforced in `Deserialize`; use [`SecretName::new`] /
/// [`SecretName::validate`] so malformed input surfaces as a typed
/// [`ApiError::Validation`] at the boundary. `#[serde(transparent)]` keeps the
/// wire form (and OpenAPI) a plain string.
#[derive(
    Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
#[serde(transparent)]
pub struct SecretName(pub String);

impl SecretName {
    /// Construct and validate. `field` is the request field path used in the
    /// returned [`ApiError::Validation`] (e.g. `"key"`).
    pub fn new(value: impl Into<String>, field: impl Into<String>) -> Result<Self, ApiError> {
        let name = SecretName(value.into());
        name.validate(field)?;
        Ok(name)
    }

    /// The raw name as a `&str`.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Validate the syntactic shape, mirroring the NixOS `codchi.secrets.env`
    /// option (`strMatching "^[a-zA-Z0-9:_.-]*$"`) with a non-empty, length-
    /// bounded guard. This is a boundary check; the authoritative test is
    /// membership in the cached declared schema (R4), done by the server.
    pub fn validate(&self, field: impl Into<String>) -> Result<(), ApiError> {
        let field = field.into();
        let invalid = |message: &str| {
            Err(ApiError::Validation {
                field: field.clone(),
                message: message.to_owned(),
            })
        };

        let s = &self.0;
        if s.is_empty() {
            return invalid("secret name must not be empty");
        }
        if s.len() > 255 {
            return invalid("secret name must be at most 255 characters");
        }
        if !s
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b':' | b'_' | b'.' | b'-'))
        {
            return invalid("secret name may only contain letters, digits, ':', '_', '.' and '-'");
        }
        Ok(())
    }
}

impl fmt::Display for SecretName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl FromStr for SecretName {
    type Err = ApiError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        SecretName::new(s, "key")
    }
}

/// A declared secret key. Values are never carried on this type.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
pub struct SecretKey {
    /// Declared key name (matches the `codchi.secrets.env` attribute).
    pub name: SecretName,
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
