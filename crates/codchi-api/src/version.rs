//! API version constant (Q5).
//!
//! A single `API_VERSION` is the authoritative major version. Every route is
//! served under the `/v1/` prefix; breaking changes bump this major and move
//! routes to `/v2/`. Additive changes (new endpoints, new optional fields)
//! stay under the current major.

/// Current major API version. Exposed in [`crate::dto::ServerStatus`] and as an
/// HTTP response header on every call.
pub const API_VERSION: u32 = 1;

/// URL prefix carrying [`API_VERSION`]. Kept in sync by hand; the value is
/// `/v1` for `API_VERSION == 1`.
pub const API_PREFIX: &str = "/v1";
