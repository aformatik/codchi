//! Secrets domain (SC3).
//!
//! Phase 4 lands the **pure** secret logic (MS10): the `list_secrets` union over
//! {active-generation declared keys} ∪ {keys with a stored value} and its four
//! `(declared?, has_value?)` states, plus the get/set/delete classifiers. The
//! durable secret *values* live in `secret_values` ([`crate::db::machines`]); the
//! *declared schema* (key names + descriptions) is the active generation's, which
//! is Phase 6 — so the live endpoints stay on the mock until a generation can
//! supply that schema. These functions encode the whole MS10 matrix and carry
//! `#[allow(dead_code)]` only until that Phase-6 wiring lands.

use codchi_api::dto::{SecretKey, SecretName, SecretStatus};
use codchi_api::error::ApiError;
use codchi_api::ids::MachineId;
use codchi_api::service::CodchiService;

use super::ServerCore;
use crate::db::machines::StoredSecret;

/// One key declared by the active generation's secret schema (MS10) — the
/// declaration source (name + live description) for `Declared` entries.
#[allow(dead_code)] // Phase 6 supplies these from the active generation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct DeclaredSecret {
    pub name: SecretName,
    pub description: String,
}

/// Derive `list_secrets` (MS10): the union of the active generation's declared
/// keys and the machine's stored values, each classified by `(declared?,
/// has_value?)`. `(no, no)` cannot occur (such a key is in neither input).
/// Descriptions come from the declaration for `Declared` keys and from the value
/// row's stored `description` (last-declared, frozen at obsolescence) for
/// `Obsolete` ones. Ordered by key.
#[allow(dead_code)] // Wired into the live `list_secrets` in Phase 6.
pub(crate) fn secret_list(declared: &[DeclaredSecret], stored: &[StoredSecret]) -> Vec<SecretKey> {
    use std::collections::BTreeMap;

    // Keyed by name so the union is deterministic (BTreeMap = sorted by key).
    let mut out: BTreeMap<&str, SecretKey> = BTreeMap::new();

    for d in declared {
        out.insert(
            d.name.as_str(),
            SecretKey {
                name: d.name.clone(),
                description: d.description.clone(),
                has_value: false,
                status: SecretStatus::Declared,
            },
        );
    }
    for s in stored {
        match out.get_mut(s.key.as_str()) {
            // Declared key with a stored value: keep the declaration's
            // description, just flip `has_value`.
            Some(entry) => entry.has_value = true,
            // Stored value with no declaration: obsolete, described by its
            // last-declared description (frozen on the value row).
            None => {
                out.insert(
                    s.key.as_str(),
                    SecretKey {
                        name: SecretName(s.key.clone()),
                        description: s.description.clone(),
                        has_value: true,
                        status: SecretStatus::Obsolete,
                    },
                );
            }
        }
    }
    out.into_values().collect()
}

/// Outcome of `get_secret` against the MS10 matrix.
#[allow(dead_code)] // Wired into the live `get_secret` in Phase 6.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum GetOutcome {
    /// Declared+value, or obsolete+value — return the plaintext.
    Value(String),
    /// Declared but unset → `SecretNotSet`.
    Unset,
    /// Neither declared nor stored → `Validation` (unknown key).
    Unknown,
}

/// Classify `get_secret` (MS10): any stored value is returned (a user may
/// inspect an obsolete value before deleting); a declared-but-unset key is
/// `Unset`; anything else is `Unknown`.
#[allow(dead_code)] // Wired into the live `get_secret` in Phase 6.
pub(crate) fn classify_get(declared: bool, stored: Option<&StoredSecret>) -> GetOutcome {
    match (declared, stored) {
        (_, Some(s)) => GetOutcome::Value(s.plaintext.clone()),
        (true, None) => GetOutcome::Unset,
        (false, None) => GetOutcome::Unknown,
    }
}

/// Whether `set_secret` is accepted (MS10): only declared keys. Obsolete and
/// unknown keys are both rejected with `Validation` — values cannot be created
/// or changed outside the committed declaration schema.
#[allow(dead_code)] // Wired into the live `set_secret` in Phase 6.
pub(crate) fn set_accepted(declared: bool) -> bool {
    declared
}

/// Whether `delete_secret` is accepted (MS10): a declared key (removes the value,
/// the declaration survives) or an obsolete value (removes the dormant row).
/// Only a truly unknown key (neither declared nor stored) is rejected.
#[allow(dead_code)] // Wired into the live `delete_secret` in Phase 6.
pub(crate) fn delete_accepted(declared: bool, has_value: bool) -> bool {
    declared || has_value
}

impl ServerCore {
    pub(crate) async fn set_secret(
        &self,
        id: &MachineId,
        key: SecretName,
        value: String,
    ) -> Result<(), ApiError> {
        self.mock.set_secret(id, key, value).await
    }

    pub(crate) async fn get_secret(
        &self,
        id: &MachineId,
        key: SecretName,
    ) -> Result<String, ApiError> {
        self.mock.get_secret(id, key).await
    }

    pub(crate) async fn list_secrets(&self, id: &MachineId) -> Result<Vec<SecretKey>, ApiError> {
        self.mock.list_secrets(id).await
    }

    pub(crate) async fn delete_secret(
        &self,
        id: &MachineId,
        key: SecretName,
    ) -> Result<(), ApiError> {
        self.mock.delete_secret(id, key).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn declared(name: &str, desc: &str) -> DeclaredSecret {
        DeclaredSecret {
            name: SecretName(name.to_owned()),
            description: desc.to_owned(),
        }
    }
    fn stored(key: &str, value: &str, description: &str) -> StoredSecret {
        StoredSecret {
            key: key.to_owned(),
            plaintext: value.to_owned(),
            description: description.to_owned(),
        }
    }

    /// MS10: the four `(declared?, has_value?)` states, deterministically ordered.
    #[test]
    fn secret_list_derives_four_states() {
        let decl = vec![declared("A_SET", "a"), declared("B_UNSET", "b")];
        let vals = vec![
            stored("A_SET", "v", "a"),                // declared + value
            stored("Z_OBSOLETE", "old", "gone desc"), // value, no declaration
        ];
        let list = secret_list(&decl, &vals);
        let by: Vec<_> = list
            .iter()
            .map(|s| {
                (
                    s.name.as_str(),
                    s.has_value,
                    s.status,
                    s.description.as_str(),
                )
            })
            .collect();
        assert_eq!(
            by,
            vec![
                ("A_SET", true, SecretStatus::Declared, "a"),
                ("B_UNSET", false, SecretStatus::Declared, "b"),
                ("Z_OBSOLETE", true, SecretStatus::Obsolete, "gone desc"),
            ]
        );
    }

    /// MS10 get matrix: obsolete & declared values return; unset → Unset;
    /// unknown → Unknown.
    #[test]
    fn classify_get_matrix() {
        let val = stored("K", "secret", "");
        assert_eq!(
            classify_get(true, Some(&val)),
            GetOutcome::Value("secret".into())
        );
        let obs = stored("K", "old", "d");
        assert_eq!(
            classify_get(false, Some(&obs)),
            GetOutcome::Value("old".into())
        );
        assert_eq!(classify_get(true, None), GetOutcome::Unset);
        assert_eq!(classify_get(false, None), GetOutcome::Unknown);
    }

    /// MS10 set/delete matrix.
    #[test]
    fn classify_set_and_delete_matrix() {
        // set: only declared keys.
        assert!(set_accepted(true));
        assert!(!set_accepted(false), "obsolete/unknown keys are rejected");

        // delete: declared (any) or obsolete value; unknown rejected.
        assert!(delete_accepted(true, true)); // declared + value
        assert!(delete_accepted(true, false)); // declared, unset (no-op delete)
        assert!(delete_accepted(false, true)); // obsolete value
        assert!(!delete_accepted(false, false)); // unknown
    }
}
