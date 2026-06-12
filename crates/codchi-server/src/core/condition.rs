//! The store's observed runtime condition (SC5/SC6).
//!
//! [`StoreCondition`] is the single sum type that the server `lifecycle`, the
//! [`StoreStatus`], any startup error, and the `store.unavailable` finding are
//! **pure projections** of. Because the value is the only stored truth, illegal
//! combinations (an `Up` store with a startup error; a `Degraded` store with no
//! reason) are unrepresentable, and the projections cannot drift.
//!
//! The transition is a pure free function [`step`]: the store supervisor
//! (`crate::supervisor`) is its only caller and the sole owner of the value, so
//! read-decide-write happens in one place over an owned field — the look-alike
//! actor TOCTOU race is impossible (SC6).

use chrono::{DateTime, Utc};
use codchi_api::dto::{
    Component, Finding, FindingCode, ServerLifecycle, Severity, StoreState, StoreStatus,
};
use codchi_api::{ApiError, FindingId};
use uuid::Uuid;

/// The observed, ephemeral runtime state of the store container.
///
/// `Up`/`Degraded` carry `since` so the projected finding's `created_at` and the
/// `Up` timestamp are **stable across reads** (no regenerated timestamps) — the
/// proof that `since`-in-the-variant is correct (SC5).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StoreCondition {
    /// The supervisor is bringing the container up (create/start).
    Starting,
    /// The container is up; the supervisor is waiting on the nix-daemon probe.
    Checking,
    /// The store answered a health probe and is serving.
    Up { since: DateTime<Utc> },
    /// The store failed its probe; `reason` is the structured failure.
    Degraded {
        reason: String,
        since: DateTime<Utc>,
    },
}

/// The result of one health probe — the only input to [`step`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProbeOutcome {
    Healthy,
    Unhealthy { reason: String },
}

/// Advance the condition by one health-probe outcome (SC6).
///
/// Pure and total. Its codomain is `{Up, Degraded}` only: a probe outcome never
/// produces `Starting`/`Checking`, so the `Degraded → Checking/Starting` and
/// `Up → Checking/Starting` edges are **absent by construction** (those phases
/// are set directly by the supervisor's bring-up, never by a probe).
///
/// `since` is preserved when the kind of condition is unchanged (`Up → Up`,
/// `Degraded → Degraded`) so the projected timestamps/finding ids stay stable;
/// it is stamped to `now` only on a genuine transition into that kind.
pub fn step(current: &StoreCondition, outcome: ProbeOutcome, now: DateTime<Utc>) -> StoreCondition {
    match outcome {
        ProbeOutcome::Healthy => StoreCondition::Up {
            since: match current {
                StoreCondition::Up { since } => *since,
                _ => now,
            },
        },
        ProbeOutcome::Unhealthy { reason } => StoreCondition::Degraded {
            reason,
            since: match current {
                StoreCondition::Degraded { since, .. } => *since,
                _ => now,
            },
        },
    }
}

/// Project the server's headline lifecycle from the shutdown flag and the store
/// condition (SC8). `shutdown` is the second input that forces `Stopping`,
/// taking precedence over any store condition — the same multi-input shape
/// Phase 3 extends for `Migrating`.
pub fn lifecycle(shutdown: bool, store: &StoreCondition) -> ServerLifecycle {
    if shutdown {
        return ServerLifecycle::Stopping;
    }
    match store {
        StoreCondition::Starting => ServerLifecycle::Starting,
        StoreCondition::Checking => ServerLifecycle::Healthcheck,
        StoreCondition::Up { .. } => ServerLifecycle::Ready,
        StoreCondition::Degraded { .. } => ServerLifecycle::Degraded,
    }
}

/// Project the wire [`StoreStatus`] from the condition.
pub fn store_status(store: &StoreCondition) -> StoreStatus {
    match store {
        StoreCondition::Starting | StoreCondition::Checking => StoreStatus {
            state: StoreState::Recovering,
            last_checked_at: None,
            last_error: None,
        },
        StoreCondition::Up { since } => StoreStatus {
            state: StoreState::Up,
            last_checked_at: Some(*since),
            last_error: None,
        },
        StoreCondition::Degraded { reason, since } => StoreStatus {
            state: StoreState::Down,
            last_checked_at: Some(*since),
            last_error: Some(reason.clone()),
        },
    }
}

/// Project the startup error — `Some` only when the store is `Degraded` (the one
/// recoverable-failure lifecycle in Phase 2), so an `Up` store can never carry
/// one (SC5).
pub fn startup_error(store: &StoreCondition) -> Option<ApiError> {
    match store {
        StoreCondition::Degraded { reason, .. } => Some(ApiError::StoreUnavailable {
            reason: reason.clone(),
        }),
        _ => None,
    }
}

/// The deterministic id of the projected `store.unavailable` finding.
///
/// A fixed value (one code → one id) so the projection is *stable* across reads:
/// re-projecting the same `Degraded` condition yields a byte-identical finding,
/// never a fresh random id (SC5). When the persistent multi-source registry
/// arrives (SC-B, Phase 7/10) durable findings get minted ids instead.
const STORE_UNAVAILABLE_FINDING_ID: Uuid = Uuid::from_bytes([
    0xc0, 0xdc, 0x01, 0x00, 0x57, 0x70, 0x4e, 0xa1, 0x9b, 0x5e, 0x00, 0x57, 0x74, 0x6f, 0x72, 0x65,
]);

/// Project the active findings from the condition. In Phase 2 the only finding
/// is `store.unavailable`, emitted iff the store is `Degraded` (SC5). The
/// finding's `created_at = since` and its id is fixed, so the projection is pure
/// *and* stable.
pub fn store_findings(store: &StoreCondition) -> Vec<Finding> {
    match store {
        StoreCondition::Degraded { reason, since } => vec![Finding {
            id: FindingId(STORE_UNAVAILABLE_FINDING_ID),
            severity: Severity::Error,
            component: Component::Store,
            machine: None,
            source_job: None,
            code: FindingCode::StoreUnavailable,
            message: reason.clone(),
            suggested_action: Some("Run `codchi doctor` for details.".to_owned()),
            auto_fixable: false,
            created_at: *since,
        }],
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn t(secs: i64) -> DateTime<Utc> {
        DateTime::from_timestamp(secs, 0).unwrap()
    }

    // ---- step matrix ----

    #[test]
    fn healthy_from_a_non_up_state_stamps_since_now() {
        for current in [
            StoreCondition::Starting,
            StoreCondition::Checking,
            StoreCondition::Degraded {
                reason: "x".into(),
                since: t(1),
            },
        ] {
            let next = step(&current, ProbeOutcome::Healthy, t(99));
            assert_eq!(next, StoreCondition::Up { since: t(99) });
        }
    }

    #[test]
    fn healthy_from_up_preserves_since() {
        let current = StoreCondition::Up { since: t(5) };
        let next = step(&current, ProbeOutcome::Healthy, t(99));
        assert_eq!(next, StoreCondition::Up { since: t(5) });
    }

    #[test]
    fn unhealthy_from_a_non_degraded_state_stamps_since_now() {
        for current in [
            StoreCondition::Starting,
            StoreCondition::Checking,
            StoreCondition::Up { since: t(5) },
        ] {
            let next = step(
                &current,
                ProbeOutcome::Unhealthy {
                    reason: "boom".into(),
                },
                t(99),
            );
            assert_eq!(
                next,
                StoreCondition::Degraded {
                    reason: "boom".into(),
                    since: t(99)
                }
            );
        }
    }

    #[test]
    fn unhealthy_from_degraded_preserves_since_but_updates_reason() {
        let current = StoreCondition::Degraded {
            reason: "old".into(),
            since: t(5),
        };
        let next = step(
            &current,
            ProbeOutcome::Unhealthy {
                reason: "new".into(),
            },
            t(99),
        );
        assert_eq!(
            next,
            StoreCondition::Degraded {
                reason: "new".into(),
                since: t(5)
            }
        );
    }

    #[test]
    fn step_never_yields_starting_or_checking() {
        // The absent edges: no probe outcome from any state re-enters a bring-up
        // phase.
        for current in [
            StoreCondition::Starting,
            StoreCondition::Checking,
            StoreCondition::Up { since: t(5) },
            StoreCondition::Degraded {
                reason: "x".into(),
                since: t(5),
            },
        ] {
            for outcome in [
                ProbeOutcome::Healthy,
                ProbeOutcome::Unhealthy { reason: "x".into() },
            ] {
                let next = step(&current, outcome, t(99));
                assert!(matches!(
                    next,
                    StoreCondition::Up { .. } | StoreCondition::Degraded { .. }
                ));
            }
        }
    }

    // ---- projections ----

    #[test]
    fn lifecycle_projection_covers_every_condition() {
        assert_eq!(
            lifecycle(false, &StoreCondition::Starting),
            ServerLifecycle::Starting
        );
        assert_eq!(
            lifecycle(false, &StoreCondition::Checking),
            ServerLifecycle::Healthcheck
        );
        assert_eq!(
            lifecycle(false, &StoreCondition::Up { since: t(1) }),
            ServerLifecycle::Ready
        );
        assert_eq!(
            lifecycle(
                false,
                &StoreCondition::Degraded {
                    reason: "x".into(),
                    since: t(1)
                }
            ),
            ServerLifecycle::Degraded
        );
    }

    #[test]
    fn shutdown_forces_stopping_over_any_condition() {
        for store in [
            StoreCondition::Starting,
            StoreCondition::Up { since: t(1) },
            StoreCondition::Degraded {
                reason: "x".into(),
                since: t(1),
            },
        ] {
            assert_eq!(lifecycle(true, &store), ServerLifecycle::Stopping);
        }
    }

    #[test]
    fn up_can_never_carry_a_startup_error() {
        assert_eq!(startup_error(&StoreCondition::Up { since: t(1) }), None);
        assert!(store_findings(&StoreCondition::Up { since: t(1) }).is_empty());
    }

    #[test]
    fn degraded_projects_a_stable_finding_anchored_at_since() {
        let store = StoreCondition::Degraded {
            reason: "nix daemon did not answer".into(),
            since: t(1234),
        };
        let first = store_findings(&store);
        let second = store_findings(&store);
        assert_eq!(first, second, "projection is stable across reads");
        assert_eq!(first.len(), 1);
        let finding = &first[0];
        assert_eq!(finding.code, FindingCode::StoreUnavailable);
        assert_eq!(finding.message, "nix daemon did not answer");
        assert_eq!(finding.created_at, t(1234), "created_at == since");
        assert_eq!(
            finding.id,
            FindingId(STORE_UNAVAILABLE_FINDING_ID),
            "id is deterministic, not freshly minted"
        );

        // The startup error mirrors the finding's reason.
        assert!(matches!(
            startup_error(&store),
            Some(ApiError::StoreUnavailable { reason }) if reason == "nix daemon did not answer"
        ));
    }
}
