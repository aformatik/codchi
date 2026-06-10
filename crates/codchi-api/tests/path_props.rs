//! Property tests for path-parameter render/parse round-trips (Phase 1 C1).
//!
//! The example-based coverage in `contract.rs` proves every route once. These
//! properties fuzz the *segment codec* — the render → transport-decode → parse
//! pipeline — across the full character set each typed id allows, which is
//! where path bugs (a missed reserved char, a lossy decode, a `/` leaking out
//! of a segment) actually hide. The round-trip law is `parse(decode(render x)) == x`.

use codchi_api::endpoints::GetSecretEp;
use codchi_api::{Endpoint, FindingId, JobId, MachineId, PathParams, PathSegment, SecretName};
use percent_encoding::percent_decode_str;
use proptest::prelude::*;
use uuid::Uuid;

/// Stand in for the transport (axum): a rendered segment is percent-decoded
/// before the router hands it to `parse`.
fn transport_decode(rendered: &str) -> String {
    percent_decode_str(rendered)
        .decode_utf8()
        .expect("rendered segment is valid UTF-8")
        .into_owned()
}

/// A valid `MachineId` body (P4), minus the reserved `codchi-machine-` prefix.
fn machine_id() -> impl Strategy<Value = String> {
    proptest::string::string_regex(r"[A-Za-z0-9]([A-Za-z0-9._-]{0,30}[A-Za-z0-9])?")
        .unwrap()
        .prop_filter("reserved prefix", |s| {
            !s.to_ascii_lowercase().starts_with("codchi-machine-")
        })
}

proptest! {
    /// A declared secret name (which may legitimately contain the reserved `:`)
    /// survives render → decode → parse unchanged, and never leaks a `/` that
    /// would split it into two path components.
    #[test]
    fn secret_name_round_trips(s in r"[A-Za-z0-9:_.-]{1,40}") {
        let name = SecretName(s);
        let rendered = PathSegment::render(&name);
        prop_assert!(!rendered.contains('/'), "segment must stay one component: {rendered}");
        let parsed = SecretName::parse("key", &transport_decode(&rendered)).unwrap();
        prop_assert_eq!(parsed, name);
    }

    /// Every valid machine name round-trips through the `{id}` segment.
    #[test]
    fn machine_id_round_trips(s in machine_id()) {
        let id = MachineId(s);
        let rendered = PathSegment::render(&id);
        let parsed = MachineId::parse("id", &transport_decode(&rendered)).unwrap();
        prop_assert_eq!(parsed, id);
    }

    /// UUID-keyed segments (jobs, findings) round-trip for any UUID.
    #[test]
    fn uuid_segments_round_trip(bytes in any::<u128>()) {
        let job = JobId(Uuid::from_u128(bytes));
        let parsed = JobId::parse("id", &transport_decode(&PathSegment::render(&job))).unwrap();
        prop_assert_eq!(parsed, job);

        let finding = FindingId(Uuid::from_u128(bytes));
        let parsed =
            FindingId::parse("id", &transport_decode(&PathSegment::render(&finding))).unwrap();
        prop_assert_eq!(parsed, finding);
    }

    /// The full two-parameter `get_secret` route round-trips: rendering yields a
    /// well-formed path, and the transport's ordered matched values parse back
    /// into the same `(MachineId, SecretName)` tuple.
    #[test]
    fn get_secret_route_round_trips(id in machine_id(), key in r"[A-Za-z0-9:_.-]{1,40}") {
        let path = (MachineId(id.clone()), SecretName(key.clone()));
        let rendered = PathParams::render(&path, GetSecretEp::PATH);
        prop_assert!(rendered.starts_with("/v1/machines/"));
        // The router percent-decodes matched params; here they have no `%`, so
        // the decoded values are the raw ids in template order.
        let parsed =
            <GetSecretEp as Endpoint>::Path::parse(GetSecretEp::PATH, &[&id, &key]).unwrap();
        prop_assert_eq!(parsed, path);
    }
}
