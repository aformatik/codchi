//! Machines domain (SC3).
//!
//! Phase 4 lands the **pure** machine logic — module-list validation (MS8/MS14)
//! and the `ConfigurationStatus` projection (MS11) — and wires validation into
//! the request boundary. The read endpoints that assemble a full [`MachineView`]
//! still delegate to the mock: a view aggregates the active **generation**
//! (Phase 6), the reconciler snapshot (Phase 5/7), and findings (Phase 10), and
//! a durable machine row is *born at its first generation commit* (MS1, Phase 6),
//! so until then `list_machines` has no committed rows to render. The accessors
//! the Phase-6 write path will use live in [`crate::db::machines`].

use codchi_api::dto::{
    ConfigurationStatus, CreateMachineRequest, DuplicateMachineRequest, Duplicated, JobView,
    Lifecycle, MachineDetail, MachineView, ModuleSpec, RunStatus, SetModulesRequest,
};
use codchi_api::error::ApiError;
use codchi_api::ids::{GenerationId, MachineId};
use codchi_api::service::CodchiService;

use super::ServerCore;

/// Validate a submitted desired module list against the pure (non-eval, P6-safe)
/// invariants the service re-checks on every untrusted request (MS8/MS14): each
/// url is canonical-form and non-empty, no two entries share a canonical url, and
/// at most one entry is the nixpkgs source. Ordering needs no check — `position`
/// derives from list order (MS7). Attr-existence needs Nix eval and is *not*
/// checked here; a bogus attr fails the later rebuild job's eval, as in beta.
pub(crate) fn validate_modules(modules: &[ModuleSpec]) -> Result<(), ApiError> {
    let invalid = |message: &str| {
        Err(ApiError::Validation {
            field: "modules".to_owned(),
            message: message.to_owned(),
        })
    };

    let mut seen = std::collections::HashSet::new();
    let mut nixpkgs_sources = 0usize;
    for m in modules {
        validate_module_url(&m.url)?;
        if !seen.insert(m.url.as_str()) {
            return invalid(&format!("duplicate module url '{}'", m.url));
        }
        if m.is_nixpkgs_source {
            nixpkgs_sources += 1;
        }
    }
    if nixpkgs_sources > 1 {
        return invalid("at most one module may be the nixpkgs source");
    }
    Ok(())
}

/// Boundary check that a module url is in canonical form (MS14: the service
/// *rejects* non-canonical urls rather than canonicalizing — canonicalization is
/// `resolve_config`'s authority).
///
/// The full canonical Nix-flake-ref format and its normalizer are locked with
/// the Phase-6 generated-`flake.nix` work (`v1/todo/flake-url-canonical-form.md`),
/// so this is intentionally a minimal structural guard for now: non-empty, has a
/// `scheme://` (or `scheme:` shorthand) separator, and carries no embedded
/// auth (auth is out-of-band — never in the durable url, MS8/MS14).
fn validate_module_url(url: &str) -> Result<(), ApiError> {
    let invalid = |message: String| {
        Err(ApiError::Validation {
            field: "modules".to_owned(),
            message,
        })
    };
    if url.is_empty() {
        return invalid("module url must not be empty".to_owned());
    }
    let scheme_end = url
        .find("://")
        .map(|i| i + 3)
        .or_else(|| url.find(':').map(|i| i + 1));
    match scheme_end {
        Some(i) if i < url.len() => {}
        _ => return invalid(format!("module url '{url}' is missing a scheme")),
    }
    // userinfo@ before the path/query would mean embedded auth — forbidden (MS8).
    let authority = &url[scheme_end.unwrap()..];
    let authority = authority.split(['/', '?', '#']).next().unwrap_or(authority);
    if authority.contains('@') {
        return invalid(format!(
            "module url '{url}' must not embed credentials; configure auth out-of-band"
        ));
    }
    // `ModuleSpec.url` is the *complete* canonical url including its module
    // attribute path (MS8) — a non-empty `#<attr>` selector is required here
    // (unlike a bare `resolve_config` input, which may omit it). The full
    // canonical-form normalizer is Phase 6; this is the structural floor.
    match url.split_once('#') {
        Some((_, attr)) if !attr.is_empty() => {}
        _ => {
            return invalid(format!(
                "module url '{url}' must include a '#<module attribute>' selector"
            ));
        }
    }
    Ok(())
}

/// Derive `ConfigurationStatus` (MS11) — a pure desired-versus-active projection,
/// never stored. `active_snapshot` is the active generation's immutable
/// configuration snapshot, or `None` for the synthesized in-flight-create view
/// (no active generation). The snapshot lives in the Phase-6 generations table;
/// this function takes it as input so the rule is testable in isolation.
#[allow(dead_code)] // Wired into the live machine view in Phase 6.
pub(crate) fn configuration_status(
    active_snapshot: Option<&[ModuleSpec]>,
    desired: &[ModuleSpec],
) -> ConfigurationStatus {
    match active_snapshot {
        None => ConfigurationStatus::Unbuilt,
        Some(snapshot) if snapshot == desired => ConfigurationStatus::Applied,
        Some(_) => ConfigurationStatus::NeedsRebuild,
    }
}

/// Derive the display [`Lifecycle`] rollup (R10) from the two orthogonal axes:
/// the active-generation pointer (`None` ⇒ synthesized in-flight create, MS1) and
/// the raw container observation `run_status` (`None` ⇒ no reconciler report yet,
/// MS2). Pure and total; the server computes it once per view so clients render a
/// consistent label without re-deriving. `run_status` stays the raw platform
/// axis on the wire alongside this rollup.
#[allow(dead_code)] // Wired into the live machine view in Phase 6 (+ reconciler, Phase 5/7).
pub(crate) fn lifecycle(
    active_generation: Option<GenerationId>,
    run_status: Option<RunStatus>,
) -> Lifecycle {
    match (active_generation, run_status) {
        (None, _) => Lifecycle::Creating,
        (Some(_), None) => Lifecycle::Reconciling,
        (Some(_), Some(RunStatus::Absent)) => Lifecycle::Absent,
        (Some(_), Some(RunStatus::Stopped)) => Lifecycle::Stopped,
        (Some(_), Some(RunStatus::Running)) => Lifecycle::Running,
    }
}

impl ServerCore {
    /// Reject a create/duplicate whose id collides with a committed machine —
    /// exact or ASCII case-insensitive (MS6/P4) — with a typed `Validation` on
    /// the id field. This is the user-facing boundary check; the `machines_id_nocase`
    /// unique index is only a backstop behind it. The *other* half of the id
    /// namespace — a terminal-failed create job that still holds retained
    /// artifacts (MS1/R9) → `CreateArtifactsRetained` — needs the job table and
    /// is checked in Phase 5. Skipped when the server has no DB (degraded/mock
    /// mode), where the create cannot proceed to commit anyway.
    async fn reject_if_id_taken(&self, id: &MachineId, field: &str) -> Result<(), ApiError> {
        let Some(db) = &self.db else { return Ok(()) };
        let probe = id.clone();
        let taken = db
            .read(move |c| crate::db::machines::nocase_collision_exists(c, &probe, None))
            .await?;
        if taken {
            return Err(ApiError::Validation {
                field: field.to_owned(),
                message: format!(
                    "a machine named '{id}' already exists (names are case-insensitive)"
                ),
            });
        }
        Ok(())
    }

    pub(crate) async fn list_machines(&self) -> Result<Vec<MachineView>, ApiError> {
        self.mock.list_machines().await
    }

    pub(crate) async fn get_machine(&self, id: &MachineId) -> Result<MachineDetail, ApiError> {
        self.mock.get_machine(id).await
    }

    pub(crate) async fn create_machine(
        &self,
        req: CreateMachineRequest,
    ) -> Result<JobView<()>, ApiError> {
        // Server-side validation of the untrusted request (MS14): id syntax, the
        // module list, then the committed-row NOCASE collision (MS6). The
        // retained failed-create-job half of the namespace (MS1/R9) and the
        // actual create job are Phase 5.
        req.id.validate("id")?;
        validate_modules(&req.modules)?;
        self.reject_if_id_taken(&req.id, "id").await?;
        self.mock.create_machine(req).await
    }

    pub(crate) async fn duplicate_machine(
        &self,
        source: &MachineId,
        req: DuplicateMachineRequest,
    ) -> Result<JobView<Duplicated>, ApiError> {
        // The new machine's id is validated and collision-checked like a create
        // (MS6/MS13); the state copy + filesystem copy are Phase 6/7.
        source.validate("id")?;
        req.target.validate("target")?;
        self.reject_if_id_taken(&req.target, "target").await?;
        self.mock.duplicate_machine(source, req).await
    }

    pub(crate) async fn delete_machine(&self, id: &MachineId) -> Result<JobView<()>, ApiError> {
        self.mock.delete_machine(id).await
    }

    pub(crate) async fn set_modules(
        &self,
        id: &MachineId,
        req: SetModulesRequest,
    ) -> Result<(), ApiError> {
        // Whole-list replace is re-validated server-side (MS7/MS14); the durable
        // `replace_modules` write (and the machine-existence check) is Phase 6.
        id.validate("id")?;
        validate_modules(&req.modules)?;
        self.mock.set_modules(id, req).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn m(url: &str, nixpkgs: bool) -> ModuleSpec {
        ModuleSpec {
            url: url.to_owned(),
            is_nixpkgs_source: nixpkgs,
        }
    }

    #[test]
    fn validate_modules_accepts_well_formed_list() {
        let ok = vec![
            m("github:aformatik/codchi#nixosModules.base", true),
            m("git+https://example.org/repo#nixosModules.dev", false),
        ];
        assert!(validate_modules(&ok).is_ok());
        assert!(
            validate_modules(&[]).is_ok(),
            "zero modules is valid (default nixpkgs)"
        );
    }

    #[test]
    fn validate_modules_rejects_duplicate_url() {
        let dup = vec![m("github:a/b#x", false), m("github:a/b#x", false)];
        assert!(matches!(
            validate_modules(&dup),
            Err(ApiError::Validation { .. })
        ));
    }

    #[test]
    fn validate_modules_rejects_multiple_nixpkgs_sources() {
        let two = vec![m("github:a/b#x", true), m("github:c/d#y", true)];
        assert!(matches!(
            validate_modules(&two),
            Err(ApiError::Validation { .. })
        ));
    }

    #[test]
    fn validate_modules_rejects_empty_or_schemeless_or_credentialed_url() {
        assert!(validate_modules(&[m("", false)]).is_err());
        assert!(validate_modules(&[m("aformatik/codchi", false)]).is_err());
        assert!(
            validate_modules(&[m("git+https://user:tok@example.org/repo#m", false)]).is_err(),
            "embedded credentials are forbidden (MS8 auth out-of-band)"
        );
    }

    #[test]
    fn validate_modules_requires_module_attribute_selector() {
        // `ModuleSpec.url` includes the `#attr` selector (MS8) — a bare flake ref
        // without it is rejected.
        assert!(
            validate_modules(&[m("github:a/b", false)]).is_err(),
            "missing '#attr' selector must be rejected"
        );
        assert!(
            validate_modules(&[m("github:a/b#", false)]).is_err(),
            "empty attr too"
        );
        assert!(validate_modules(&[m("github:a/b#nixosModules.x", false)]).is_ok());
    }

    #[test]
    fn configuration_status_projection() {
        let desired = vec![m("github:a/b#x", false)];
        // No active generation ⇒ Unbuilt (synthesized create view only).
        assert_eq!(
            configuration_status(None, &desired),
            ConfigurationStatus::Unbuilt
        );
        // Snapshot equals desired ⇒ Applied.
        assert_eq!(
            configuration_status(Some(&desired), &desired),
            ConfigurationStatus::Applied
        );
        // Any difference (order, url, nixpkgs flag) ⇒ NeedsRebuild.
        let reordered = vec![m("github:a/b#x", true)];
        assert_eq!(
            configuration_status(Some(&reordered), &desired),
            ConfigurationStatus::NeedsRebuild
        );
        let different = vec![m("github:a/b#x", false), m("github:c/d#y", false)];
        assert_eq!(
            configuration_status(Some(&desired), &different),
            ConfigurationStatus::NeedsRebuild
        );
    }

    #[test]
    fn lifecycle_rollup() {
        let born = Some(GenerationId(1));
        // No active generation ⇒ Creating, regardless of run_status.
        assert_eq!(lifecycle(None, None), Lifecycle::Creating);
        assert_eq!(
            lifecycle(None, Some(RunStatus::Running)),
            Lifecycle::Creating
        );
        // Born machine, no observation yet ⇒ Reconciling.
        assert_eq!(lifecycle(born, None), Lifecycle::Reconciling);
        // Container observations map straight through.
        assert_eq!(lifecycle(born, Some(RunStatus::Absent)), Lifecycle::Absent);
        assert_eq!(
            lifecycle(born, Some(RunStatus::Stopped)),
            Lifecycle::Stopped
        );
        assert_eq!(
            lifecycle(born, Some(RunStatus::Running)),
            Lifecycle::Running
        );
    }

    /// MS6/P4: `create_machine` rejects a committed-row NOCASE collision at the
    /// boundary with a typed `Validation` on the id field — `Foo` blocks `foo`.
    #[tokio::test]
    async fn create_machine_rejects_nocase_collision_at_boundary() {
        use crate::core::{SchemaState, ServerCore, StoreCondition};
        use crate::db::{Db, MAX_SCHEMA_VERSION, machines::fixtures::insert_machine};
        use crate::logs::LogStore;
        use tokio::sync::watch;
        use tokio_util::sync::CancellationToken;

        let db = Db::open_memory().await.unwrap();
        db.migrate().await.unwrap();
        db.transaction(|tx| insert_machine(tx, "Foo", 1))
            .await
            .unwrap();

        let tmp = tempfile::tempdir().unwrap();
        let (tx, rx) = watch::channel(StoreCondition::Starting);
        std::mem::forget(tx); // keep the sender alive for the receiver's lifetime
        let core = ServerCore::new(
            LogStore::new(tmp.path()),
            rx,
            CancellationToken::new(),
            Some(db),
            SchemaState::Ready,
            MAX_SCHEMA_VERSION,
        );

        let err = core
            .create_machine(CreateMachineRequest {
                id: MachineId("foo".to_owned()),
                modules: vec![],
                keep_on_fail: false,
            })
            .await
            .expect_err("case-variant of a committed machine must be rejected");
        assert!(
            matches!(err, ApiError::Validation { ref field, .. } if field == "id"),
            "expected Validation on id, got {err:?}"
        );
    }
}
