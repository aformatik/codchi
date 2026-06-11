//! The v1 router: every catalog route wired to its [`CodchiService`] method.
//!
//! [`build_router`] is the one place that enumerates the 28 endpoints. Each line
//! is the small, explicit per-route glue doc 06 calls "the residual glue":
//! Rust cannot introspect a trait method's argument list, so the mapping from a
//! route's typed `(Path, Query, Body)` to its service call is hand-written —
//! while everything mechanical (routing, extraction, serialization) is the
//! generic [`mount_json`]/[`mount_ndjson`]/[`mount_empty`] machinery. The
//! `router_covers_all_routes` test keeps this list honest against [`ROUTES`].
//!
//! [`CodchiService`]: codchi_api::CodchiService
//! [`ROUTES`]: codchi_api::ROUTES

use axum::Router;
use codchi_api::endpoints::*;

use crate::mount::{mount_empty, mount_json, mount_ndjson};
use crate::state::AppState;

/// Build the complete v1 router, ready to serve once given its [`AppState`].
pub fn build_router(state: AppState) -> Router {
    let router: Router<AppState> = Router::new();

    // ---- server lifecycle ----
    // Readiness is server-infrastructure state, not a service method (D7): the
    // service supplies the bulk of `ServerStatus` (store/schema/findings), but
    // the headline `lifecycle` is overlaid from the daemon's own
    // `LifecycleHandle` so the readiness endpoint reports the *real* state the
    // client polls during spawn (C5) and that C6 will drive off store bring-up.
    let router = mount_json::<ServerStatusEp, _, _>(router, |s, _p, _q, _b| async move {
        let mut status = s.service.server_status().await?;
        let infrastructure = s.infrastructure.snapshot();
        status.lifecycle = s.lifecycle.current();
        status.store = infrastructure.store;
        let infrastructure_summary = codchi_api::dto::FindingsSummary::of(&infrastructure.findings);
        status.findings_summary.critical += infrastructure_summary.critical;
        status.findings_summary.error += infrastructure_summary.error;
        status.findings_summary.warning += infrastructure_summary.warning;
        status.findings_summary.info += infrastructure_summary.info;
        status.startup_error = infrastructure.startup_error;
        Ok(status)
    });

    // ---- machines ----
    let router = mount_json::<ListMachinesEp, _, _>(router, |s, _p, _q, _b| async move {
        s.service.list_machines().await
    });
    let router = mount_json::<CreateMachineEp, _, _>(router, |s, _p, _q, body| async move {
        s.service.create_machine(body).await
    });
    let router = mount_json::<GetMachineEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.get_machine(&p.0).await
    });
    let router = mount_json::<DeleteMachineEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.delete_machine(&p.0).await
    });
    let router = mount_json::<CloneMachineEp, _, _>(router, |s, p, _q, body| async move {
        s.service.clone_machine(&p.0, body).await
    });

    // ---- modules / config / secrets ----
    let router = mount_empty::<SetModulesEp, _, _>(router, |s, p, _q, body| async move {
        s.service.set_modules(&p.0, body).await
    });
    let router = mount_json::<ListSecretsEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.list_secrets(&p.0).await
    });
    let router = mount_json::<GetSecretEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.get_secret(&p.0, p.1).await
    });
    let router = mount_empty::<SetSecretEp, _, _>(router, |s, p, _q, body| async move {
        s.service.set_secret(&p.0, p.1, body.value).await
    });
    let router = mount_empty::<DeleteSecretEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.delete_secret(&p.0, p.1).await
    });

    // ---- build / update / activation ----
    let router = mount_json::<RebuildEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.rebuild(&p.0).await
    });
    let router = mount_json::<UpdateEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.update(&p.0).await
    });
    let router = mount_json::<ListGenerationsEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.list_generations(&p.0).await
    });
    let router = mount_json::<ActivateGenerationEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.activate_generation(&p.0, p.1).await
    });

    // ---- store generations ----
    let router = mount_json::<ListStoreGenerationsEp, _, _>(router, |s, _p, _q, _b| async move {
        s.service.list_store_generations().await
    });

    // ---- config resolution (R3) ----
    let router = mount_json::<ResolveConfigEp, _, _>(router, |s, _p, _q, body| async move {
        s.service.resolve_config(body).await
    });

    // ---- exec (R7) ----
    let router = mount_json::<PrepareExecEp, _, _>(router, |s, p, _q, body| async move {
        s.service.prepare_exec(&p.0, body).await
    });

    // ---- jobs ----
    let router = mount_json::<ListJobsEp, _, _>(router, |s, _p, query, _b| async move {
        s.service.list_jobs(query).await
    });
    let router = mount_json::<GetJobEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.get_job(&p.0).await
    });
    let router = mount_empty::<CancelJobEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.cancel_job(&p.0).await
    });
    let router = mount_ndjson::<StreamJobEventsEp, _, _>(router, |s, p, query, _b| async move {
        s.service.stream_job_events(&p.0, query).await
    });

    // ---- logs (R11) ----
    // `Server`/`Store` are real server-owned sources (C7); `Machine` is still
    // mock data on the service until Phase 7. The source decides the backend.
    let router = mount_ndjson::<StreamLogsEp, _, _>(router, |s, p, query, _b| async move {
        if matches!(p.0, codchi_api::LogSource::Machine(_)) {
            s.service.stream_logs(p.0, query).await
        } else {
            s.logs.stream(p.0, query)
        }
    });

    // ---- doctor ----
    let router = mount_json::<DoctorEp, _, _>(router, |s, _p, query, _b| async move {
        let mut report = s.service.doctor(query).await?;
        report.findings.extend(s.infrastructure.snapshot().findings);
        report.generated_at = chrono::Utc::now();
        Ok(report)
    });
    let router = mount_json::<DoctorScanEp, _, _>(router, |s, _p, _q, body| async move {
        s.service.doctor_scan(body).await
    });
    let router = mount_json::<DoctorFixEp, _, _>(router, |s, p, _q, _b| async move {
        s.service.doctor_fix(p.0).await
    });

    // ---- migration ----
    let router = mount_json::<MigrationPlanEp, _, _>(router, |s, _p, _q, _b| async move {
        s.service.migration_plan().await
    });
    let router = mount_json::<MigrationRunEp, _, _>(router, |s, _p, _q, body| async move {
        s.service.migration_run(body).await
    });

    router.with_state(state)
}
