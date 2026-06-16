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
//! Per SC1 every handler is now a one-line dispatch into [`AppState::core`]: the
//! Phase-1 `ServerStatusEp`/`StreamLogsEp`/`DoctorEp` overlays moved *into*
//! [`ServerCore`](crate::core::ServerCore), so the router holds no lifecycle,
//! source-branch, or findings logic.
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
    let router = mount_json::<ServerStatusEp, _, _>(router, |s, _p, _q, _b| async move {
        s.core.server_status().await
    });

    // ---- machines ----
    let router = mount_json::<ListMachinesEp, _, _>(router, |s, _p, _q, _b| async move {
        s.core.list_machines().await
    });
    let router = mount_json::<CreateMachineEp, _, _>(router, |s, _p, _q, body| async move {
        s.core.create_machine(body).await
    });
    let router = mount_json::<GetMachineEp, _, _>(router, |s, p, _q, _b| async move {
        s.core.get_machine(&p.0).await
    });
    let router = mount_json::<DeleteMachineEp, _, _>(router, |s, p, _q, _b| async move {
        s.core.delete_machine(&p.0).await
    });
    let router = mount_json::<DuplicateMachineEp, _, _>(router, |s, p, _q, body| async move {
        s.core.duplicate_machine(&p.0, body).await
    });

    // ---- modules / config / secrets ----
    let router = mount_empty::<SetModulesEp, _, _>(router, |s, p, _q, body| async move {
        s.core.set_modules(&p.0, body).await
    });
    let router = mount_json::<ListSecretsEp, _, _>(router, |s, p, _q, _b| async move {
        s.core.list_secrets(&p.0).await
    });
    let router = mount_json::<GetSecretEp, _, _>(router, |s, p, _q, _b| async move {
        s.core.get_secret(&p.0, p.1).await
    });
    let router = mount_empty::<SetSecretEp, _, _>(router, |s, p, _q, body| async move {
        s.core.set_secret(&p.0, p.1, body.value).await
    });
    let router = mount_empty::<DeleteSecretEp, _, _>(router, |s, p, _q, _b| async move {
        s.core.delete_secret(&p.0, p.1).await
    });

    // ---- build / update / activation ----
    let router =
        mount_json::<RebuildEp, _, _>(
            router,
            |s, p, _q, _b| async move { s.core.rebuild(&p.0).await },
        );
    let router =
        mount_json::<UpdateEp, _, _>(
            router,
            |s, p, _q, _b| async move { s.core.update(&p.0).await },
        );
    let router = mount_json::<ListGenerationsEp, _, _>(router, |s, p, _q, _b| async move {
        s.core.list_generations(&p.0).await
    });
    let router = mount_json::<ActivateGenerationEp, _, _>(router, |s, p, _q, _b| async move {
        s.core.activate_generation(&p.0, p.1).await
    });

    // ---- store generations ----
    let router = mount_json::<ListStoreGenerationsEp, _, _>(router, |s, _p, _q, _b| async move {
        s.core.list_store_generations().await
    });

    // ---- config resolution (R3) ----
    let router = mount_json::<ResolveConfigEp, _, _>(router, |s, _p, _q, body| async move {
        s.core.resolve_config(body).await
    });

    // ---- exec (R7) ----
    let router = mount_json::<PrepareExecEp, _, _>(router, |s, p, _q, body| async move {
        s.core.prepare_exec(&p.0, body).await
    });

    // ---- jobs ----
    let router = mount_json::<ListJobsEp, _, _>(router, |s, _p, query, _b| async move {
        s.core.list_jobs(query).await
    });
    let router =
        mount_json::<GetJobEp, _, _>(
            router,
            |s, p, _q, _b| async move { s.core.get_job(&p.0).await },
        );
    let router = mount_empty::<CancelJobEp, _, _>(router, |s, p, _q, _b| async move {
        s.core.cancel_job(&p.0).await
    });
    let router = mount_ndjson::<StreamJobEventsEp, _, _>(router, |s, p, query, _b| async move {
        s.core.stream_job_events(&p.0, query).await
    });

    // ---- logs (R11) ----
    let router = mount_ndjson::<StreamLogsEp, _, _>(router, |s, p, query, _b| async move {
        s.core.stream_logs(p.0, query).await
    });

    // ---- doctor ----
    let router = mount_json::<DoctorEp, _, _>(router, |s, _p, query, _b| async move {
        s.core.doctor(query).await
    });
    let router = mount_json::<DoctorScanEp, _, _>(router, |s, _p, _q, body| async move {
        s.core.doctor_scan(body).await
    });
    let router = mount_json::<DoctorFixEp, _, _>(router, |s, p, _q, _b| async move {
        s.core.doctor_fix(p.0).await
    });

    // ---- migration ----
    let router = mount_json::<MigrationPlanEp, _, _>(router, |s, _p, _q, _b| async move {
        s.core.migration_plan().await
    });
    let router = mount_json::<MigrationRunEp, _, _>(router, |s, _p, _q, body| async move {
        s.core.migration_run(body).await
    });

    router.with_state(state)
}
