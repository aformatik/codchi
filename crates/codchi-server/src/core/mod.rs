//! [`ServerCore`] — the single `impl CodchiService` (SC1–SC3).
//!
//! `ServerCore` owns the server-owned state the Phase-1 router used to assemble
//! by hand: the store-condition reader (`watch::Receiver<StoreCondition>`), the
//! shutdown flag, and the [`LogStore`]. The `ServerStatusEp` overlay and the
//! `StreamLogsEp` source-branch that used to live in the router now live here, so
//! every handler is a one-line dispatch into the matching trait method (SC1).
//!
//! ## Structure (SC3)
//!
//! Rust allows only one `impl CodchiService for ServerCore`, so the block below
//! is a **thin dispatcher**: each method forwards to an inherent `ServerCore`
//! method grouped into a domain module (`machines`, `jobs`, `store`, …).
//! Inherent-method resolution means `self.foo()` here calls the inherent `foo`,
//! never the trait method, so there is no recursion. For Phase 2 most inherent
//! bodies are `self.mock.<x>()` — a single quarantined [`MockCodchiService`]
//! field backs every still-unbacked domain (SC2), removed domain-by-domain as
//! Phases 4/5/6/7 land. The already-real seams are `server_status` (the SC5
//! projection), the `Server`/`Store` source logs, and the `doctor` findings
//! overlay.

mod condition;
mod doctor;
mod exec;
mod generations;
mod jobs;
mod logs;
mod machines;
mod migration;
mod secrets;
mod server;
mod store;

pub use condition::{ProbeOutcome, SchemaState, StoreCondition, step};

use async_trait::async_trait;
use codchi_api::dto::*;
use codchi_api::error::ApiError;
use codchi_api::events::EventStreamOpts;
use codchi_api::ids::{FindingId, GenerationId, JobId, MachineId};
use codchi_api::service::{CodchiService, EventStream};
use codchi_api::testing::MockCodchiService;
use tokio::sync::watch;
use tokio_util::sync::CancellationToken;

use crate::db::Db;
use crate::logs::LogStore;

/// The single orchestration type that implements [`CodchiService`] directly.
///
/// Cheap state only: it *reads* the store condition (the supervisor is the sole
/// writer, SC7) and *projects* it on demand. Held behind an `Arc` in
/// [`AppState`](crate::AppState), so it need not be `Clone` itself.
pub struct ServerCore {
    /// Server-owned `Server`/`Store` source logs (C7). `Machine` logs stay on
    /// the mock until Phase 7; `logs` branches on the source.
    logs: LogStore,
    /// The store condition, written only by the supervisor (SC7). Read and
    /// projected by [`store`].
    condition: watch::Receiver<StoreCondition>,
    /// Tripped on SIGINT/SIGTERM; projects `Stopping` over the condition (SC8).
    shutdown: CancellationToken,
    /// The SQLite handle (DB7). `None` only when DB open itself failed — the
    /// daemon still serves a `Degraded` status so `codchi doctor`/`status` work
    /// (DB8). Phase 4+ domains read machine/job/secret state through it.
    db: Option<Db>,
    /// The schema subsystem's condition, fed to the lifecycle projection-join
    /// (DB8). Fixed after startup: migration is synchronous-before-serve.
    schema: SchemaState,
    /// `PRAGMA user_version` captured at startup — the `current` reported by
    /// `server_status` when the live DB cannot be read (absent/failed).
    schema_current: u32,
    /// The quarantined placeholder backing every still-unbacked domain (SC2).
    mock: MockCodchiService,
}

impl ServerCore {
    /// Build the core over the supervisor's condition receiver, the daemon's
    /// shutdown token, and the already-opened+migrated DB (the real `main`
    /// path). A non-migrated DB can never reach the core: `main` opens and
    /// migrates before calling this (DB7).
    pub fn new(
        logs: LogStore,
        condition: watch::Receiver<StoreCondition>,
        shutdown: CancellationToken,
        db: Option<Db>,
        schema: SchemaState,
        schema_current: u32,
    ) -> Self {
        Self {
            logs,
            condition,
            shutdown,
            db,
            schema,
            schema_current,
            mock: MockCodchiService::new(),
        }
    }
}

#[async_trait]
impl CodchiService for ServerCore {
    // ---- server lifecycle ----
    async fn server_status(&self) -> Result<ServerStatus, ApiError> {
        self.server_status().await
    }

    // ---- machines ----
    async fn list_machines(&self) -> Result<Vec<MachineView>, ApiError> {
        self.list_machines().await
    }
    async fn get_machine(&self, id: &MachineId) -> Result<MachineDetail, ApiError> {
        self.get_machine(id).await
    }
    async fn create_machine(&self, req: CreateMachineRequest) -> Result<JobView<()>, ApiError> {
        self.create_machine(req).await
    }
    async fn duplicate_machine(
        &self,
        source: &MachineId,
        req: DuplicateMachineRequest,
    ) -> Result<JobView<Duplicated>, ApiError> {
        self.duplicate_machine(source, req).await
    }
    async fn delete_machine(&self, id: &MachineId) -> Result<JobView<()>, ApiError> {
        self.delete_machine(id).await
    }

    // ---- modules / config / secrets ----
    async fn set_modules(&self, id: &MachineId, req: SetModulesRequest) -> Result<(), ApiError> {
        self.set_modules(id, req).await
    }
    async fn set_secret(
        &self,
        id: &MachineId,
        key: SecretName,
        value: String,
    ) -> Result<(), ApiError> {
        self.set_secret(id, key, value).await
    }
    async fn get_secret(&self, id: &MachineId, key: SecretName) -> Result<String, ApiError> {
        self.get_secret(id, key).await
    }
    async fn list_secrets(&self, id: &MachineId) -> Result<Vec<SecretKey>, ApiError> {
        self.list_secrets(id).await
    }
    async fn delete_secret(&self, id: &MachineId, key: SecretName) -> Result<(), ApiError> {
        self.delete_secret(id, key).await
    }

    // ---- build / update / activation ----
    async fn rebuild(&self, id: &MachineId) -> Result<JobView<Rebuilt>, ApiError> {
        self.rebuild(id).await
    }
    async fn update(&self, id: &MachineId) -> Result<JobView<Updated>, ApiError> {
        self.update(id).await
    }
    async fn list_generations(&self, id: &MachineId) -> Result<Vec<GenerationView>, ApiError> {
        self.list_generations(id).await
    }
    async fn activate_generation(
        &self,
        id: &MachineId,
        generation: GenerationId,
    ) -> Result<JobView<()>, ApiError> {
        self.activate_generation(id, generation).await
    }

    // ---- store generations ----
    async fn list_store_generations(&self) -> Result<Vec<StoreGenerationView>, ApiError> {
        self.list_store_generations().await
    }

    // ---- config resolution (R3) ----
    async fn resolve_config(
        &self,
        req: ResolveConfigRequest,
    ) -> Result<JobView<ConfigResolution>, ApiError> {
        self.resolve_config(req).await
    }

    // ---- exec (R7) ----
    async fn prepare_exec(
        &self,
        id: &MachineId,
        req: PrepareExecRequest,
    ) -> Result<JobView<ExecPlan>, ApiError> {
        self.prepare_exec(id, req).await
    }

    // ---- jobs ----
    async fn list_jobs(&self, filter: JobFilter) -> Result<Vec<JobView>, ApiError> {
        self.list_jobs(filter).await
    }
    async fn get_job(&self, id: &JobId) -> Result<JobView, ApiError> {
        self.get_job(id).await
    }
    async fn cancel_job(&self, id: &JobId) -> Result<(), ApiError> {
        self.cancel_job(id).await
    }
    async fn stream_job_events(
        &self,
        id: &JobId,
        opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError> {
        self.stream_job_events(id, opts).await
    }

    // ---- logs (R11) ----
    async fn stream_logs(
        &self,
        source: LogSource,
        opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError> {
        self.stream_logs(source, opts).await
    }

    // ---- doctor ----
    async fn doctor(&self, opts: DoctorOpts) -> Result<DoctorReport, ApiError> {
        self.doctor(opts).await
    }
    async fn doctor_scan(&self, opts: DoctorOpts) -> Result<JobView<DoctorReport>, ApiError> {
        self.doctor_scan(opts).await
    }
    async fn doctor_fix(&self, finding: FindingId) -> Result<JobView<()>, ApiError> {
        self.doctor_fix(finding).await
    }

    // ---- migration ----
    async fn migration_plan(&self) -> Result<MigrationPlan, ApiError> {
        self.migration_plan().await
    }
    async fn migration_run(
        &self,
        opts: MigrationOpts,
    ) -> Result<JobView<MigrationSummary>, ApiError> {
        self.migration_run(opts).await
    }
}
