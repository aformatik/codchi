//! The `CodchiService` trait — the semantic service surface.
//!
//! The server implements this trait directly; the HTTP client implements the
//! same trait over the wire; tests use [`crate::testing::MockCodchiService`].
//! The URL mapping lives in [`crate::endpoints`] (the typed [`crate::endpoints::Endpoint`]
//! catalog), the single source of truth reused by the `axum` router, the typed
//! client, and OpenAPI generation (P1).

use std::pin::Pin;

use async_trait::async_trait;
use futures_core::Stream;

use crate::dto::*;
use crate::error::ApiError;
use crate::events::{Event, EventStreamOpts};
use crate::ids::{FindingId, GenerationId, JobId, MachineId};

/// A streamed sequence of job events. Each item is an [`Event`] or a stream
/// error. Boxed so the trait stays object-safe and transport-agnostic.
pub type EventStream = Pin<Box<dyn Stream<Item = Result<Event, ApiError>> + Send>>;

/// The complete v1 service surface. Sync vs. job mapping follows the Q4 table:
/// sync methods read or record Codchi state and return data directly; job
/// methods reconcile state with external reality and return a [`JobView`] the
/// caller observes via [`CodchiService::stream_job_events`].
#[async_trait]
pub trait CodchiService: Send + Sync {
    // ---- server lifecycle ----
    async fn server_status(&self) -> Result<ServerStatus, ApiError>;

    // ---- machines ----
    async fn list_machines(&self) -> Result<Vec<MachineView>, ApiError>;
    async fn get_machine(&self, id: &MachineId) -> Result<MachineDetail, ApiError>;
    async fn create_machine(&self, req: CreateMachineRequest) -> Result<JobView<()>, ApiError>;
    /// `source` is the `{id}` path segment; `req` carries only the new name.
    async fn clone_machine(
        &self,
        source: &MachineId,
        req: CloneMachineRequest,
    ) -> Result<JobView<()>, ApiError>;
    async fn delete_machine(&self, id: &MachineId) -> Result<JobView<()>, ApiError>;

    // ---- modules / config / secrets ----
    async fn set_modules(&self, id: &MachineId, req: SetModulesRequest) -> Result<(), ApiError>;
    async fn set_secret(
        &self,
        id: &MachineId,
        key: SecretName,
        value: String,
    ) -> Result<(), ApiError>;
    async fn get_secret(&self, id: &MachineId, key: SecretName) -> Result<String, ApiError>;
    async fn list_secrets(&self, id: &MachineId) -> Result<Vec<SecretKey>, ApiError>;
    async fn delete_secret(&self, id: &MachineId, key: SecretName) -> Result<(), ApiError>;

    // ---- build / update / activation ----
    async fn rebuild(&self, id: &MachineId) -> Result<JobView<Rebuilt>, ApiError>;
    async fn update(&self, id: &MachineId) -> Result<JobView<Updated>, ApiError>;
    async fn list_generations(&self, id: &MachineId) -> Result<Vec<GenerationView>, ApiError>;
    async fn activate_generation(
        &self,
        id: &MachineId,
        generation: GenerationId,
    ) -> Result<JobView<()>, ApiError>;

    // ---- store generations (read-only for v1) ----
    async fn list_store_generations(&self) -> Result<Vec<StoreGenerationView>, ApiError>;

    // ---- config resolution (R3) ----
    async fn resolve_config(
        &self,
        req: ResolveConfigRequest,
    ) -> Result<JobView<ConfigResolution>, ApiError>;

    // ---- exec (R7) ----
    /// `id` is the `{id}` path segment; `req` carries only the command.
    async fn prepare_exec(
        &self,
        id: &MachineId,
        req: PrepareExecRequest,
    ) -> Result<JobView<ExecPlan>, ApiError>;

    // ---- jobs ----
    /// List jobs across all subjects (R11), newest first; `filter` narrows by
    /// subject kind / machine / activity.
    async fn list_jobs(&self, filter: JobFilter) -> Result<Vec<JobView>, ApiError>;
    async fn get_job(&self, id: &JobId) -> Result<JobView, ApiError>;
    async fn cancel_job(&self, id: &JobId) -> Result<(), ApiError>;
    async fn stream_job_events(
        &self,
        id: &JobId,
        opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError>;

    // ---- logs (R11) ----
    /// Stream a log source's durable log — the server, the store, or a machine.
    /// `stream_job_events` is the job-correlated view over this same store;
    /// `opts.follow = false` does a bounded `tail` replay and then ends.
    async fn stream_logs(
        &self,
        source: LogSource,
        opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError>;

    // ---- doctor ----
    async fn doctor(&self, opts: DoctorOpts) -> Result<DoctorReport, ApiError>;
    async fn doctor_scan(&self, opts: DoctorOpts) -> Result<JobView<DoctorReport>, ApiError>;
    async fn doctor_fix(&self, finding: FindingId) -> Result<JobView<()>, ApiError>;

    // ---- migration ----
    async fn migration_plan(&self) -> Result<MigrationPlan, ApiError>;
    async fn migration_run(
        &self,
        opts: MigrationOpts,
    ) -> Result<JobView<MigrationSummary>, ApiError>;
}
