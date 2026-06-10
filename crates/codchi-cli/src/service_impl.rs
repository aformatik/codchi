//! `impl CodchiService for HttpClient` — the semantic surface over the wire.
//!
//! Each method is the one-line residual glue from doc 06: it maps the call's
//! arguments to a typed `(Path, Query, Body)` and dispatches through the generic
//! `call_*` helper for the endpoint's response shape. Everything mechanical
//! lives in [`crate::client`]; this file is just the argument mapping Rust can't
//! derive.

use async_trait::async_trait;
use codchi_api::dto::*;
use codchi_api::endpoints::*;
use codchi_api::error::ApiError;
use codchi_api::events::EventStreamOpts;
use codchi_api::ids::{FindingId, GenerationId, JobId, MachineId};
use codchi_api::service::{CodchiService, EventStream};

use crate::client::HttpClient;

#[async_trait]
impl CodchiService for HttpClient {
    async fn server_status(&self) -> Result<ServerStatus, ApiError> {
        self.call_json::<ServerStatusEp>((), (), ()).await
    }

    async fn list_machines(&self) -> Result<Vec<MachineView>, ApiError> {
        self.call_json::<ListMachinesEp>((), (), ()).await
    }

    async fn get_machine(&self, id: &MachineId) -> Result<MachineDetail, ApiError> {
        self.call_json::<GetMachineEp>((id.clone(),), (), ()).await
    }

    async fn create_machine(&self, req: CreateMachineRequest) -> Result<JobView<()>, ApiError> {
        self.call_json::<CreateMachineEp>((), (), req).await
    }

    async fn clone_machine(
        &self,
        source: &MachineId,
        req: CloneMachineRequest,
    ) -> Result<JobView<()>, ApiError> {
        self.call_json::<CloneMachineEp>((source.clone(),), (), req)
            .await
    }

    async fn delete_machine(&self, id: &MachineId) -> Result<JobView<()>, ApiError> {
        self.call_json::<DeleteMachineEp>((id.clone(),), (), ())
            .await
    }

    async fn set_modules(&self, id: &MachineId, req: SetModulesRequest) -> Result<(), ApiError> {
        self.call_empty::<SetModulesEp>((id.clone(),), (), req)
            .await
    }

    async fn set_secret(
        &self,
        id: &MachineId,
        key: SecretName,
        value: String,
    ) -> Result<(), ApiError> {
        self.call_empty::<SetSecretEp>((id.clone(), key), (), SetSecretRequest { value })
            .await
    }

    async fn get_secret(&self, id: &MachineId, key: SecretName) -> Result<String, ApiError> {
        self.call_json::<GetSecretEp>((id.clone(), key), (), ())
            .await
    }

    async fn list_secrets(&self, id: &MachineId) -> Result<Vec<SecretKey>, ApiError> {
        self.call_json::<ListSecretsEp>((id.clone(),), (), ()).await
    }

    async fn delete_secret(&self, id: &MachineId, key: SecretName) -> Result<(), ApiError> {
        self.call_empty::<DeleteSecretEp>((id.clone(), key), (), ())
            .await
    }

    async fn rebuild(&self, id: &MachineId) -> Result<JobView<Rebuilt>, ApiError> {
        self.call_json::<RebuildEp>((id.clone(),), (), ()).await
    }

    async fn update(&self, id: &MachineId) -> Result<JobView<Updated>, ApiError> {
        self.call_json::<UpdateEp>((id.clone(),), (), ()).await
    }

    async fn list_generations(&self, id: &MachineId) -> Result<Vec<GenerationView>, ApiError> {
        self.call_json::<ListGenerationsEp>((id.clone(),), (), ())
            .await
    }

    async fn activate_generation(
        &self,
        id: &MachineId,
        generation: GenerationId,
    ) -> Result<JobView<()>, ApiError> {
        self.call_json::<ActivateGenerationEp>((id.clone(), generation), (), ())
            .await
    }

    async fn list_store_generations(&self) -> Result<Vec<StoreGenerationView>, ApiError> {
        self.call_json::<ListStoreGenerationsEp>((), (), ()).await
    }

    async fn resolve_config(
        &self,
        req: ResolveConfigRequest,
    ) -> Result<JobView<ConfigResolution>, ApiError> {
        self.call_json::<ResolveConfigEp>((), (), req).await
    }

    async fn prepare_exec(
        &self,
        id: &MachineId,
        req: PrepareExecRequest,
    ) -> Result<JobView<ExecPlan>, ApiError> {
        self.call_json::<PrepareExecEp>((id.clone(),), (), req)
            .await
    }

    async fn list_jobs(&self, filter: JobFilter) -> Result<Vec<JobView>, ApiError> {
        self.call_json::<ListJobsEp>((), filter, ()).await
    }

    async fn get_job(&self, id: &JobId) -> Result<JobView, ApiError> {
        self.call_json::<GetJobEp>((*id,), (), ()).await
    }

    async fn cancel_job(&self, id: &JobId) -> Result<(), ApiError> {
        self.call_empty::<CancelJobEp>((*id,), (), ()).await
    }

    async fn stream_job_events(
        &self,
        id: &JobId,
        opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError> {
        self.call_ndjson::<StreamJobEventsEp>((*id,), opts, ())
            .await
    }

    async fn stream_logs(
        &self,
        source: LogSource,
        opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError> {
        self.call_ndjson::<StreamLogsEp>((source,), opts, ()).await
    }

    async fn doctor(&self, opts: DoctorOpts) -> Result<DoctorReport, ApiError> {
        self.call_json::<DoctorEp>((), opts, ()).await
    }

    async fn doctor_scan(&self, opts: DoctorOpts) -> Result<JobView<DoctorReport>, ApiError> {
        self.call_json::<DoctorScanEp>((), (), opts).await
    }

    async fn doctor_fix(&self, finding: FindingId) -> Result<JobView<()>, ApiError> {
        self.call_json::<DoctorFixEp>((finding,), (), ()).await
    }

    async fn migration_plan(&self) -> Result<MigrationPlan, ApiError> {
        self.call_json::<MigrationPlanEp>((), (), ()).await
    }

    async fn migration_run(
        &self,
        opts: MigrationOpts,
    ) -> Result<JobView<MigrationSummary>, ApiError> {
        self.call_json::<MigrationRunEp>((), (), opts).await
    }
}
