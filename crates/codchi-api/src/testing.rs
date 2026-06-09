//! [`MockCodchiService`] — an in-memory fake of the full contract.
//!
//! Returns plausible, self-consistent data for every endpoint so CLI and tray
//! agents can develop against the trait without a running server. It performs
//! no real work and keeps no persistent state; mutating calls succeed and
//! read calls return canned fixtures.

use std::pin::Pin;
use std::task::{Context, Poll};

use async_trait::async_trait;
use chrono::{DateTime, TimeZone, Utc};
use futures_core::Stream;

use crate::dto::*;
use crate::error::ApiError;
use crate::events::{Event, EventStreamOpts, LogLevel, PhaseStatus};
use crate::ids::{EventSeq, FindingId, GenerationId, JobId, MachineId, StoreGenerationId};
use crate::service::{CodchiService, EventStream};

/// In-memory fake implementation of [`CodchiService`].
#[derive(Clone, Debug, Default)]
pub struct MockCodchiService;

impl MockCodchiService {
    /// Construct a fresh mock.
    pub fn new() -> Self {
        MockCodchiService
    }
}

/// A fixed timestamp so fixtures are deterministic across calls.
fn fixed_time() -> DateTime<Utc> {
    Utc.with_ymd_and_hms(2026, 1, 1, 12, 0, 0).single().unwrap()
}

fn sample_module() -> ModuleSpec {
    ModuleSpec {
        name: "base".to_owned(),
        url: "github://github.com/aformatik/codchi?#nixosModules.base".to_owned(),
        is_nixpkgs_source: true,
    }
}

fn sample_secret() -> SecretKey {
    SecretKey {
        name: "GITHUB_TOKEN".to_owned(),
        description: "Token used to fetch private flakes".to_owned(),
        has_value: true,
    }
}

fn sample_generation(id: u64) -> GenerationView {
    GenerationView {
        id: GenerationId(id),
        created_at: fixed_time(),
        activated_at: Some(fixed_time()),
        flake_lock_hash: "sha256-0000000000000000000000000000000000000000000=".to_owned(),
        system_store_path: format!("/nix/store/mock-system-{id}"),
        status: GenerationStatus::Active,
        source_job: JobId::new(),
    }
}

fn sample_machine_view(id: MachineId) -> MachineView {
    MachineView {
        id,
        run_status: RunStatus::Running,
        update_status: UpdateStatus::UpToDate,
        active_generation: Some(GenerationId(1)),
        findings: Vec::new(),
        busy_with: None,
        schema_version: 1,
        last_reconciled_at: Some(fixed_time()),
        last_reconcile_attempt_at: Some(fixed_time()),
        snapshot_stale: false,
    }
}

/// A freshly-accepted job (Queued, no terminal output yet). Generic over the
/// typed output `O` so it serves every job-returning endpoint.
fn accepted_job<O>(kind: JobKind, machine: Option<MachineId>) -> JobView<O> {
    JobView {
        id: JobId::new(),
        kind,
        // R11: a machine-less operation is `Server`-subject in the mock.
        subject: machine.map(LogSource::Machine).unwrap_or(LogSource::Server),
        state: JobState::Queued,
        created_at: fixed_time(),
        started_at: None,
        finished_at: None,
        error: None,
        output: None,
        last_event_seq: EventSeq(0),
    }
}

/// A small canned event stream (Stream over a fixed vec). Used by the mock so
/// the trait stays dependency-light (no `futures` combinators).
struct VecEventStream(std::vec::IntoIter<Result<Event, ApiError>>);

impl Stream for VecEventStream {
    type Item = Result<Event, ApiError>;
    fn poll_next(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        Poll::Ready(self.0.next())
    }
}

#[async_trait]
impl CodchiService for MockCodchiService {
    async fn server_status(&self) -> Result<ServerStatus, ApiError> {
        Ok(ServerStatus {
            lifecycle: ServerLifecycle::Ready,
            api_version: crate::version::API_VERSION,
            server_version: "0.0.0-mock".to_owned(),
            started_at: fixed_time(),
            store: StoreStatus {
                state: StoreState::Up,
                last_checked_at: Some(fixed_time()),
                last_error: None,
            },
            schema: SchemaStatus {
                current: 1,
                required: 1,
                migrating: false,
            },
            last_reconciled_at: Some(fixed_time()),
            findings_summary: FindingsSummary::default(),
            startup_error: None,
        })
    }

    async fn list_machines(&self) -> Result<Vec<MachineView>, ApiError> {
        Ok(vec![sample_machine_view(MachineId("demo".to_owned()))])
    }

    async fn get_machine(&self, id: &MachineId) -> Result<MachineDetail, ApiError> {
        Ok(MachineDetail {
            view: sample_machine_view(id.clone()),
            modules: vec![sample_module()],
            secrets: vec![sample_secret()],
            flake_lock_hash: "sha256-0000000000000000000000000000000000000000000=".to_owned(),
            generations: vec![sample_generation(1)],
        })
    }

    async fn create_machine(&self, req: CreateMachineRequest) -> Result<JobView<()>, ApiError> {
        req.id.validate("id")?;
        Ok(accepted_job(JobKind::Init, Some(req.id)))
    }

    async fn clone_machine(
        &self,
        source: &MachineId,
        req: CloneMachineRequest,
    ) -> Result<JobView<()>, ApiError> {
        source.validate("id")?;
        req.target.validate("target")?;
        Ok(accepted_job(JobKind::Clone, Some(req.target)))
    }

    async fn delete_machine(&self, id: &MachineId) -> Result<JobView<()>, ApiError> {
        Ok(accepted_job(JobKind::Delete, Some(id.clone())))
    }

    async fn set_modules(&self, id: &MachineId, _req: SetModulesRequest) -> Result<(), ApiError> {
        id.validate("id")?;
        Ok(())
    }

    async fn set_secret(
        &self,
        id: &MachineId,
        _key: String,
        _value: String,
    ) -> Result<(), ApiError> {
        id.validate("id")?;
        Ok(())
    }

    async fn get_secret(&self, _id: &MachineId, _key: String) -> Result<String, ApiError> {
        Ok("mock-secret-value".to_owned())
    }

    async fn list_secrets(&self, _id: &MachineId) -> Result<Vec<SecretKey>, ApiError> {
        Ok(vec![sample_secret()])
    }

    async fn delete_secret(&self, _id: &MachineId, _key: String) -> Result<(), ApiError> {
        Ok(())
    }

    async fn rebuild(&self, id: &MachineId) -> Result<JobView<Rebuilt>, ApiError> {
        id.validate("id")?;
        Ok(accepted_job(JobKind::Rebuild, Some(id.clone())))
    }

    async fn update(&self, id: &MachineId) -> Result<JobView<Updated>, ApiError> {
        id.validate("id")?;
        Ok(accepted_job(JobKind::Update, Some(id.clone())))
    }

    async fn list_generations(&self, _id: &MachineId) -> Result<Vec<GenerationView>, ApiError> {
        Ok(vec![sample_generation(1)])
    }

    async fn activate_generation(
        &self,
        id: &MachineId,
        _generation: GenerationId,
    ) -> Result<JobView<()>, ApiError> {
        Ok(accepted_job(JobKind::ActivateGeneration, Some(id.clone())))
    }

    async fn list_store_generations(&self) -> Result<Vec<StoreGenerationView>, ApiError> {
        Ok(vec![StoreGenerationView {
            id: StoreGenerationId(1),
            created_at: fixed_time(),
            activated_at: Some(fixed_time()),
            flake_lock_hash: "sha256-0000000000000000000000000000000000000000000=".to_owned(),
            runtime_store_path: "/nix/store/mock-store-1".to_owned(),
            status: GenerationStatus::Active,
        }])
    }

    async fn resolve_config(
        &self,
        _req: ResolveConfigRequest,
    ) -> Result<JobView<ConfigResolution>, ApiError> {
        Ok(accepted_job(JobKind::Resolve, None))
    }

    async fn prepare_exec(
        &self,
        id: &MachineId,
        _req: PrepareExecRequest,
    ) -> Result<JobView<ExecPlan>, ApiError> {
        id.validate("id")?;
        Ok(accepted_job(JobKind::PrepareExec, Some(id.clone())))
    }

    async fn get_job(&self, id: &JobId) -> Result<JobView, ApiError> {
        // A terminal, successful rebuild for demonstration (R1). `get_job` is
        // kind-erased, so it returns the default `JobView<JobOutput>`.
        Ok(JobView {
            id: *id,
            kind: JobKind::Rebuild,
            subject: LogSource::Machine(MachineId("demo".to_owned())),
            state: JobState::Succeeded,
            created_at: fixed_time(),
            started_at: Some(fixed_time()),
            finished_at: Some(fixed_time()),
            error: None,
            output: Some(JobOutput::Rebuilt(Rebuilt {
                generation: GenerationId(2),
            })),
            last_event_seq: EventSeq(3),
        })
    }

    async fn list_jobs(&self, _filter: JobFilter) -> Result<Vec<JobView>, ApiError> {
        // A running store-start job plus a terminal machine rebuild, to show
        // the subject variety (R11).
        Ok(vec![
            JobView {
                id: JobId::new(),
                kind: JobKind::StoreStart,
                subject: LogSource::Store,
                state: JobState::Running,
                created_at: fixed_time(),
                started_at: Some(fixed_time()),
                finished_at: None,
                error: None,
                output: None,
                last_event_seq: EventSeq(1),
            },
            self.get_job(&JobId::new()).await?,
        ])
    }

    async fn cancel_job(&self, _id: &JobId) -> Result<(), ApiError> {
        Ok(())
    }

    async fn stream_job_events(
        &self,
        _id: &JobId,
        _opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError> {
        let events = vec![
            Ok(Event::Phase {
                seq: EventSeq(1),
                ts: fixed_time(),
                name: "build".to_owned(),
                status: PhaseStatus::Started,
            }),
            Ok(Event::Log {
                seq: EventSeq(2),
                ts: fixed_time(),
                level: LogLevel::Info,
                topic: "build".to_owned(),
                message: "building mock system".to_owned(),
            }),
            Ok(Event::Phase {
                seq: EventSeq(3),
                ts: fixed_time(),
                name: "build".to_owned(),
                status: PhaseStatus::Finished,
            }),
        ];
        Ok(Box::pin(VecEventStream(events.into_iter())))
    }

    async fn stream_logs(
        &self,
        _source: LogSource,
        _opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError> {
        let events = vec![
            Ok(Event::Log {
                seq: EventSeq(1),
                ts: fixed_time(),
                level: LogLevel::Info,
                topic: "store".to_owned(),
                message: "store container started".to_owned(),
            }),
            Ok(Event::Log {
                seq: EventSeq(2),
                ts: fixed_time(),
                level: LogLevel::Info,
                topic: "gc".to_owned(),
                message: "store gc complete".to_owned(),
            }),
        ];
        Ok(Box::pin(VecEventStream(events.into_iter())))
    }

    async fn doctor(&self, _opts: DoctorOpts) -> Result<DoctorReport, ApiError> {
        Ok(DoctorReport {
            findings: Vec::new(),
            generated_at: fixed_time(),
        })
    }

    async fn doctor_scan(&self, _opts: DoctorOpts) -> Result<JobView<DoctorReport>, ApiError> {
        Ok(accepted_job(JobKind::DoctorScan, None))
    }

    async fn doctor_fix(&self, _finding: FindingId) -> Result<JobView<()>, ApiError> {
        Ok(accepted_job(JobKind::DoctorFix, None))
    }

    async fn migration_plan(&self) -> Result<MigrationPlan, ApiError> {
        Ok(MigrationPlan {
            beta_detected: false,
            machines: Vec::new(),
        })
    }

    async fn migration_run(
        &self,
        _opts: MigrationOpts,
    ) -> Result<JobView<MigrationSummary>, ApiError> {
        Ok(accepted_job(JobKind::Migration, None))
    }
}
