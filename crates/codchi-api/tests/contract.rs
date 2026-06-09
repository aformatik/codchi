//! Contract tests: serde roundtrips for every DTO and every `ApiError`
//! variant, error-code stability, OpenAPI completeness, and a mock smoke test.

use std::fmt::Debug;

use chrono::{TimeZone, Utc};
use codchi_api::dto::*;
use codchi_api::endpoints::ROUTES;
use codchi_api::error::ApiError;
use codchi_api::events::{Event, EventStreamOpts, LogLevel, NixBuildStatus, PhaseStatus};
use codchi_api::ids::*;
use codchi_api::service::CodchiService;
use codchi_api::testing::MockCodchiService;
use serde::Serialize;
use serde::de::DeserializeOwned;

fn ts() -> chrono::DateTime<chrono::Utc> {
    Utc.with_ymd_and_hms(2026, 1, 1, 0, 0, 0).single().unwrap()
}

/// Assert JSON serialize → deserialize is a fixed point.
fn roundtrip<T: Serialize + DeserializeOwned + PartialEq + Debug>(value: T) {
    let json = serde_json::to_string(&value).expect("serialize");
    let back: T = serde_json::from_str(&json).expect("deserialize");
    assert_eq!(value, back, "roundtrip mismatch for {json}");
}

#[test]
fn api_error_variants_roundtrip() {
    let machine = MachineId("foo".to_owned());
    let job = JobId::new();
    let variants = vec![
        ApiError::MachineNotFound {
            machine: machine.clone(),
        },
        ApiError::MachineBusy {
            machine: machine.clone(),
            job,
        },
        ApiError::JobNotFound { job },
        ApiError::JobNotCancellable {
            job,
            state: JobState::CleaningUp,
        },
        ApiError::StoreUnavailable {
            reason: "down".to_owned(),
        },
        ApiError::StoreBusy { job },
        ApiError::SchemaMigrationRequired {
            current: 1,
            required: 2,
        },
        ApiError::ResumeGapTooLarge {
            requested: EventSeq(10),
            oldest: EventSeq(42),
        },
        ApiError::ApiVersionMismatch {
            client: 1,
            server: 2,
        },
        ApiError::MissingRequiredSecrets {
            machine,
            keys: vec![SecretKey {
                name: "TOKEN".to_owned(),
                description: "desc".to_owned(),
                has_value: false,
            }],
        },
        ApiError::Validation {
            field: "id".to_owned(),
            message: "bad".to_owned(),
        },
        ApiError::Internal {
            message: "boom".to_owned(),
        },
    ];
    for v in variants {
        roundtrip(v);
    }
}

#[test]
fn api_error_codes_are_stable() {
    let job = JobId::new();
    let cases = [
        (
            ApiError::MachineNotFound {
                machine: MachineId("x".into()),
            },
            "machine_not_found",
        ),
        (ApiError::JobNotFound { job }, "job_not_found"),
        (ApiError::StoreBusy { job }, "store_busy"),
        (
            ApiError::Validation {
                field: "f".into(),
                message: "m".into(),
            },
            "validation",
        ),
        (
            ApiError::Internal {
                message: "m".into(),
            },
            "internal",
        ),
    ];
    for (err, code) in cases {
        assert_eq!(err.code(), code);
        let json: serde_json::Value = serde_json::to_value(&err).unwrap();
        assert_eq!(json["code"], code, "wire code must match code()");
    }
}

#[test]
fn job_output_variants_roundtrip() {
    let outputs = vec![
        JobOutput::ConfigResolution(ConfigResolution {
            available_modules: vec![ModuleSpec {
                name: "base".into(),
                url: "github://github.com/aformatik/codchi?#nixosModules.base".into(),
                is_nixpkgs_source: true,
            }],
            nixpkgs_input_present: true,
        }),
        JobOutput::Rebuilt(Rebuilt {
            generation: GenerationId(2),
        }),
        JobOutput::Updated(Updated {
            generation: GenerationId(3),
            input_changes: vec![LockInputChange {
                input: "nixpkgs".into(),
                old_rev: Some("a".into()),
                new_rev: Some("b".into()),
            }],
        }),
        JobOutput::GarbageCollected(GarbageCollected { freed_bytes: 1024 }),
        JobOutput::Migrated(MigrationSummary {
            migrated: 1,
            skipped: 0,
            failed: 0,
        }),
        JobOutput::Scanned(DoctorReport {
            findings: vec![],
            generated_at: ts(),
        }),
        JobOutput::ExecPlan(ExecPlan {
            machine: MachineId("foo".into()),
            target: "codchi-machine-foo".into(),
            command: vec!["bash".into()],
            cwd: None,
        }),
    ];
    for o in outputs {
        roundtrip(o);
    }
}

#[test]
fn event_variants_roundtrip() {
    let events = vec![
        Event::Log {
            seq: EventSeq(1),
            ts: ts(),
            level: LogLevel::Warn,
            topic: "eval".into(),
            message: "msg".into(),
        },
        Event::Progress {
            seq: EventSeq(2),
            ts: ts(),
            phase: "build".into(),
            done: 3,
            total: Some(10),
        },
        Event::Phase {
            seq: EventSeq(3),
            ts: ts(),
            name: "activate".into(),
            status: PhaseStatus::Finished,
        },
        Event::StateChange {
            seq: EventSeq(4),
            ts: ts(),
            state: JobState::Running,
        },
        Event::NixBuild {
            seq: EventSeq(5),
            ts: ts(),
            drv: "/nix/store/x.drv".into(),
            status: NixBuildStatus::Built,
        },
        Event::HealthFinding {
            seq: EventSeq(6),
            ts: ts(),
            finding: FindingId::new(),
        },
    ];
    for e in &events {
        assert_eq!(e.seq(), e.seq()); // exercise accessor
        roundtrip(e.clone());
    }
}

#[test]
fn core_views_roundtrip() {
    roundtrip(ServerStatus {
        lifecycle: ServerLifecycle::Degraded,
        api_version: 1,
        server_version: "1.2.3".into(),
        started_at: ts(),
        store: StoreStatus {
            state: StoreState::Recovering,
            last_checked_at: Some(ts()),
            last_error: Some("flaky".into()),
        },
        schema: SchemaStatus {
            current: 1,
            required: 1,
            migrating: false,
        },
        last_reconciled_at: None,
        findings_summary: FindingsSummary {
            critical: 1,
            error: 2,
            warning: 0,
            info: 5,
        },
        startup_error: Some(ApiError::StoreUnavailable {
            reason: "boot".into(),
        }),
    });

    let view = MachineView {
        id: MachineId("demo".into()),
        run_status: RunStatus::Running,
        update_status: UpdateStatus::NeedsRebuild,
        active_generation: Some(GenerationId(1)),
        findings: vec![Finding {
            id: FindingId::new(),
            severity: Severity::Warning,
            component: Component::Machine,
            machine: Some(MachineId("demo".into())),
            source_job: None,
            code: "podman.mount_missing".into(),
            message: "mount gone".into(),
            suggested_action: Some("run doctor fix".into()),
            auto_fixable: true,
            created_at: ts(),
        }],
        busy_with: None,
        schema_version: 1,
        last_reconciled_at: Some(ts()),
        last_reconcile_attempt_at: Some(ts()),
        snapshot_stale: false,
    };
    roundtrip(view.clone());

    roundtrip(MachineDetail {
        view,
        modules: vec![],
        secrets: vec![],
        flake_lock_hash: "h".into(),
        generations: vec![],
    });

    roundtrip(EventStreamOpts {
        tail: Some(50),
        since_seq: Some(EventSeq(7)),
        follow: false,
    });
}

#[test]
fn health_is_worst_severity() {
    assert_eq!(health(&[]), Severity::Info);
    let f = |sev| Finding {
        id: FindingId::new(),
        severity: sev,
        component: Component::Machine,
        machine: None,
        source_job: None,
        code: "x".into(),
        message: "m".into(),
        suggested_action: None,
        auto_fixable: false,
        created_at: ts(),
    };
    assert_eq!(
        health(&[f(Severity::Info), f(Severity::Error), f(Severity::Warning)]),
        Severity::Error
    );
    assert!(!is_healthy(&[f(Severity::Warning)]));
    assert!(is_healthy(&[f(Severity::Info)]));
}

#[test]
fn machine_id_validation() {
    for ok in [
        "a",
        "myMachine",
        "gpu_test",
        "a.b-c_d",
        "x".repeat(63).as_str(),
    ] {
        assert!(
            MachineId(ok.to_owned()).validate("id").is_ok(),
            "{ok} should pass"
        );
    }
    for bad in [
        "",
        "-leading",
        "trailing-",
        ".dot",
        "has space",
        "weird$",
        "codchi-machine-foo",
        "CODCHI-MACHINE-foo",
        "ünïcode",
    ] {
        assert!(
            MachineId(bad.to_owned()).validate("id").is_err(),
            "{bad} should fail"
        );
    }
    assert!(MachineId("x".repeat(64)).validate("id").is_err());
}

#[test]
fn openapi_covers_all_routes() {
    let doc = codchi_api::openapi::openapi();

    // Serializes cleanly.
    let json = serde_json::to_string_pretty(&doc).expect("serialize openapi");
    assert!(json.contains("\"openapi\": \"3.1.0\""));

    // Every route operation_id appears exactly once as an operationId.
    let mut found: Vec<String> = doc
        .operations()
        .filter_map(|(_, _, op)| op.operation_id.clone())
        .collect();
    found.sort();
    let mut expected: Vec<String> = ROUTES.iter().map(|r| r.operation_id.to_owned()).collect();
    expected.sort();
    assert_eq!(found, expected, "OpenAPI operations must match ROUTES");

    // Component schemas are present and use the component ref path. The
    // kind-erased `get_job` uses `JobView<JobOutput>`; job-specific endpoints
    // narrow to typed instantiations (here, `rebuild` -> `JobView<Rebuilt>`).
    assert!(json.contains("#/components/schemas/JobView_for_JobOutput"));
    assert!(json.contains("#/components/schemas/JobView_for_Rebuilt"));
    assert!(json.contains("#/components/schemas/ApiError"));
}

#[test]
fn endpoint_catalog_is_consistent() {
    use codchi_api::endpoints::{Endpoint, Method, RebuildEp, StreamJobEventsEp, route};

    // Every route carries the /v1 prefix and a unique operation id.
    assert_eq!(ROUTES.len(), 28);
    let mut ids: Vec<&str> = ROUTES.iter().map(|r| r.operation_id).collect();
    let count = ids.len();
    ids.sort_unstable();
    ids.dedup();
    assert_eq!(ids.len(), count, "operation ids must be unique");
    assert!(ROUTES.iter().all(|r| r.path.starts_with("/v1/")));

    // The typed markers and the lookup agree with their `Endpoint` metadata.
    assert_eq!(RebuildEp::METHOD, Method::Post);
    assert_eq!(RebuildEp::PATH, "/v1/machines/{id}/rebuild");
    assert_eq!(route("rebuild"), Some(&RebuildEp::ROUTE));
    assert_eq!(StreamJobEventsEp::OPERATION_ID, "stream_job_events");
    assert!(route("does_not_exist").is_none());
}

#[test]
fn mock_serves_every_endpoint() {
    futures::executor::block_on(async {
        let svc = MockCodchiService::new();
        let id = MachineId("demo".to_owned());

        svc.server_status().await.unwrap();
        svc.list_machines().await.unwrap();
        svc.get_machine(&id).await.unwrap();
        svc.create_machine(CreateMachineRequest {
            id: id.clone(),
            modules: vec![],
            keep_on_fail: false,
        })
        .await
        .unwrap();
        svc.clone_machine(
            &id,
            CloneMachineRequest {
                target: MachineId("demo2".into()),
            },
        )
        .await
        .unwrap();
        svc.delete_machine(&id).await.unwrap();
        svc.set_modules(&id, SetModulesRequest { modules: vec![] })
            .await
            .unwrap();
        svc.set_secret(&id, "K".into(), "V".into()).await.unwrap();
        svc.get_secret(&id, "K".into()).await.unwrap();
        svc.list_secrets(&id).await.unwrap();
        svc.delete_secret(&id, "K".into()).await.unwrap();
        svc.rebuild(&id).await.unwrap();
        svc.update(&id).await.unwrap();
        svc.list_generations(&id).await.unwrap();
        svc.activate_generation(&id, GenerationId(1)).await.unwrap();
        svc.list_store_generations().await.unwrap();
        svc.resolve_config(ResolveConfigRequest {
            url: "github://x/y?#m".into(),
        })
        .await
        .unwrap();
        svc.prepare_exec(&id, PrepareExecRequest { command: None })
            .await
            .unwrap();
        svc.list_jobs(JobFilter::default()).await.unwrap();
        let job = svc.get_job(&JobId::new()).await.unwrap();
        assert!(job.output.is_some());
        svc.cancel_job(&job.id).await.unwrap();
        svc.doctor(DoctorOpts::default()).await.unwrap();
        svc.doctor_scan(DoctorOpts::default()).await.unwrap();
        svc.doctor_fix(FindingId::new()).await.unwrap();
        svc.migration_plan().await.unwrap();
        svc.migration_run(MigrationOpts::default()).await.unwrap();

        // Stream yields the canned events.
        use futures::StreamExt;
        let stream = svc
            .stream_job_events(&job.id, EventStreamOpts::default())
            .await
            .unwrap();
        let collected: Vec<_> = stream.collect().await;
        assert_eq!(collected.len(), 3);
        assert!(collected.iter().all(|e| e.is_ok()));

        // R11: a source log stream (store) yields canned events too.
        let logs = svc
            .stream_logs(LogSource::Store, EventStreamOpts::default())
            .await
            .unwrap();
        let log_lines: Vec<_> = logs.collect().await;
        assert!(!log_lines.is_empty());
        assert!(log_lines.iter().all(|e| e.is_ok()));
    });
}
