//! Typed endpoint catalog — the single source of truth for the v1 URL map.
//!
//! Each route is a zero-sized marker type implementing [`Endpoint`], declared
//! once in the [`endpoints!`] block below. From that one declaration we derive:
//!
//! - [`ROUTES`] — cheap `const` metadata (method, path, operation id, summary)
//!   for routing and lookup;
//! - the OpenAPI document ([`crate::openapi`] consumes [`build_operations`]);
//! - and, in Phase 1, the `axum` router and the typed HTTP client, which are
//!   generic over `E: Endpoint` and read its associated `Body` / `Query` /
//!   `Response` types (see `v1/06-api-endpoint-codegen.md`).
//!
//! Endpoints are matched by **type**, not by stringly-typed `operation_id`
//! comparisons: adding a route is one entry here, and forgetting to wire its
//! request/response is a compile error, not a runtime panic.

use aide::openapi::{Operation, ReferenceOr, SchemaObject};
use schemars::r#gen::SchemaGenerator;

use crate::dto::*;
use crate::events::{Event, EventStreamOpts};
use crate::openapi::{path_param, path_params};

/// HTTP method for a [`Route`]. Only the methods the contract uses.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Method {
    Get,
    Post,
    Delete,
}

impl Method {
    /// Lowercase wire name, matching OpenAPI path-item keys.
    pub fn as_str(self) -> &'static str {
        match self {
            Method::Get => "get",
            Method::Post => "post",
            Method::Delete => "delete",
        }
    }
}

/// How an endpoint's success body is encoded on the wire.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ResponseShape {
    /// `application/json` body of [`Endpoint::Response`], status 200.
    Json,
    /// `application/x-ndjson` stream of [`Endpoint::Response`] objects (Q2),
    /// status 200.
    Ndjson,
    /// No body, status 204.
    Empty,
}

/// Static metadata for one route. `path` includes the `/v1` prefix (Q5) with
/// `{param}` placeholders; `operation_id` is the OpenAPI `operationId` and
/// matches the [`crate::service::CodchiService`] method name.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Route {
    pub method: Method,
    pub path: &'static str,
    pub operation_id: &'static str,
    pub summary: &'static str,
}

/// A typed API endpoint. One zero-sized marker type per route implements this.
///
/// The associated types name the request body, query, and success payload so
/// that transport code (OpenAPI now; the `axum` router and typed client in
/// Phase 1) can be generic over `E: Endpoint` instead of matching on strings.
/// `()` is the sentinel for "no body" / "no query" / "empty response".
pub trait Endpoint {
    const METHOD: Method;
    const PATH: &'static str;
    const OPERATION_ID: &'static str;
    const SUMMARY: &'static str;
    const SUCCESS_STATUS: u16;
    const RESPONSE: ResponseShape;

    /// Request body DTO; `()` ⇒ no body.
    type Body;
    /// Query-parameter struct; `()` ⇒ none. Its fields become query params.
    type Query;
    /// Success payload DTO; `()` ⇒ empty (204). For [`ResponseShape::Ndjson`]
    /// this is the element type streamed one-per-line.
    type Response;

    /// The cheap, `const` metadata for this endpoint.
    const ROUTE: Route = Route {
        method: Self::METHOD,
        path: Self::PATH,
        operation_id: Self::OPERATION_ID,
        summary: Self::SUMMARY,
    };
}

/// One built OpenAPI operation plus where it belongs in the path map.
pub(crate) struct OperationEntry {
    pub method: Method,
    pub path: &'static str,
    pub operation: Operation,
}

/// Map a [`ResponseShape`] token to its success status code.
macro_rules! status_for {
    (Json) => {
        200
    };
    (Ndjson) => {
        200
    };
    (Empty) => {
        204
    };
}

/// Attach the request body iff the endpoint declares one (`()` ⇒ no-op). The
/// `()` arm must precede the typed arm.
macro_rules! op_body {
    ($g:expr, $op:expr, ()) => {};
    ($g:expr, $op:expr, $body:ty) => {
        $op.request_body = Some($crate::openapi::json_body::<$body>($g));
    };
}

/// Attach one query parameter per field of the query struct (`()` ⇒ no-op).
macro_rules! op_query {
    ($g:expr, $op:expr, ()) => {};
    ($g:expr, $op:expr, $q:ty) => {
        for p in $crate::openapi::query_params::<$q>($g) {
            $op.parameters.push(ReferenceOr::Item(p));
        }
    };
}

/// Build the responses map for the endpoint's response shape.
macro_rules! op_resp {
    ($g:expr, Json, $status:expr, $ty:ty, $desc:expr, $err:expr) => {
        $crate::openapi::responses($status, $crate::openapi::ok_json::<$ty>($g, $desc), $err)
    };
    ($g:expr, Ndjson, $status:expr, $ty:ty, $desc:expr, $err:expr) => {
        $crate::openapi::responses($status, $crate::openapi::ok_ndjson::<$ty>($g, $desc), $err)
    };
    ($g:expr, Empty, $status:expr, $ty:ty, $desc:expr, $err:expr) => {
        $crate::openapi::responses($status, $crate::openapi::ok_empty($desc), $err)
    };
}

/// Declare every route once. Generates the marker types + [`Endpoint`] impls,
/// the [`ROUTES`] catalog, and [`build_operations`].
macro_rules! endpoints {
    ($(
        $marker:ident = $method:ident $path:literal $op:literal $summary:literal,
            body $body:tt, query $query:tt, $shape:ident $resp:ty
    );+ $(;)?) => {
        $(
            #[doc = concat!("Endpoint marker for `", $op, "` (`", $path, "`).")]
            #[derive(Clone, Copy, Debug)]
            pub struct $marker;

            impl Endpoint for $marker {
                const METHOD: Method = Method::$method;
                const PATH: &'static str = concat!("/v1", $path);
                const OPERATION_ID: &'static str = $op;
                const SUMMARY: &'static str = $summary;
                const SUCCESS_STATUS: u16 = status_for!($shape);
                const RESPONSE: ResponseShape = ResponseShape::$shape;
                type Body = $body;
                type Query = $query;
                type Response = $resp;
            }
        )+

        /// The full route catalog — the single source of truth for routing,
        /// the typed client, and OpenAPI (P1). Order is the documented order.
        pub const ROUTES: &[Route] = &[ $( <$marker as Endpoint>::ROUTE ),+ ];

        /// Build every OpenAPI operation from the catalog. Each endpoint's
        /// body / query / response come from the same tokens as its
        /// [`Endpoint`] impl, so the two cannot drift.
        pub(crate) fn build_operations(
            g: &mut SchemaGenerator,
            err: &SchemaObject,
            string: &SchemaObject,
        ) -> Vec<OperationEntry> {
            let mut out = Vec::new();
            $(
                {
                    let mut op = Operation {
                        operation_id: Some(<$marker as Endpoint>::OPERATION_ID.to_owned()),
                        summary: Some(<$marker as Endpoint>::SUMMARY.to_owned()),
                        ..Default::default()
                    };
                    for name in path_params(<$marker as Endpoint>::PATH) {
                        op.parameters
                            .push(ReferenceOr::Item(path_param(name, string.clone())));
                    }
                    op_query!(g, op, $query);
                    op_body!(g, op, $body);
                    op.responses = Some(op_resp!(
                        g,
                        $shape,
                        <$marker as Endpoint>::SUCCESS_STATUS,
                        $resp,
                        <$marker as Endpoint>::SUMMARY,
                        err
                    ));
                    out.push(OperationEntry {
                        method: <$marker as Endpoint>::METHOD,
                        path: <$marker as Endpoint>::PATH,
                        operation: op,
                    });
                }
            )+
            out
        }
    };
}

endpoints! {
    ServerStatusEp = Get "/server" "server_status" "Get server status",
        body (), query (), Json ServerStatus;

    ListMachinesEp = Get "/machines" "list_machines" "List machines",
        body (), query (), Json Vec<MachineView>;
    CreateMachineEp = Post "/machines" "create_machine" "Create a machine (job)",
        body CreateMachineRequest, query (), Json JobView<()>;
    GetMachineEp = Get "/machines/{id}" "get_machine" "Get a machine",
        body (), query (), Json MachineDetail;
    DeleteMachineEp = Delete "/machines/{id}" "delete_machine" "Delete a machine (job)",
        body (), query (), Json JobView<()>;
    CloneMachineEp = Post "/machines/{id}/clone" "clone_machine" "Clone a machine (job)",
        body CloneMachineRequest, query (), Json JobView<()>;

    SetModulesEp = Post "/machines/{id}/modules" "set_modules" "Set a machine's modules",
        body SetModulesRequest, query (), Empty ();
    ListSecretsEp = Get "/machines/{id}/secrets" "list_secrets" "List declared secret keys",
        body (), query (), Json Vec<SecretKey>;
    GetSecretEp = Get "/machines/{id}/secrets/{key}" "get_secret" "Get a secret value",
        body (), query (), Json String;
    SetSecretEp = Post "/machines/{id}/secrets/{key}" "set_secret" "Set a secret value",
        body SetSecretRequest, query (), Empty ();
    DeleteSecretEp = Delete "/machines/{id}/secrets/{key}" "delete_secret" "Delete a secret",
        body (), query (), Empty ();

    RebuildEp = Post "/machines/{id}/rebuild" "rebuild" "Rebuild a machine (job)",
        body (), query (), Json JobView<Rebuilt>;
    UpdateEp = Post "/machines/{id}/update" "update" "Update a machine (job)",
        body (), query (), Json JobView<Updated>;
    ListGenerationsEp = Get "/machines/{id}/generations" "list_generations" "List machine generations",
        body (), query (), Json Vec<GenerationView>;
    ActivateGenerationEp = Post "/machines/{id}/generations/{generation}/activate" "activate_generation" "Activate a generation (job)",
        body (), query (), Json JobView<()>;

    ListStoreGenerationsEp = Get "/store/generations" "list_store_generations" "List store generations",
        body (), query (), Json Vec<StoreGenerationView>;

    ResolveConfigEp = Post "/resolve-config" "resolve_config" "Resolve a flake's modules (job)",
        body ResolveConfigRequest, query (), Json JobView<ConfigResolution>;
    PrepareExecEp = Post "/machines/{id}/exec" "prepare_exec" "Prepare an exec session (job)",
        body PrepareExecRequest, query (), Json JobView<ExecPlan>;

    GetJobEp = Get "/jobs/{id}" "get_job" "Get a job",
        body (), query (), Json JobView;
    CancelJobEp = Post "/jobs/{id}/cancel" "cancel_job" "Cancel a job",
        body (), query (), Empty ();
    StreamJobEventsEp = Get "/jobs/{id}/events" "stream_job_events" "Stream job events (NDJSON)",
        body (), query EventStreamOpts, Ndjson Event;

    DoctorEp = Get "/doctor" "doctor" "Get cached doctor findings",
        body (), query DoctorOpts, Json DoctorReport;
    DoctorScanEp = Post "/doctor/scan" "doctor_scan" "Run a doctor scan (job)",
        body DoctorOpts, query (), Json JobView<DoctorReport>;
    DoctorFixEp = Post "/doctor/findings/{id}/fix" "doctor_fix" "Fix a finding (job)",
        body (), query (), Json JobView<()>;

    MigrationPlanEp = Get "/migration/plan" "migration_plan" "Get the beta migration plan",
        body (), query (), Json MigrationPlan;
    MigrationRunEp = Post "/migration/run" "migration_run" "Run beta migration (job)",
        body MigrationOpts, query (), Json JobView<MigrationSummary>;
}

/// Look up a route by its operation id.
pub fn route(operation_id: &str) -> Option<&'static Route> {
    ROUTES.iter().find(|r| r.operation_id == operation_id)
}
