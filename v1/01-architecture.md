# Architecture

## Target Shape

Codchi v1 has a central user-level server:

```text
CLI / tray
  -> codchi-api typed HTTP client
  -> codchi-server
  -> SQLite state
  -> platform drivers
  -> Podman / WSL / Nix
```

The server owns Codchi configuration, machine definitions, generations,
migrations, jobs, health findings, and platform reconciliation.

The CLI and tray are clients. They should not rebuild their own view of machine
state from files and subprocess calls.

## Daemon Lifecycle

`codchi-server` is on-demand per user:

1. A CLI or tray command attempts to connect.
2. If no server is reachable, the client starts `codchi-server`.
3. The server initializes its state, opens SQLite, performs health checks, and
   becomes ready.
4. The server remains running in the background.

The server should have an explicit lifecycle state:

```text
Starting
Healthcheck
Ready
Degraded
Stopping
```

There is no `Migrating` state: the daemon runs its (sub-millisecond) schema
migrations synchronously *before* it serves, so a client never observes a
mid-migration daemon (`v1/phases/03-sqlite-foundation.md`, DB8). A failed or
too-new schema surfaces as `Degraded` + a structured startup error, the same as
a failed store start.

Clients must not hang forever when startup fails. Startup failures should
transition to `Degraded` or return a structured startup error.

## API Transport

Use HTTP/JSON for v1.

Reasons:

- Easier versioning and diagnostics than Rust-only RPC.
- Easier CLI/tray integration.
- Easier testing with normal HTTP tools.
- Better fit for Docker/Podman-style local daemon APIs.
- OpenAPI can document and validate the internal contract.

`remoc` is useful for prototypes and same-version Rust RPC, but it should not be
the v1 foundation.

## Rust Server Framework

Use `axum`.

The server should keep one shared state handle:

```rust
#[derive(Clone)]
pub struct AppState {
    pub core: Arc<ServerCore>,
}
```

Handlers should be thin adapters. They should parse HTTP requests, call
`ServerCore`, and return typed responses. They should not contain lifecycle or
mutation state machines.

## API Source of Truth

Create a `codchi-api` crate.

Responsibilities:

- Request and response DTOs.
- Error codes and machine-readable error responses.
- Event DTOs for logs, progress, jobs, health findings, and notifications.
- A semantic service trait used by server and typed clients.
- A typed HTTP client used by CLI and tray.
- OpenAPI generation and CI checks.

The semantic trait should express Codchi operations:

```rust
#[async_trait::async_trait]
pub trait CodchiService {
    async fn list_machines(&self) -> Result<Vec<MachineView>, ApiError>;
    async fn prepare_exec(&self, req: PrepareExecRequest) -> Result<ExecPlan, ApiError>;
    async fn start_rebuild(&self, req: RebuildRequest) -> Result<JobView, ApiError>;
    async fn cancel_job(&self, id: JobId) -> Result<(), ApiError>;
}
```

`ServerCore` implements the trait directly. The HTTP client also implements the
same trait by making HTTP calls.

The URL map is a typed endpoint catalog (one marker type per route implementing
an `Endpoint` trait), the single source of truth reused by the router, the typed
client, and OpenAPI. See [06 — API Endpoint Catalog & Client/Server Wiring](06-api-endpoint-codegen.md).

## API Stability

The HTTP API is internal. It should be stable enough for Codchi's own CLI and
tray across normal releases, but it is not a public external API commitment for
v1.

The CLI behavior is the public compatibility surface.

Machine-readable CLI output and error codes should be treated carefully because
users may script them.

## Security Boundary

The API is internal, but local access still matters because it can control
machines and read plaintext secrets.

Open design details:

- Unix socket versus localhost TCP on Linux. **Decided (Phase 1, D6):** per-user
  Unix domain socket, chosen so it can later bind-mount into machines. See
  [phases/01-http-vertical-slice.md](phases/01-http-vertical-slice.md).
- Named pipe versus localhost TCP on Windows. (Open — Phase 12.)
- Per-user socket permissions. (Open — Phase 17.)
- Local auth token or capability file. (Open — Phase 17.)
- Restrictions for tray/CLI only. (Open — Phase 17.)

