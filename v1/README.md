# Codchi v1 Design Notes

This folder captures the working design for moving Codchi from the beta CLI-owned
architecture to a v1 central-server architecture.

These notes are not website documentation. They are contributor design notes meant
to be revised in later sessions.

## Scope

The v1 target is a public beta replacement with stable CLI behavior. The daemon
API is internal to Codchi clients, not a public semver-stable API.

Required v1 clients:

- CLI
- tray

Required v1 platforms:

- Linux with Podman
- Windows with WSL

Linux LXD support is not part of v1.

## Design Index

General design docs (target architecture, not current state):

- [01 Architecture](01-architecture.md)
- [02 State and Generations](02-state-and-generations.md)
- [03 Jobs, Logs, and Doctor](03-jobs-logs-doctor.md)
- [04 Platform Boot and Exec](04-platform-boot-exec.md)
- [05 Testing](05-testing.md)
- [06 API Endpoint Catalog & Client/Server Wiring](06-api-endpoint-codegen.md)

## Delivery Plan

[PLAN.md](PLAN.md) breaks the work into numbered phases with estimates and
acceptance criteria.

## Phase Specs

Per-phase locked specs live under [phases/](phases/). Each one freezes the
inputs for that phase; changes require an explicit revision of the spec.

- [Phase 0 — Contract Decisions](phases/00-contract-decisions.md)

## Core Decisions

- Use a central, on-demand, per-user `codchi-server`.
- Keep the server running in the background after first use.
- Use HTTP/JSON for the internal API instead of `remoc`.
- Use `axum` for the HTTP server.
- Create a shared `codchi-api` crate for DTOs, errors, semantic service traits,
  typed clients, and generated OpenAPI.
- Use SQLite as the single source of truth for Codchi configuration and state.
- Store `flake.lock` content in SQLite; generate `flake.nix` and temporary
  `flake.lock` files only during build/update jobs.
- Use rolling JSONL logs on disk, indexed by SQLite.
- Keep secrets plaintext for v1. (Probably)
- Introduce explicit migrations with per-store and per-machine versions.
- Beta machines have no version and are detected/imported by migration.
- Back up beta config files during migration.
- Long-running operations are jobs with cancellation and log streaming.
- Conflicting mutating jobs are rejected, not queued.
- Explicit cancellation cleans up immediately.
- Failed or crashed jobs preserve data/artifacts for inspection and recovery.
- User data must never be deleted or corrupted by any operation, except when the user explicitly requests it (e.g. when deleting a machine).

## Non-Negotiable Invariants

- User data has priority over automatic cleanup.
- A failed server, failed boot, failed migration, or failed recovery must not
  delete user data.
- Operations commit SQLite state only at safe boundaries.
- Failed updates do not advance the stored `flake.lock`.
- Windows WSL boot failures must use singular deduplicated error reporting, not
  repeated init-script popups. Source-side dedupe is impossible because
  `codchi-machine-init` is short-lived; dedupe lives on the receiver
  (`codchi-hostctl` + `codchi-server`).
- Only the daemon controls store startup and recovery.
- Automatic repair is bounded to **server startup only**, and only for
  findings flagged `auto_fixable` (no user data, no rewrite of the stored
  `flake.lock`; system-file portions of store and machine distros are in
  scope). The background reconciler is observe-only. After startup, repair
  is exclusively user-invoked via `doctor_fix`.

## Current Branch Context

The current `server` branch is an early skeleton:

- `codchi-server` starts a store and exposes readiness/log streaming.
- The IPC API only has readiness and log streaming.
- Machine state is not yet server-owned.
- The CLI currently connects to the server and then reaches unreachable command
  dispatch code.

The v1 design below is a target architecture, not a description of the current
implementation state.
