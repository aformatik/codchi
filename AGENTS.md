# Agent Entry Point

Codchi is being reimplemented from the beta CLI-owned architecture to a v1
architecture with a central, per-user `codchi-server` and SQLite-owned state.
The repo is mid-migration: the `server` branch has the v1 contract and clean
crate scaffolding, but end-user behavior has not yet been ported from beta.

Read these before making changes:

- **[v1/README.md](v1/README.md)** — scope, core decisions, non-negotiable invariants.
- **[v1/STATUS.md](v1/STATUS.md)** — current migration state, per-phase progress.
- **[v1/PLAN.md](v1/PLAN.md)** — phased delivery plan, estimates, acceptance criteria.
- **[v1/01-architecture.md](v1/01-architecture.md)** through **[v1/06-api-endpoint-codegen.md](v1/06-api-endpoint-codegen.md)** — general target-architecture design docs.
- **[v1/phases/](v1/phases/)** — per-phase locked specs: Phase 0 contract decisions (incl. R11), Phase 1 vertical-slice spec.
- **[CONTEXT.md](CONTEXT.md)** — glossary of canonical terms (log source, job, subject, the crate set).

## Crates

The active v1 workspace (members in `crates/Cargo.toml`):

- `codchi-api` — the v1 wire contract: DTOs, error catalog, `Event` model, the
  `CodchiService` trait, the typed `Endpoint` route catalog, OpenAPI. The single
  source of truth; depends on neither `axum` nor `reqwest`. **Frozen** — changes
  go through an explicit `v1/phases/` revision (see R11).
- `codchi-server` — the v1 daemon (new).
- `codchi-cli` — the v1 CLI (new). Package `codchi-cli`, binary `codchi`.
- `codchi-shared` — non-wire common code (new; lean — pulled forward as needed).
- `codchi-container-utils` — `ndd`, the in-store `nix` wrapper baked into the
  store image (renamed from `utils`).

The beta (CLI-owned-state) crates are retired under `crates/beta/` with a
`beta-` prefix (`beta-codchi`, `beta-codchi-server`, `beta-ipc`, `beta-shared`,
`beta-codchi-gui`, `beta-codchiw`): **frozen, read-only reference, not compiled**
(workspace-excluded). Read them for orientation — especially the Podman store
work in `beta-codchi-server` — but never link them.

> Status: Phase 1 chunks **C0–C4** are complete. The beta crates are archived and
> the five crates above are the complete active workspace. `codchi-server` serves
> the full typed API over a per-user Unix socket and `codchi-cli` has a typed
> client, but **against `MockCodchiService`** — the real Podman store lifecycle
> (C6), source-log capture (C7), and `codchi status` / daemon spawn (C5) are not
> built yet, so daemon behavior is still mock data over real transport.

## Rules for agents

- Treat `v1/` as the source of truth for the target architecture. If your
  change contradicts a design doc, update the doc in the same change — don't
  let code and docs drift.
- When you complete or advance a phase, update [v1/STATUS.md](v1/STATUS.md)
  and bump its `Last updated` date.
- When a locked decision in `v1/phases/` needs to change, revise the spec
  explicitly; do not silently diverge in code.
- `v1/PLAN.md` is the stable delivery plan — only edit it when phase scope or
  acceptance criteria genuinely change, not for progress updates (use STATUS).
- The v1 design is a target, not the current implementation. Don't assume a
  doc-described component exists in code without checking.
