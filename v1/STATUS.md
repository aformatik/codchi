# Codchi v1 Migration Status

Tracks the current state of the v1 reimplementation. This file is expected to
change often. Stable phase definitions live in [PLAN.md](PLAN.md); locked
per-phase specs live under [phases/](phases/).

**Last updated:** 2026-06-09 (Phase 1 spec locked in `phases/01-http-vertical-slice.md`; Phase 0 contract revised by **R11/CR1** — logs are source-keyed and jobs are subject-tagged: `JobView.machine`→`subject: LogSource`, new `LogSource`/`LogSourceKind`/`JobFilter`, `JobKind::StoreStart`/`StoreRecover`, `list_jobs` + `stream_logs` endpoints; `codchi-api` rebuilt + 10 contract tests green + `openapi.json` regenerated)

## Overall

The repo is on the `server` branch, an early skeleton of the v1 architecture.
The beta architecture (CLI-owned state) still drives end-user behavior. The v1
target is described in [01-architecture.md](01-architecture.md) and
[README.md](README.md).

## Phase Status

| # | Phase | Status |
|---|---|---|
| 0 | Contract design (`codchi-api` crate) | Done — decisions locked + refined (R1–R10) in `phases/00-contract-decisions.md`; both former open items closed (R8/R9/R10). **Crate created**: `crates/codchi-api` compiles with all DTOs, IDs, `ApiError` catalog, `Event` model, `CodchiService` trait + typed `Endpoint` catalog, `MockCodchiService`, `schemars`+`aide` OpenAPI generation (`gen-openapi` bin), committed `openapi.json`, and contract tests (error-code stability, OpenAPI/route coverage, endpoint-catalog consistency, mock smoke, plus existing serde roundtrips). `JobView` is generic over its success payload (`JobView<O = JobOutput>`): kind-specific methods return narrowed views (`JobView<Rebuilt>`, `JobView<()>`, …), `get_job` returns the kind-erased default; `JobOutput` variants are newtypes over the same payload structs (revised R1). Path params are the canonical identity and are no longer duplicated in request bodies (`rebuild`/`update` take no body; `clone` body is `{ target }`; `exec` body is `{ command }`). URL map is a typed `Endpoint` catalog (`endpoints.rs`, one marker per route) driving routing + OpenAPI; `operation_id`-string dispatch removed (see `06-api-endpoint-codegen.md`). **Deactivation + CI**: the product crates are excluded from the cargo workspace (kept on disk as reference) so the contract crate builds/tests in isolation; CI gates are wired as hermetic nix checks — `checks.codchi-api` (clippy `-D warnings` + `cargo test` + OpenAPI snapshot-drift), `checks.formatting` (treefmt: rustfmt edition-2024 + nixpkgs-fmt), and an `oasdiff` breaking-change gate (`packages.oasdiff`, built from `build/oasdiff.nix`) in `.github/workflows/ci.yml`. Next: Phase 1 generic router/client. |
| 1 | HTTP API + Linux/Podman vertical slice | Spec locked (`phases/01-http-vertical-slice.md`, D1–D13); contract revision **R11/CR1** applied to `codchi-api`. Implementation not started — beta `codchi-server` skeleton still on disk pending the `crates/beta/` restructure |
| 2 | `ServerCore` boundary | Not started |
| 3 | SQLite foundation | Not started |
| 4 | Machine state in SQLite | Not started |
| 5 | Job system MVP | Not started |
| 6 | Build/update generation model | Not started |
| 7 | Linux/Podman machines | Not started |
| 8 | Exec/session model | Not started |
| 9 | Logs/events | Not started |
| 10 | Doctor + recovery findings | Not started |
| 11 | Beta migration | Not started |
| 12 | Windows/WSL server backend | Not started |
| 13 | WSL boot/session components | Not started |
| 14 | CLI parity pass | Not started |
| 15 | Tray client | Not started |
| 16 | Test suites + CI | Not started |
| 17 | Hardening / release | Not started |

## Notes

- The `codchi-api` shared crate now exists (`crates/codchi-api`). HTTP/DTO work
  in `codchi-server` and `codchi` should now build on it (the existing `ipc`
  types remain provisional and still need to be reconciled with / replaced by
  the `codchi-api` contract). Dependency choices: `schemars` 0.8 + `aide` 0.14
  (axum optional, disabled here), `chrono`, `uuid` v7, `async-trait`,
  `futures-core`.
- **Workspace deactivation (Phase 0):** `crates/Cargo.toml` now lists only
  `codchi-api` as a workspace member; the product crates (`codchi`, `codchiw`,
  `codchi-server`, `codchi-gui`, `shared`, `ipc`, `utils`) are in `exclude` and
  kept on disk as reference. The trimmed `[workspace.dependencies]` keeps only
  `codchi-api`'s deps; restore the rest when reactivating a crate. The nix
  product builds (`packages.default`/`.windows`, container tarballs) and the
  `populate-cache` product inputs are commented out until reactivation; the
  release workflow (`build-windows.yml`, tag-triggered) is unchanged and will
  fail until the product compiles again.
- **CI (Phase 0):** `.github/workflows/ci.yml` runs on push (`master`/`server`)
  and PRs: hermetic `checks.codchi-api` + `checks.formatting`, plus an
  `oasdiff` breaking-change gate on PRs. `nix-cache.yml` warms cachix from the
  trimmed `populate-cache`. `test.yml` is now only the post-release Windows
  Pester smoke. Per guidance, serde serialization-roundtrip coverage is left as
  the existing in-crate tests and not expanded (serde is trusted on its own).
- Machine state is not yet server-owned; the CLI still manipulates it directly.
- Tray client is currently disabled.