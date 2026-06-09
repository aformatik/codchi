# Agent Entry Point

Codchi is being reimplemented from the beta CLI-owned architecture to a v1
architecture with a central, per-user `codchi-server` and SQLite-owned state.
The repo is mid-migration: the `server` branch carries an early skeleton, but
end-user behavior is still driven by the beta path.

Read these before making changes:

- **[v1/README.md](v1/README.md)** — scope, core decisions, non-negotiable invariants.
- **[v1/STATUS.md](v1/STATUS.md)** — current migration state, per-phase progress.
- **[v1/PLAN.md](v1/PLAN.md)** — phased delivery plan, estimates, acceptance criteria.
- **[v1/01-architecture.md](v1/01-architecture.md)** through **[v1/05-testing.md](v1/05-testing.md)** — general target-architecture design docs.
- **[v1/phases/](v1/phases/)** — per-phase locked specs (currently: Phase 0 contract decisions).

The current code lives under [crates/](crates/):

- `codchi` — CLI (beta behavior + early v1 client wiring)
- `codchi-server` — v1 daemon skeleton
- `codchi-gui`, `codchiw` — tray / Windows launcher
- `ipc` — current IPC types (provisional; will move into a v1 `codchi-api` crate)
- `shared`, `utils` — common code

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