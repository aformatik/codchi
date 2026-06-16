# Phase 6 — Build/Update Generation Model (decisions)

Owns temp flake workspaces, SQLite-owned `flake.lock`, safe commit boundaries,
generation records, the active-generation pointer, and gcroot/profile
projection (PLAN Phase 6).

> Provisional. The diagnostic-workspace decisions below (GM-W1..GM-W3) were
> grilled during the **Phase 4** machine-state session and relocated here
> because the diagnostic workspace *is* the build workspace ("temp flake dirs").
> They are recorded with their open caveats and must be finalized when Phase 6
> is grilled. The job-side API that operates on these artifacts lives in
> [05-job-system.md](05-job-system.md); the wire contract is catalogued in
> [00-contract-decisions.md](00-contract-decisions.md).

## Diagnostic / build workspace

### GM-W1 — The workspace is a job artifact, not machine state

A diagnostic flake workspace is produced by a build/eval **job** and is owned and
cleaned independently of any machine — it is not machine state. With the
simplified model (Phase 4 MS1) a failed create leaves **no machine row at all**,
only this job and its retained artifacts, so workspace lifecycle is wholly
job-scoped. It is materialized by create, rebuild, update, duplicate, and the
read-only `resolve_config`. A job is debuggable exactly when it has a retained
reproducible workspace — capability is determined from the retained artifact, not
a hard-coded `JobKind` list.

Debugging is **post-failure inspection**, not job resumption: the user enters an
interactive environment **inside the store Podman container / WSL distro**, with
the retained workspace as cwd, the shared Nix store available, and the same Nix
config/tool context as the failed command (e.g. `nix build --debugger`).

Cleanup is independent of machine deletion:

- Cleaning a failed rebuild/update/resolve workspace removes only that job's
  retained artifacts; it does not touch the machine, its desired config, stored
  lock, active generation, or platform realization.
- A failed **create** job's cleanup is the only thing tying its workspace to
  platform resources: `delete_job_artifacts` on it removes both the workspace and
  the partial `codchi-machine-<id>` resources (Phase 5 JS-D3), since there is no
  machine row to delete.
- Job metadata and durable events are not workspace artifacts and follow their
  own retention policy.

Because the workspace lives inside the store, debug and cleanup both cross the
platform boundary: the server performs them through the store driver, not by
direct host-filesystem deletion. Workspace cleanup is nevertheless a **bounded
synchronous operation, not a job** — one deletion of a server-owned directory,
no Nix eval/build, no platform lifecycle transition, no user data. This is a
narrow, deliberate exception to Phase-0 Q4's "external reality ⇒ job" rule; it
does not justify making arbitrary platform shellouts synchronous.

### GM-W2 — Workspace path on persistent store backing storage

Canonical root:

```text
/nix/var/codchi/jobs/<job-id>/flake/
  flake.nix
  flake.lock
```

It lives on the store's persistent `/nix` backing storage (the Podman
`codchi-store-nix` volume or the dedicated WSL Nix VHD), so it survives store
container/distro recreation and sits beside the exact Nix store used by the
failed evaluation. Successful jobs remove their workspace; retained failed
workspaces persist until explicit cleanup or the job-artifact retention policy
prunes them. The workspace is **not** a gcroot and does not by itself protect
build outputs.

### GM-W3 — Candidate lock lives in the workspace until commit

For create/rebuild/update, candidate `flake.lock` content stays in the job
workspace while external work runs; it is committed to SQLite only inside the
success transaction (Phase 4 MS9). A retained failed workspace may therefore
hold a candidate `flake.lock` useful for debugging that is not machine state.

## Caveat carried from the Phase 4 grill

- The capability flag surfaced on `JobView` (`has_diagnostic_workspace`) must be
  **durable job state**, not computed by stat'ing the store on each read — that
  would be the synchronous probe P6 forbids. See
  [05-job-system.md](05-job-system.md).
