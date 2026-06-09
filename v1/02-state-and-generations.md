# State and Generations

## State Model

SQLite is the single source of truth for Codchi configuration and state.

SQLite owns:

- global config
- machine config
- module definitions and ordering
- plaintext secrets for v1
- flake lock content
- store and machine schema versions
- migration state
- jobs
- health findings
- generation metadata
- log indexes and paths

Disk owns external artifacts:

- user data
- WSL instance filesystems
- Podman volumes/containers
- Nix store paths
- generated temporary build directories
- JSONL job logs (the durable event tier only; see R8 in
  [phases/00-contract-decisions.md](phases/00-contract-decisions.md))

Generated artifacts are not authoritative:

- `flake.nix` is always generated.
- `flake.lock` is stored in SQLite and projected to disk only during Nix jobs.
- profiles/gcroots are projections from SQLite generation state.

## Build and Update Flow

Machine rebuild/update uses a temporary flake directory:

```text
$RUNTIME/codchi/jobs/<job-id>/flake/
  flake.nix
  flake.lock
```

Flow:

1. Start a job.
2. Read machine config and current lock from SQLite.
3. Generate `flake.nix` into the temp directory.
4. Write `flake.lock` from SQLite if present.
5. Run `nix flake lock` or `nix flake update` only if requested.
6. Build from the temp flake.
7. If build and switch succeed, store the resulting lock in SQLite.
8. Record a new generation.
9. Update the active generation pointer.
10. Remove temp files on success.

Failed updates do not advance the stored lock. Candidate locks may be preserved
in job logs or preserved temp directories for inspection, but they are not
committed as machine state.

## Generations

All machine and store generations should be recorded in SQLite.

Machine generation metadata should include:

- machine id
- generation number
- config snapshot
- flake lock content/hash
- build job id
- system store path
- created timestamp
- activated timestamp
- active/protected/deleted status

Store generation metadata should include:

- store generation number
- runtime store path
- flake lock content/hash
- created timestamp
- activated timestamp
- active/protected/deleted status

For v1, store generations are exposed **read-only** through the API
(`list_store_generations`, see
[phases/00-contract-decisions.md](phases/00-contract-decisions.md)).
Store-generation activation and garbage collection are internal to
`codchi-server`; clients cannot rollback or activate them via the API.

## Profiles and Gcroots

Profiles and gcroots should be generated from SQLite generation records.

They are cheap projections, but they must be persistent when they protect
rollbackable generations from garbage collection.

Example shape:

```text
/data/gcroots/machines/<machine>/<generation> -> /nix/store/...
/runtime/profiles/machines/<machine>/system -> /nix/store/active-system
```

The exact paths must be chosen per platform so they are visible from the store's
GC perspective.

`codchi doctor` should reconcile:

- generation in DB but missing gcroot
- gcroot without DB generation
- active generation missing profile
- DB generation pointing to missing store path
- active generation pointing to missing store path

## Migration

Introduce a migration system with per-store and per-machine versions.

Beta machines have no version. They can be detected by existing config files and
container naming conventions.

Migration should support:

- `codchi migrate --dry-run`
- `codchi migrate --json`
- explicit migration actions
- backups of beta config files
- idempotent step execution

Migration imports:

- global `config.toml`
- per-machine `config.json`
- existing `flake.lock`
- existing platform/container metadata where available

Backups are kept. Do not delete beta config files after migration.

Open detail:

- Exact backup path naming.
- Whether migrated beta files remain in place with a marker or are moved to a
  backup folder.
- Exact migration graph.
