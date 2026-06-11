# Phase 1 — Podman Store Architecture (locked decisions)

Locks the store-container design for the Linux/Podman slice. **Revises the store
parts of `01-http-vertical-slice.md` D11 and C6** — the C6 implementation (a
`PodmanStore` that writes a github-pointing `flake.nix` and lets the container
self-provision via `nix profile install`) is superseded by the model below and is
to be reworked. Changes to this file require an explicit revision (same rule as
`00-contract-decisions.md`).

Grounded in a survey of the working beta on `master` (the WSL/LXD store drivers
in `codchi/src/platform/`; the Podman impl on `server` never ran) and the Nix
store image in `nix/container/`.

## Background: how the beta store works (so the revision is honest)

A container always runs exactly one PID 1; for the store that is the Nix-built
bash `/sbin/init` (`nix/container/store/default.nix`). On `master` it runs five
stages: **filesystem** (mkdir, mounts) → **ssl** → **runtime** (the heavy part)
→ **files** → **services** (`nix daemon`, foreground, keeps the container alive).

The store ships in two layers:

- **Bootstrap** — a ~14 MB image (`store-podman-image`) of *static* binaries
  deployed as flat dereferenced files: `nix` (`nix-everything-static`, ~30 MB
  uncompressed), `bash`, `busybox`, `ndd` (685 KB), plus `/etc` and `/sbin/init`.
  `createFiles` copies these out of the Nix store and **drops the store closure**,
  which is why the image is tiny.
- **Runtime** — `git` + `openssh` + `coreutils` (the `runtimePackages`), *not* in
  the image. The **runtime** stage installs them at container start via
  `nix profile install --profile /config/store/profile <flake>`, where the flake
  is host-written into `/config/store/flake.nix` pointing
  `inputs.codchi.url = github:aformatik/codchi/<rev>`. This is the ~72 MB
  first-init download, substituted from cache.

`files.*` is double-duty: the same definitions are materialized into the image
tar at build time **and** re-materialized by `create-files` on each init/update
(dereferencing each `build-hierarchy` symlink out of `/nix` into the container
root, pruning files dropped since last run).

### Why this is wrong for v1

1. **Dev ≠ release.** The runtime is built/substituted for `github:…/<rev>`, which
   never matches a dirty local tree. Development gets a stale store.
2. **Split-brain orchestration.** Half the logic is host Rust, half is Nix-built
   bash inside the container; neither side owns it and the bash is untestable.
3. **No clean manual bring-up.** Nothing brings the store to "nix-daemon usable"
   without the full provisioning dance.
4. **The host cannot be assumed to have Nix** (only when developing codchi). So
   "deliver the runtime closure via `nix copy` from the host" is a dev-only trick,
   not a production option. Any Nix build/substitution must happen *inside the
   container* or be *prebuilt when codchi itself is built*.

A measured aside: `store-podman.config.build.runtime`'s **closure** is 3.1 GB
(1.8 GB of which is a phantom Rust-toolchain reference embedded in `ndd`, plus
156 MB of nix `-dev` outputs). This never reaches the deployed container because
`createFiles` dereferences — but it *would* if the runtime were delivered as a Nix
closure (substitution/import). It is a decisive argument against closure delivery
and in favor of **tar/file delivery**.

## Core decisions

- **S1 — The store is image-defined and fully self-contained.** The store
  container is defined *entirely* by a single Nix-built image: bootstrap **plus**
  all runtime tools, all static, all baked at codchi-build time. There is no
  runtime provisioning step, no host-written `flake.nix`, no `nix profile
  install`, no github fetch, no `create-files` at init. A bare
  `podman run <store-image>` reaches "nix-daemon usable" with **zero** server
  involvement — the manual-bring-up requirement holds by construction.

- **S2 — Delete the provisioning subsystem.** Removed from the store path:
  the host-written store `flake.nix`; the in-container `git init` / `nix profile
  install` / `nix flake update` / `nix profile upgrade`; the **runtime** init
  stage; `create-files` as a *runtime* step (it stays a *build-time* step that
  bakes `/etc`, `/sbin/init`, … into the image). The C6 `PodmanStore::prepare`
  /`write_store_flake` and `CODCHI_FLAKE_URL` are dropped.

- **S3 — Minimal static runtime, shipped in the image.** Empirically (nix 2.34,
  strace, `git`/`ssh` absent from PATH), a nix flake build needs **almost no
  external binaries** — fetching is libcurl/libgit2/libssh2 (linked into nix) and
  realizing a derivation uses only store-path tooling from cache:

  | scenario | external binary exec'd |
  |---|---|
  | `github:`/tarball input | none (libcurl + libarchive) |
  | `git+https` input | none (**libgit2**) |
  | `git+ssh` input | none (libssh2; no `ssh` exec) |
  | real build (eval + substitute) | none (store-path builder + inputs) |

  So the store ships **static `nix` only** as its substantive runtime, plus the
  bootstrap (`busybox`/`bash`/`ndd`). **All three** beta `runtimePackages` —
  `git`, `openssh`, `coreutils` — are **dropped from the store image**, because
  the machine-build need for them is met *without* a store-image binary (S3a):
    - **`git`/`ssh` for remote inputs** → nix's built-in libgit2/libssh2
      (`git+https`/`git+ssh` fetch in-process; verified no `git`/`ssh` exec).
    - **codchi's own wrapper flake** → built via **`path:`** with a pre-written
      `flake.lock`, clean by construction, so it needs no `git`. Verified: a
      `path:` flake with a `github` input evaluates, locks, and builds with no
      `git` on PATH. (A *user's* referenced **dirty** local config does shell out
      to `git` — supplied on demand per S3a, not baked.)
    - **`git`/`ssh` for build-time fetches** (`fetchgit`, `fetchFromGitHub`) →
      the **FOD's own** `git`/`ssh` as store-path `nativeBuildInputs`, substituted
      from cache — independent of the store image.
    - **`openssh` binary** → only for remote builders / `ssh-ng://` stores, which
      the store does not use. **`coreutils`** → nix never execs it; busybox covers
      the init script.

  Resulting image ≈ **~33 MB uncompressed (~14 MB compressed)** = `nix` ~30 +
  bootstrap ~3, replacing *14 MB ship + 72 MB network* with a ~14 MB ship and
  **no network at init**. Network at *build-a-user-machine* time stays (their
  flake inputs + cache), via nix's built-in fetchers — what is removed is fetching
  **codchi's own store definition**.

- **S3a — `git`/`ssh` are left out of the store; provided on demand for machine
  builds.** They are not in the store image/closure/tar at all. When a machine
  build genuinely needs them — e.g. a **dirty local config tree** at eval time
  (verified: with `git` off PATH, `nix eval git+file://<dirty>` fails with
  "program git failed") — `codchi-server` runs the build with them on PATH via
  `nix shell nixpkgs#git nixpkgs#openssh --command nix build …` (or equivalent).
  They are substituted from cache into the store's `/nix` on first use and cached
  thereafter. So dirty local configs are fully supported without baking `git` into
  the store, and the store image stays minimal (S3). codchi's *own* ephemeral
  wrapper flake is clean by construction (`path:` + pre-written lock); only the
  *user's* referenced local config might be dirty, and that path gets `git` from
  `nix shell`.

- **S4 — One artifact (the store tar) for create, update, *and* repair; the
  store's `/nix` persists.** There is a single store image/tar, applied **over the
  container's filesystem** — we never generate the same files a second time
  through a separate update path. The store version is pinned to the installed
  codchi version (new codchi → new `CODCHI_PODMAN_STORE_IMAGE`). The apply
  primitive is platform-shaped but the artifact is identical:
    - **Podman** — recreate `codchi-store` from the new image. The **`/nix` store
      volume** (machine build outputs + nix db) is a *named volume* separate from
      the container, so it **survives** recreation; the build cache persists.
    - **WSL** — keep `/nix` in a dedicated persistent ext4 VHD, separate from the
      disposable store distro VHD. Before launching the store distro,
      `codchi-server` runs
      `wsl.exe --mount <codchi-nix.vhdx> --vhd --name codchi-nix`; the store init
      then bind-mounts `/mnt/wsl/codchi-nix` at `/nix` before starting
      `nix daemon`. Direct `--vhd` attachment works without administrator access
      (unlike physical-disk pass-through). WSL installs the shared `/mnt/wsl`
      mount before running the distro init, provided the mount command completes
      before the distro is launched. Do not put this bind in `/etc/fstab`: WSL
      processes fstab before moving the cross-distro share into `/mnt/wsl`.
      Create/update/repair therefore unregisters and re-imports the disposable
      store distro from the same tar while leaving `codchi-nix.vhdx` untouched.
      `wsl --shutdown` clears active disk attachments, so server startup always
      idempotently verifies/mounts the VHD before launching the store.
  No in-container nix-profile generations/rollback for the store; rollback =
  re-apply the previous image/tar. This is also the `auto_fixable` startup repair
  path (`00-contract-decisions.md` P6): re-applying system files is in scope;
  user data and the `/nix` build cache are untouched.

- **S5 — The in-container PID-1 init is thin: prepare + start `nix-daemon`.**
  It does platform filesystem prep (Podman: minimal mkdir; WSL: bind-mounts +
  bridge — see open items) → SSL → `exec nix daemon` (the foreground keep-alive).
  No provisioning, no file deployment, no network. Whether this stays Nix-built
  bash or becomes a small static Rust init bin is **open (S9)**.

- **S6 — State lives on the host (SQLite) and in volumes; nix files are
  ephemeral.** The store keeps **no durable host config tree** of its own. The
  `/nix` store is a Podman named volume or a dedicated WSL ext4 VHD (S4).
  Per-operation machine nix files (`flake.nix`/`flake.lock`) are written by
  `codchi-server` per invocation into ephemeral locations (consistent with the
  contract's ephemeral-nix-files rule), not persisted under `~/.config/codchi`.
  `/data` (logs) is captured by the C7 source-log mechanism rather than relying
  on a host-bind-mounted log tree. The beta's `~/.config/codchi` +
  `~/.local/share/codchi` host trees are **not** reused wholesale; what survives
  is justified case-by-case.

- **S7 — dev == release by construction.** The image is built from the local tree
  (`store-podman-image`), its path baked into `codchi-server` as
  `CODCHI_PODMAN_STORE_IMAGE` (already the case). Because nothing is fetched at
  runtime, a dev build and a release build of the store differ only in their
  source tree — never in *mechanism*. No dev-only code path, no `nix copy`
  host→container, no substituter juggling.

## Open items / next branches

- **S8 — `ndd`'s Rust-toolchain closure leak — FIXED.** The `rust-src` toolchain
  extension baked std panic-location source paths
  (`…rust-default…/lib/rustlib/src/…`) into `ndd` as string data; stripping leaves
  them, so Nix retained a ~1.8 GB reference to the whole toolchain. Fixed in
  `crates/codchi-container-utils/default.nix` with `remove-references-to -t
  ${rust}` in `postInstall` plus `disallowedReferences = [ rust ]` as a guard. The
  paths are display-only. Result: `ndd` closure 1.8 GB → **0.65 MB, zero
  references**; `store-podman` runtime closure 3,119 MB → **940 MB**.
- **S9 — Init implementation: bash for Podman now.** Decided: the thin PID-1 init
  (prep + `nix daemon`) stays **Nix-built bash** for the Podman slice — it is too
  trivial to justify an abstraction. The unifying **static Rust `codchi-store-init`**
  (shared with `codchi-machine-init`, testable, carries the WSL bind-mount/bridge
  logic) is the right move once WSL lands; that approach is noted in
  `v1/04-platform-boot-exec.md` for the WSL phase (13) to pick up.
- **S10 — WSL unification (deferred to Phases 12/13).** Mirror the
  `codchi-machine-init` → `codchi-hostctl.exe` → `codchi-server` boot-spec pattern
  for the store where it helps; WSL bind-mounts/bridge stay in WSL init. On the
  WSL keep-alive: a static `daemonize` **exists** (`nix/overlays/daemonize-static.nix`
  pre-seeds the Autoconf `setpgrp` probe result that breaks static
  cross-compilation; gated as `packages.daemonize-static`), **but it is not
  needed** — because `codchi-server` is a persistent daemon, it simply keeps the
  launching `wsl.exe` subprocess alive, which keeps the store distro up. No
  in-distro daemonization of `nix daemon` is required.
- **S11 — Build-time caution for what goes *in* the tar.** Because the tar is a
  flat dereference (closure dropped), any tool baked in must be self-contained:
  static binaries are fine (nix/busybox/bash/ndd), but a package's aux *scripts*
  with `/nix/...` shebangs (e.g. `git-cvsserver`, `ssh-copy-id`) would dangle.
  Not applicable to the chosen design — `git`/`ssh` are **not** baked (S3a
  provides them via `nix shell`, with their full closure on PATH, so their
  shebangs resolve). Keep this in mind only if future work bakes a script-bearing
  package into the store tar.

## Impact on the C6 changes already on `server`

The current diff (`PodmanStore`, `StoreManager`, `store_manager.rs`,
`platform/{store,podman}.rs`) keeps its **lifecycle skeleton** — the
`Starting → Healthcheck → Ready/Degraded` drive, the `Arc<dyn Store>` seam, the
15 s observe-only sentinel, the `store.unavailable` finding/recovery — but the
**provisioning bits are removed**: `prepare`/`write_store_flake`,
`CODCHI_FLAKE_URL`, and the assumption that the container self-installs from a
flake. `register` creates the container from the image with the `/nix` volume
(S4); `start`/`probe_health`/`stop` stay; `status` stays. The Nix changes
exposing `packages.store-podman` (the runtime closure) are no longer needed for
delivery and can be dropped or repurposed for S8 measurement only.
