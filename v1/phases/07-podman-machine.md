# Phase 7 — Podman Machine Container (findings)

How a v1 **machine** runs on Linux/Podman. Grounded in a hands-on bring-up on the
`server` branch (2026-06-12): a real codchi machine (`nixosConfigurations.podman-base`,
the new `podman` driver) booted in rootless Podman off the shared store volume to
`systemctl is-system-running = running`, zero failed units, with the codchi user,
`/home/codchi`, the `/run/wrappers/bin/sudo` setuid wrapper, dbus-broker active, and
X11 GUI to the host (`xdpyinfo` → host `:0`).

Companion to `01-podman-store.md` (the **store** container). Same change rule:
revisions go through an explicit edit here.

## Background: how the beta machine works

The beta ran Linux machines on **LXD** (Model B): `codchi-server` imports a
bootstrap **rootfs tarball** built by `nix/container/machine` (static `/sbin/init`
+ busybox/bash + `run`/`runin`), mounts the shared `/nix/store` and the machine's
profile/home into it, writes `/etc/codchi-env`, and starts it. The tarball's thin
`/sbin/init` runs `/nix/var/nix/profiles/system/activate` then execs systemd. The
tarball exists because LXD/WSL run a **fixed `/sbin/init`** and cannot be pointed
at an arbitrary system path. LXD is out of v1 scope (`PLAN.md`); WSL keeps Model B.

## Core decisions

- **M1 — Model A: boot the NixOS system's own `/init` directly.** A Podman machine
  is a NixOS configuration built (in the store, via its nix-daemon) to a system
  toplevel, run as `podman run … <system>/init`. No base image, no rootfs tarball,
  no `create-files`. Podman *can* be pointed at an arbitrary command, so the
  bootstrap layer that LXD/WSL need is unnecessary. Verified: the system's stage-2
  `init` does activation + execs systemd with only the shared store mounted.

- **M2 — The machine shares the store's `/nix`, read-only.** `/nix/store` is the
  **store container's** named volume (`codchi-store-nix`), mounted into the machine
  read-only via a subpath:
  `--mount type=volume,src=codchi-store-nix,dst=/nix/store,ro,subpath=store`.
  (The store holds it `rw`; concurrent ro mount into machines is fine.) The machine
  never runs its own nix-daemon — `nix-daemon`/`nix-gc` are disabled by the driver;
  builds happen server-side in the store. Distinct from beta, which bind-mounted the
  *host* `/nix/store`.

- **M3 — Codchi features live in the `podman` NixOS driver, not a hand-built init.**
  `nix/nixos/driver/podman/default.nix` (new) + `podman` added to the
  `codchi.driver.name` enum and the flake `drivers` list. It reuses the generic
  driver block (codchi user, secrets/`/etc/codchi-env`, desktop/XDG/icons, gcroots,
  `boot.isContainer`, `hardware.graphics`) and adds:
  - `iconCommand` = copy icons through (Linux host understands freedesktop icons);
  - `environment.extraInit` xauth-cookie registration for X11 (same as LXD);
  - a systemd drop-in (`zzz-podman-service.conf`) relaxing
    `ProtectControlGroups`/`ProtectKernelTunables`/`NoNewPrivileges`/… for rootless
    systemd, mirroring the LXD distrobuilder overrides.
  The Model-B tarball + `create-files` service in `driver/default.nix` were **gated
  to `lxd`/`wsl`** (were: every non-`none` driver); Podman builds neither.

- **M4 — `--cap-add SYS_ADMIN` is required, and is the single fix for two problems.**
  Rootless Podman drops `SYS_ADMIN`. Without it: (a) the NixOS activation `specialfs`
  snippet fails `mount(/proc,/run,/run/wrappers,…)` with EPERM, leaving
  `/run/wrappers` unmounted; (b) **dbus-broker crash-loops** (`launcher_run_child:
  Package not installed` → `service_add: Transport endpoint is not connected`,
  ENOTCONN/-107; visible as defunct `[dbus-broker-lau]`), wedging boot in
  "starting" so even `podman exec` hangs. Bisected empirically: `--privileged` and
  `--cap-add=all` fix it; **`--cap-add AUDIT_WRITE` alone and
  `--security-opt seccomp=unconfined` alone do not**; `--cap-add SYS_ADMIN` alone is
  sufficient. With `SYS_ADMIN`, activation mounts its own specialfs (incl.
  `/run/wrappers`), so the driver needs **no** `boot.specialFileSystems` override and
  the run needs **no** `--mount tmpfs /run` or `/run/wrappers`. (An earlier attempt
  cleared `specialFileSystems` + externally mounted `/run/wrappers`; it booted but
  was more fragile, so it was reverted in favour of plain `SYS_ADMIN`.)

- **M5 — Required Podman options (verified by elimination).**

  | option | role | required |
  |---|---|---|
  | `--mount type=volume,src=codchi-store-nix,dst=/nix/store,ro,subpath=store` | shared store | yes |
  | `--systemd=always` | systemd as PID 1 (default detection does not trigger on `<store-path>/init`; without it systemd exits "container state improper") | yes |
  | `--cap-add SYS_ADMIN` | activation specialfs mounts + dbus-broker (M4) | yes |
  | `--rootfs <dir>` | writable machine root (see M6) | yes |
  | `<system>/init` | boot the active generation | yes |
  | `--env container=podman` | systemd container detection | recommended |

- **M6 — Persistence: the writable root must outlive the container; do NOT abuse a
  named volume as the rootfs.** Every generation switch / store update *recreates*
  the container, so machine state must live outside it. `/nix/store` is the shared
  ro store volume; the writable root (`/etc`, `/var`, `/home/codchi`) is per-machine.
  Two reliable shapes:
    - **(A) codchi-owned rootfs directory** — `--rootfs <data-dir>/machines/<name>/rootfs`
      (a plain directory under codchi's data dir, no `:O`). The whole writable root
      persists across recreation; activation reconciles `/etc` to each new generation.
      `codchi-server` owns create/delete. Closest to beta-LXD whole-instance
      semantics. **Recommended.**
    - **(B) ephemeral overlay rootfs + named DATA volumes** — `--rootfs <empty>:O`
      plus `-v codchi-machine-<name>-home:/home/codchi` (and a decision on
      `/var/lib`). `/etc` and most of `/` are regenerated by NixOS each boot; only the
      data volumes persist. Most Podman-idiomatic (volumes are ref-counted,
      prune-safe, `inspect`-visible, GC-able), but requires enumerating persistent
      paths.

  **Rejected — `--rootfs $(podman volume inspect -f '{{.Mountpoint}}' …)`** (use a
  named volume's `_data` dir as the rootfs). It *works mechanically* (verified: a
  container runs from it; writes survive `--rm`), but Podman does **not** ref-count
  it: with a container actively running on the volume, `podman inspect` shows
  `Mounts: []`, **`podman volume rm` (no `--force`) removes the volume without
  complaint**, and `podman volume prune` would too — silently destroying live
  machine state, and the running container is left on a deleted backing dir. So it is
  a **shaky hack, not reliable**; prefer (A) or (B). (`-v name:/` with an *image*
  triggers a copy-up of the image rootfs into the volume, but Model A has no image,
  and it interacts badly with `/dev`.)

- **M7 — Feature → option map (for the `codchi-server` `podman run`).**

  | feature | option(s) |
  |---|---|
  | boot (PID 1 systemd) | `--systemd=always` `--cap-add SYS_ADMIN` `--env container=podman` |
  | shared Nix store | `--mount …codchi-store-nix…dst=/nix/store,ro,subpath=store` |
  | identity / exec target | `--name codchi-machine-<name>` then `podman exec -it … codchi-session` |
  | active generation | positional `<system>/init`; switch = recreate/restart |
  | user-file persistence | `--rootfs <codchi-owned dir>` (M6 A) — *not* a volume mountpoint |
  | X11 GUI | `-e DISPLAY` `--net=host` `-v /tmp/.X11-unix:/tmp/.X11-unix` `-v $XAUTHORITY:/root/.Xauthority` (driver `extraInit` registers the cookie) |
  | host-file access as user | `--uidmap 1000:0:1 --uidmap 0:1:1000` |
  | GPU | `--device /dev/dri` (+ render gid); driver enables `hardware.graphics` |
  | secrets | `codchi-server` writes `/etc/codchi-env`; driver `extraInit` sources it |

## Argument-set regression suite (M12)

The Podman `run` contract above (M5/M7) is **empirical** — it depends on rootless
Podman and nixpkgs behaviour that drifts between versions (e.g. default capability
sets, dbus-broker internals, the activation `specialfs` snippet, `--systemd`
detection, option/attr renames). `nix/tests/podman-machine-args.sh` pins it so a
Podman/nixpkgs bump that changes a requirement fails loudly. It is a
**scheduled/manual** suite (needs rootless podman + systemd; not a `nix flake
check`), the Podman analogue of the WSL suites in `v1/05-testing.md`.

It runs as a **matrix over NixOS versions** — `NIXPKGS_REFS` (default
`flake nixos-25.05`) builds `podman-base` against each via
`nix build … --override-input nixpkgs github:NixOS/nixpkgs/<ref>`, imports the
closure into the store volume, and runs every group per version. The NixOS axis is
the one most likely to move the contract; the host `podman --version` is recorded
so a failure is attributable to either side. (This axis already paid off: it caught
that the driver's `pkgs.xauth` exists only on recent nixpkgs — fixed to
`pkgs.xauth or pkgs.xorg.xauth` so the driver evaluates on 25.05 *and* 26.11.)

Podman commands the suite exercises (the codchi-server machine-driver surface):
`podman run -d … --mount type=volume,…,ro,subpath=store <FLAGS> --rootfs <dir>
<system>/init` (the core boot — note `--rootfs` is a **boolean** flag, so all
flags must precede it or podman treats the next as the command), `podman exec`
(in-machine probes), `podman inspect -f {{.State.Status}}`/`{{.State.Running}}`,
`podman rm -f` (generation-switch recreation), and `podman load`/`create`/`start`
for store bring-up when the store is not already running.

Two halves — the necessity half is the part that catches *silent* drift:

- **Sufficiency** — the documented arg set boots one machine and asserts the
  health invariants that prove each arg still does its job: `is-system-running =
  running`, zero failed units, `dbus-broker active` (proves `SYS_ADMIN`),
  `/run/wrappers/bin/sudo` setuid wrapper present (proves activation `specialfs`
  mounted under `SYS_ADMIN`), codchi user present, `/nix/store` visible **and
  read-only**.
- **Necessity** — drop each *required* arg in turn (`--cap-add SYS_ADMIN`,
  `--systemd=always`, the store mount) and assert boot **still breaks**. If a
  removal stops breaking, the requirement changed: the suite reports **DRIFT** (not
  pass) and names the decision (M2/M4/M5) to reconcile before shipping — that is
  how "are these args still the right minimal set" is answered automatically.
- Plus **persistence** (codchi-owned dir rootfs survives container recreation,
  M6-A) and an optional **X11** probe (`xdpyinfo` → host display) when a host X
  server is present.

Run on each Podman bump, each nixpkgs bump, and on a schedule. A `DRIFT` result is
the trigger to revise M2/M4/M5 here; a `FAIL` is a real regression in the machine
path.

## Supported version matrix (M13)

The boot contract holds only over a tested range of **Podman × NixOS** versions;
codchi must **warn the user** (not silently misbehave) when the host Podman or a
machine's NixOS release falls outside it. The matrix is kept honest by the M12
regression suite: a green cell there *is* a "verified" cell here.

| NixOS \ Podman | < 4.3 | 4.3 – 5.x |
|---|---|---|
| < 24.11        | unsupported | unknown (untested; driver uses the `< 24.11` `opengl` path) |
| 24.11          | unsupported | expected (designed floor; untested) |
| 25.05          | unsupported | **verified** (podman 5.8.2) |
| 26.11 / unstable | unsupported | **verified** (podman 5.8.2) |

Floors and why:

- **Podman ≥ 4.3 (hard floor).** The shared-store mount uses
  `--mount type=volume,…,subpath=store`; volume `subpath=` landed in Podman ~4.3.
  Below it the store cannot be shed-mounted as designed (M2). `--systemd=always`,
  `--rootfs`, `--cap-add SYS_ADMIN` are all older, so 4.3 is the binding constraint.
  Verified only on 5.8.2; 4.3–5.7 is *expected*.
- **NixOS ≥ 24.11 (soft floor).** The driver branches on
  `stateVersion >= 24.11` (`hardware.graphics` vs `hardware.opengl`) and dbus-broker
  has been the default since ~24.05. 25.05 and 26.11 are verified; 24.11 is the
  designed floor but untested; < 24.11 takes the legacy `opengl` path and is
  unknown.

How codchi surfaces it (warn, don't silently break):

- `codchi-server` reads the host `podman --version` once at startup and the
  machine's resolved NixOS release (from its flake lock / `system.nixos.release`)
  at create/rebuild, and compares against the floors above.
- Below the **hard** Podman floor → block with a finding
  (`platform.podman_unsupported`); outside the **verified** range but above the
  floor → a non-blocking warning finding (`platform.podman_untested` /
  `machine.nixos_untested`) pointing at `codchi doctor`. Codes are stable per the
  `00-contract-decisions` code-variant rule (see `04-platform-boot-exec` failure
  path); message/remediation text may evolve freely.
- The floors should live as constants in `codchi-shared` (e.g.
  `PODMAN_MIN_VERSION = 4.3`, supported-NixOS range) so the doc and the runtime
  check cannot drift; bumping them is a deliberate edit gated by an M12 run.

## Open items / next branches

- **M8 — Wayland / GPU not yet exercised.** X11 verified; Wayland socket forwarding
  and `--device /dev/dri` GPU passthrough are designed (M7) but untested here.
- **M9 — `/var/lib` persistence policy (M6).** Decide whether the whole writable
  root persists (A) or only enumerated data paths (B). Beta-LXD persisted all of
  `/var` implicitly; a dev machine running stateful services (e.g. postgres in
  `/var/lib`) needs it. Recommend (A) unless a reason to split emerges.
- **M10 — Build-in-store delivery.** The machine system must land in the store's
  `/nix` volume. The faithful path is building inside the store via its nix-daemon
  (Phase 6 generation flow already writes flake dirs server-side). During this
  bring-up an in-store `nix build` stalled on a cache.nixos.org substitution; a host
  build + `nix-store --export | podman exec … nix-store --import` into the store
  volume was used as a fallback and is a valid dev shortcut, not the product path.
- **M11 — Wire into `codchi-server`.** Translate M5/M7 into the Podman driver's
  `register`/`start` (mirrors `PodmanStore`). Boot readiness = poll
  `systemctl is-system-running` (or the machine agent) rather than a fixed sleep.
