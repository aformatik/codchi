# ndd / nix self-deadlock in shared store container

`crates/utils/src/ndd.rs` ("nix don't deadlock") wraps every `nix` invocation,
watches CPU + network + stdout activity, and on >15s of total inactivity treats
nix as deadlocked: it kills the child, runs `rm -f /nix/store/*.lock`, and
restarts (nix resumes from committed store paths). This is a band-aid; we want
to remove the load-bearing watchdog by fixing the underlying deadlock.

## Observed behavior (corrected understanding)

- Happens with a **single nix-daemon running only in the store container** and a
  **single build** — no other machines, no concurrent clients. The build
  **deadlocks itself**.
- Socket + db (`/nix/var/nix/daemon-socket`, `/nix/var/nix/db`) are maximally
  observed; not a cross-distro contention problem.
- **Intermittent**: kill + restart makes progress (so it is not a deterministic
  build-graph lock cycle / IFD self-lock — those would re-deadlock at the same
  point).
- The cure that works is **deleting the `.lock` files**, which points at a
  **PathLock** as the stuck resource.
- Reproduces on **both WSL and LXD**. This rules out WSL-only causes (9p
  `/mnt/wsl` semantics, the WSL2 lost-futex-wakeup bug). The common factor is
  the **store filesystem under a container**: WSL = drvfs/9p or ext4-on-VHD,
  LXD = overlayfs / zfs / btrfs.

## Leading hypothesis

Nix takes a PathLock (`fcntl`/`flock` on a `*.lock`) around a store path. On
overlayfs/9p/CoW filesystems, advisory-lock **release-on-close is not propagated
promptly / coherently** to the underlying fs. The next acquire of the same path
(a dependency, a retry, or a recursive/IFD nix call into the same daemon) then
blocks forever in `F_SETLKW` — zero CPU, zero net, zero output, which is exactly
ndd's signature. It is intermittent because it is a race between close/release
and the next acquire. Deleting the `.lock` file yields a fresh inode so the
blocking acquire succeeds.

Alternative mechanisms not yet ruled out (different fixes):
- **fsync hang** on overlayfs/btrfs/zfs after build / on db commit → looks like a
  deadlock, recovers once the fs flushes. Fix dir: `fsync-metadata = false`,
  `use-sqlite-wal = false`.
- **lost futex/condvar wakeup** in the multithreaded daemon (WSL2-class kernel
  bug); the LXD case would then be a separate coincidence. Less likely given it
  reproduces on both.

## Decisive diagnostic (do this on a live hang before fixing)

```bash
pstree -p $(pgrep -x nix-daemon)        # find the worker pid under the daemon
cat /proc/<pid>/wchan; echo             # client AND worker
cat /proc/<pid>/stack
strace -fp <pid>                        # expect fcntl(...,F_SETLKW,...) on a *.lock
ls -l /proc/<pid>/fd | grep '\.lock'    # which path is stuck
findmnt -no FSTYPE,SOURCE /nix/store    # which fs is breaking lock semantics
stat -f -c '%T' /nix/store
```

- Worker/recursive client parked in `F_SETLKW` on a `.lock` nobody holds live
  → confirms the broken-advisory-lock hypothesis; `findmnt` names the fs.
- Parked in `fsync` → fsync variant. Parked in `futex` → wakeup variant.

Open questions to answer from a live hang:
1. What is `/nix/store` actually on inside the store container in each env
   (overlayfs upperdir / zfs / btrfs / ext4-on-loop / 9p)?
2. Is it genuinely `F_SETLKW`, or `fsync` / `futex`?

## Real fix direction (if broken-lock hypothesis holds)

- Give the store container a **lock-correct filesystem for `/nix`**: a dedicated
  ext4/xfs volume (zvol or loopback) mounted straight at `/nix`, instead of
  living on the overlay rootfs / 9p. In LXD give the container a real
  disk/dataset for `/nix` rather than the overlay rootfs; in WSL ensure the store
  container's `/nix` is native ext4.
- Then `ndd` becomes belt-and-suspenders rather than load-bearing.

## Make ndd safer regardless (interim)

`ndd.rs:81` runs `rm -f /nix/store/*.lock` — global. In a shared store this
deletes locks held by **legitimate concurrent builds in other machines** →
two builders on the same drv → partial/corrupt store paths. Stale flock-style
locks auto-release on holder death, so after `child.kill()` the delete is often
unnecessary. Prefer kill+restart only; if deletion is needed, scope it to the
paths the killed process was touching, not a global glob.

## Related code

- `crates/utils/src/ndd.rs` — the watchdog.
- `crates/codchi/src/platform/machine.rs:312,358,362,365` — ndd call sites
  (flake update, build, profile install/upgrade).
- `nix/container/store/default.nix` — store container; note contradictory
  `NIX_REMOTE` handling (`:67` sets `daemon`, `:168` `unset`s it). Direct
  local-store clients take locks themselves; daemon clients do not. Worth
  straightening so "client vs direct store user" is unambiguous.
- `nix/container/machine/wsl.nix:8-13` — store/db/socket bind mounts into
  machines (relevant to the multi-machine case, not the self-deadlock).