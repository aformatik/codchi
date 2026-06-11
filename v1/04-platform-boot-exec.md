# Platform Boot and Exec

## Platform Scope

v1 supports:

- Linux with Podman
- Windows with WSL

v0 had Linux LXD which is to be migrated / can serve as inspiration.

## Store Authority

Only `codchi-server` controls store startup and recovery.

`codchi-server` starts (or verifies) the store as part of its own startup and
keeps it running for the lifetime of the daemon. Machine boot paths never
trigger store start — they only consume an already-running store.

If the store cannot be started on server startup, the server attempts
**bounded auto-repair** before transitioning to `Degraded`. Auto-repair is
gated by the `auto_fixable` rule: it must not touch user data and must not
rewrite the stored `flake.lock`. Re-extracting the packaged store rootfs
over a corrupt store distro is in scope, because the store contains only
system files. Re-extracting system-file portions of a machine distro is also
in scope; user data partitions, mounted volumes, and home directories are
not. If auto-repair fails or is not applicable, the server transitions to
`Degraded`, records a `store.unavailable` finding, and waits for
user-invoked `doctor_fix`. After startup, no auto-repair runs anywhere — the
background reconciler is observe-only.

This is especially important on Windows. WSL instances can become partially
corrupt, and store recovery may need to re-extract the packaged root filesystem
without normal store boot.

No fallback helper should independently start or repair the store if the daemon
cannot do it.

## Windows WSL Boot Success Path

For a machine WSL instance:

1. WSL starts a minimal `codchi-machine-init`.
2. `codchi-machine-init` calls a Windows helper, `codchi-hostctl.exe`, using
   JSON over stdin/stdout.
3. `hostctl` starts/connects to `codchi-server`. The server brings up the
   store as part of its own startup (see *Store Authority*); the boot path
   does not start the store.
4. `codchi-server` verifies the store is running. If it is not, the server is
   in `Degraded` and the boot returns a structured failure (see
   *Windows WSL Boot Failure Path*).
5. `codchi-server` returns a complete machine boot spec.
6. `codchi-machine-init` mounts/prepares the machine and hands off to systemd.
7. `codchi-machine-agent` starts after boot and reports health.

The init binary should be small and conservative:

- no rebuild logic
- no migration logic
- no user prompts
- bounded timeouts
- atomic writes
- clear failure path

### Shared store/machine init binary (forward note from the store phase)

The **store** container has the same need for a thin PID-1 init (prepare
filesystem/mounts → bring up its long-lived service). For the Podman slice that
init is trivial enough to stay Nix-built bash (`v1/phases/01-podman-store.md`
S9). When WSL lands, fold both into **one small static Rust init binary** —
`codchi-store-init` and `codchi-machine-init` sharing the conservative contract
above (bounded, atomic, clear failure path) and the WSL specifics (bind-mounts,
bridge/NAT, the `codchi-hostctl.exe` boot-spec call). Note the asymmetry: the
store is *only ever* started by `codchi-server` (Store Authority), so its init can
be driven/pushed; a machine may boot on its own and must *pull* a boot spec. The
WSL keep-alive uses the persistent `codchi-server` holding the launching
`wsl.exe` subprocess open rather than in-distro `daemonize` (store-phase S10).

## Windows WSL Boot Failure Path

Starting a WSL instance does not guarantee an attached terminal.

Therefore WSL init scripts must not rely on terminal output and must not show
repeated Windows popups directly.

Failure path:

1. `codchi-machine-init` reports a structured boot failure through
   `codchi-hostctl.exe report-boot-failure`.
2. Host-side code (hostctl + `codchi-server`) records the failure. Source-side
   dedupe is not possible because `codchi-machine-init` is short-lived per WSL
   launch; dedupe must happen on the receiver.
3. The receiver applies a dedupe/throttle layer per `(machine, component,
   error code)`. Repeated launches into the same failure increment a count
   rather than re-recording the finding.
4. The display layer **merges** distinct simultaneous errors into a single
   user-visible notification. The intent is one popup per machine that lists
   what is currently wrong, not one popup per error.
5. The message points to `codchi doctor`.
6. Init exits without trying destructive repair.

Dedupe key:

```text
machine id
component
error code
```

If a finding's remediation needs to change in a way that should re-notify the
user, introduce a new stable `error code` variant rather than relying on
free-form remediation text. Code variants are the stable contract; message and
suggested-action text may evolve freely.

## In-Machine Components

Use three in-machine components:

```text
codchi-machine-agent
codchi-login-shell
codchi-session
```

`codchi-machine-agent`:

- starts as a systemd service after boot
- connects to `codchi-server`
- reports health
- fetches current runtime/session policy
- maintains runtime env/session state under `/run/codchi`
- provides a local helper API if needed

`codchi-login-shell`:

- is configured as the default user login shell
- handles direct entry such as `wsl -d codchi-machine-foo`
- contacts the local machine agent
- applies Codchi session environment
- execs the real configured shell
- degrades gracefully if the agent/server is unavailable

`codchi-session`:

- is the common per-command/session wrapper
- is used by `codchi exec`, login shell, and shortcuts where possible
- loads session information from the agent/server
- merges environment deterministically
- execs the final command or shell

## Environment Model

Separate boot environment from exec/session environment.

Boot environment:

- machine id/name
- active generation
- store paths
- boot flags
- activation/system-service secrets if needed

Exec/session environment:

- display/Wayland/Xauthority
- DBus/session bus
- SSH agent
- terminal/cwd
- per-command overrides
- user-facing secrets/env

Environment merge order:

```text
NixOS/PAM/login environment
< machine persistent env/secrets from SQLite
< client session env from CLI/tray
< per-command env overrides
```

Per-exec environment should live under `/run/codchi/...`, not in global files
such as `/etc/profile` or `/etc/environment`.

## Exec Model

For `codchi exec`, the CLI should use the platform-native exec mechanism
directly after a server health/preparation call.

Reason:

- PTY forwarding is hard.
- `wsl.exe` and `podman exec -it` already handle terminal attachment, resize,
  Ctrl+C, stdin/stdout/stderr, and exit code behavior better than a custom HTTP
  proxy would.

Flow:

```text
CLI
  -> codchi-server prepare-exec
  -> native platform exec
  -> codchi-session inside machine
```

Windows example:

```text
wsl.exe -d codchi-machine-foo --exec codchi-session --session <id> -- <cmd>
```

Linux example:

```text
podman exec -it codchi-foo codchi-session --session <id> -- <cmd>
```

The server still owns:

- machine validation
- store startup
- machine startup/preparation
- session id and session metadata
- environment resolution
- health/error reporting

The CLI owns:

- native terminal attachment
- stdin/stdout/stderr
- local Ctrl+C behavior
- exit code propagation

## Shell Entrypoint Matrix

Design rule: do not assume all paths go through a login shell. Every entrypoint
must either route through `codchi-session`, or be explicitly treated as a bypass
that may miss Codchi session environment.

Environment classes:

| Class | Known startup behavior |
| --- | --- |
| `direct exec` | No shell startup files. The process gets the runtime/container/WSL environment plus explicit env passed by the launcher. |
| `bash login` | Bash reads `/etc/profile`, then the first readable file among `~/.bash_profile`, `~/.bash_login`, `~/.profile`. |
| `sh login` | Bash invoked as `sh` reads `/etc/profile` and `~/.profile`. Other `/bin/sh` implementations may differ; verify on the image. |
| `bash interactive non-login` | Bash reads `~/.bashrc`. It does not read `/etc/profile` or `~/.profile`. |
| `bash non-interactive` | Bash does not read profile files or `~/.bashrc`; it may read `$BASH_ENV` if set. |
| `sh non-interactive` | No profile files. |
| `PAM/login session` | PAM/login may set environment before the shell starts. The final shell startup class still depends on argv and shell flags. |
| `systemd service` | No shell startup files for direct `ExecStart=...`. Environment comes from the manager, unit settings, credentials, and explicit `EnvironmentFile=`. |
| `existing session child` | Inherits the already-created session environment. Shell files only run if the child command starts a shell. |

### Codchi-Controlled Entrypoints

| Entrypoint | Command shape | Environment class | Certainty / action |
| --- | --- | --- | --- |
| Windows `codchi exec MACHINE -- CMD...` | `wsl.exe -d codchi-machine-foo --exec codchi-session --session <id> -- CMD...` | `direct exec` into `codchi-session`; `codchi-session` then creates the command env deterministically. | Known if Codchi uses `--exec` or `--shell-type none`. TODO: verify current legacy `--` behavior before supporting it. |
| Windows `codchi exec MACHINE` interactive shell | Same, ending in `codchi-session --session <id> -- <configured-shell>` | `codchi-session` must decide whether the real shell is `bash login`, `bash interactive non-login`, etc. | TODO: specify v1 policy. Recommended: Codchi default interactive shell is explicit `login` unless the user asks for raw/non-login. |
| Linux `codchi exec MACHINE -- CMD...` | `podman exec [-it] codchi-machine-foo codchi-session --session <id> -- CMD...` | `direct exec` into `codchi-session`; Podman does not create a shell by itself. | Known. Preserve cwd, tty, user, and env explicitly. |
| Linux `codchi exec MACHINE` interactive shell | `podman exec -it codchi-machine-foo codchi-session --session <id> -- <configured-shell>` | `direct exec` into `codchi-session`, then explicit shell startup chosen by Codchi. | Known up to `codchi-session`; shell class depends on v1 policy. |
| Codchi desktop shortcut / tray action | Should call `codchi exec` or native launcher with `codchi-session`. | Same as matching `codchi exec` path. | Must not launch `wsl.exe -d ...` or `podman exec ... bash` directly. |
| Codchi IDE launcher | Should call `codchi exec` or install an IDE terminal profile that invokes `codchi-session`. | Same as matching `codchi exec` path. | TODO: define per-IDE integration contract. |
| Store/machine maintenance command | Current store uses `run`/`runin`; machine uses `#!/bin/bash -l` wrappers. | Store wrapper injects Codchi build shell init, not user profile files. Machine wrapper is `bash login`. | TODO: decide whether v1 maintenance commands should keep using `run`/`runin` or move to `codchi-session` only for user-facing commands. |

### Windows WSL Direct Entrypoints

These paths can bypass Codchi entirely if the user, terminal, or IDE targets the
WSL distribution directly.

| Entrypoint | Command shape | Environment class | Certainty / action |
| --- | --- | --- | --- |
| Bare distro shell | `wsl.exe -d codchi-machine-foo` | WSL launches the default Linux shell. Exact login/non-login behavior is not guaranteed here. | TODO: test on supported WSL versions with bash, zsh, fish, and changed login shell. Prefer making the default login shell `codchi-login-shell`. |
| Distro shell as user | `wsl.exe -d codchi-machine-foo --user codchi` | Same as bare distro shell, but user selected by WSL. | TODO: verify whether `--user` changes HOME, cwd, PAM/session state, and login-shell status. |
| Explicit WSL login shell type | `wsl.exe -d codchi-machine-foo --shell-type login -- <cmd>` | WSL says it uses the default Linux shell as a login shell. For bash, expect `bash login`. | TODO: verify real behavior on supported WSL versions and shells. |
| Explicit WSL standard shell type | `wsl.exe -d codchi-machine-foo --shell-type standard -- <cmd>` | WSL says it uses the default Linux shell. Startup files depend on shell and whether WSL marks it interactive/login. | TODO: test. Treat as unsafe for Codchi env unless it enters `codchi-login-shell`. |
| Explicit WSL no-shell exec | `wsl.exe -d codchi-machine-foo --exec CMD...` or `wsl.exe -d codchi-machine-foo --shell-type none -- CMD...` | `direct exec`. | Known from WSL help. This is the desired native primitive for `codchi exec`. |
| Current/legacy WSL separator form | `wsl.exe -d codchi-machine-foo -- CMD...` | Ambiguous without testing: WSL help says `--` passes the remaining command line as-is, but shell insertion depends on WSL shell-type defaults. | TODO: test and replace with `--exec` / `--shell-type none` if this is not direct exec. |
| Windows command passthrough | `wsl.exe -d codchi-machine-foo CMD...` | Ambiguous: Microsoft documents this as running Linux tools from Windows; shell involvement depends on WSL mode/options. | TODO: test `argv`, parent process, profile markers, cwd, HOME, and env. |
| Start menu distro app | `codchi-machine-foo.exe` or distro app tile | Usually equivalent to launching the distro's default shell. | TODO: imported distributions may not have a launcher. Verify whether Codchi-created WSL distros expose one. |
| Windows Terminal WSL profile | Generated profile or commandline such as `wsl.exe -d codchi-machine-foo` | Depends on generated profile and WSL behavior. | TODO: verify Windows Terminal generated profile and custom profile examples. |
| VS Code Remote-WSL server launch | VS Code invokes WSL to start its server. | Unknown; likely direct command or shell command, version-dependent. | TODO: capture actual command line and profile files read on Windows + VS Code. |
| VS Code integrated terminal inside WSL | Shell started by VS Code server in existing distro session. | Usually `bash interactive non-login` if shell path is `/bin/bash`; user-configurable. | TODO: verify VS Code defaults and make profile call `codchi-session`. |
| JetBrains WSL terminal/server | IDE invokes WSL or starts shell inside the distro. | Unknown; reported behavior differs by IDE version and terminal backend. | TODO: capture command line and startup files for current JetBrains releases. |
| `wsl.exe -d codchi-machine-foo --system --user root CMD...` | WSL system context command. | Should be `direct exec` for the requested command, but system-distro details are WSL-specific. | TODO: verify before relying on it for recovery. Do not use for user sessions. |
| `\\wsl$` / `\\wsl.localhost` filesystem access | Windows process opens files through Plan9/redirector. | No command or shell is created. | Known. Must not trigger boot repair UX that assumes a terminal. |
| WSL boot/init | WSL starts init/systemd for the distro. | Boot environment only; no user shell. | Known enough for design. Do not put session env here. |

### Linux Podman Direct Entrypoints

| Entrypoint | Command shape | Environment class | Certainty / action |
| --- | --- | --- | --- |
| Direct command | `podman exec codchi-machine-foo CMD...` | `direct exec`. | Known. Bypasses `codchi-session` unless `CMD` is `codchi-session`. |
| Direct interactive shell | `podman exec -it codchi-machine-foo bash` | `bash interactive non-login`; reads `~/.bashrc`. | Known from bash rules. |
| Direct login shell | `podman exec -it codchi-machine-foo bash -l` | `bash login`. | Known from bash rules. |
| Direct non-interactive shell command | `podman exec codchi-machine-foo bash -c 'CMD'` | `bash non-interactive`; no profile files unless `$BASH_ENV` is set. | Known from bash rules. |
| Direct POSIX shell command | `podman exec codchi-machine-foo sh -c 'CMD'` | `sh non-interactive`; no profile files. | Known for bash-as-sh; verify actual `/bin/sh` in image. |
| Direct exec as user | `podman exec --user UID:GID codchi-machine-foo CMD...` | `direct exec`; Podman changes UID/GID. HOME and supplementary groups depend on Podman/image behavior. | TODO: verify HOME, USER, LOGNAME, groups, cwd, and `/etc/passwd` lookup on supported Podman. |
| Direct exec with env | `podman exec -e NAME=VALUE codchi-machine-foo CMD...` | `direct exec` plus explicit env. | Known. |
| Container start command | `podman start` / container `CMD` / `/sbin/init` | Runtime starts the configured process; no user shell unless the image command is a shell. | Known. Boot env only. |
| Attach existing process | `podman attach codchi-machine-foo` | No new shell; attaches to existing process stdio. | Known. |
| Namespace bypass | `nsenter ... -- CMD...` against container or machine PID | `direct exec` in selected namespaces. | TODO: verify whether this is possible for rootless Podman setup; treat as unsupported bypass. |

### Other Linux / Systemd Entrypoints

| Entrypoint | Command shape | Environment class | Certainty / action |
| --- | --- | --- | --- |
| System service | `systemd` unit with `ExecStart=/path/to/cmd` | `systemd service`; no shell profile files. | Known. Use explicit `Environment=`/credentials for service env. |
| System service via shell | `ExecStart=/bin/sh -c 'CMD'` | `sh non-interactive`; no profile files. | Known for `/bin/sh -c`. |
| User service | `systemd --user` unit | User manager env, then direct exec; no shell profile files. | TODO: verify whether WSL/Podman user managers are started through PAM/login or lingering. |
| `systemd-run --machine` | `systemd-run --machine=... --pty ...` | Direct service command or service shell depending on options. | TODO: verify systemd version and whether it is available in WSL/Podman machines. |
| `machinectl shell` | `machinectl shell USER@MACHINE [CMD...]` | PAM/login session plus either user's shell or explicit command. Exact shell login status needs testing. | TODO: verify if retained from v0/LXD paths. |
| `machinectl shell ... /bin/bash -lc 'CMD'` | Explicit bash command under `machinectl`. | PAM/session env plus `bash login` because of `-l`; non-interactive command because of `-c`. | Known bash part; TODO: verify PAM/systemd env contribution. |
| `machinectl login` / console login | Virtual console login. | `PAM/login session`, then user's login shell. | TODO: verify availability in machine image. |
| SSH interactive session | `ssh codchi-machine` with no remote command | Usually PAM/login plus user's login shell. | TODO: only relevant if sshd is enabled; verify OpenSSH behavior in image. |
| SSH remote command | `ssh codchi-machine CMD...` | Usually sshd executes user's shell with `-c CMD`; startup files vary by shell and sshd config. | TODO: verify before supporting SSH as Codchi entrypoint. |
| `su - USER` inside machine | `su - codchi` | Login shell for target user. | Known conceptually; PAM/env details distro-specific. |
| `su USER` inside machine | `su codchi` | Non-login shell for target user. | TODO: verify profile files and env preservation in image. |
| `sudo -i` inside machine | `sudo -i` | Login shell for target user with sudoers-controlled env reset/preservation. | TODO: verify sudo policy if sudo is installed. |
| `sudo -s` inside machine | `sudo -s` | Shell with sudoers-controlled env; not necessarily login. | TODO: verify if sudo is installed. |
| Terminal multiplexer new shell | `tmux`, `screen`, `zellij` | `existing session child`; shell startup depends on multiplexer config. | TODO: verify defaults if these are preinstalled or recommended. |

### Required Test Probe

For every TODO entry, run a real-platform probe that records both parent
launcher behavior and shell startup files. At minimum:

```sh
printf 'argv0=%s\nflags=%s\nshell=%s\nuser=%s\nhome=%s\npwd=%s\n' \
  "$0" "$-" "$SHELL" "$(id -un)" "$HOME" "$PWD"
if command -v shopt >/dev/null 2>&1; then
  shopt -q login_shell && echo 'bash_login_shell=yes' || echo 'bash_login_shell=no'
fi
ps -o pid,ppid,user,args -p "$$" -p "$PPID"
env | sort
```

Use sentinel exports or append-only log lines in `/etc/profile`,
`~/.profile`, `~/.bash_profile`, `~/.bash_login`, `~/.bashrc`, and `$BASH_ENV`
to identify which files actually ran.
