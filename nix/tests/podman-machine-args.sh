#!/usr/bin/env bash
# Podman × NixOS machine: argument-set regression matrix.
#
# Pins the empirical Podman `run` contract from v1/phases/07-podman-machine.md
# (M4/M5/M7) so a Podman *or* nixpkgs change that alters what the machine needs
# fails loudly instead of silently. Two ideas:
#   - SUFFICIENCY: the documented arg set boots a healthy machine.
#   - NECESSITY:   removing each "required" arg still breaks boot. If a removal
#                  stops breaking, the requirement changed → reported as DRIFT
#                  (review M5/M4), not a pass.
# Run as a MATRIX over NixOS versions (the axis most likely to move dbus-broker /
# the activation specialfs / option names under our feet). The Podman version is
# the host's; it is recorded so a cross-version failure is attributable.
#
# Podman commands exercised (the codchi-server machine-driver surface):
#   podman --version                              · version record
#   podman container exists / inspect -f {{...}}  · store + machine state checks
#   podman load -q -i / create --volume / start   · store bring-up (if not running)
#   podman run -d --name --mount type=volume,...,ro,subpath=store \
#              <FLAGS> --rootfs <dir> <system>/init   · THE machine boot (core)
#   podman exec [-e ...] <name> <cmd>             · in-machine probes (boot/dbus/
#                                                   wrappers/store-ro/persistence/X11)
#   podman rm -f                                  · generation-switch recreation + cleanup
# (`--rootfs` is a BOOLEAN flag: the dir + init are trailing positionals, so all
#  flags MUST precede `--rootfs`. Getting that wrong leaves the container "created".)
#
# Prereqs: nix, rootless podman. Store started from the baked image if absent.
# Scheduled/manual (needs rootless podman + systemd) — not a `nix flake check`.
#
# Config (env):
#   NIXPKGS_REFS  space list; "flake" = repo-pinned, else a github nixpkgs ref.
#                 default: "flake nixos-25.05"
#   BOOT_TIMEOUT  s to reach `running` (default 120)
#   FAIL_TIMEOUT  s to confirm a negative test stays broken (default 40)
#   KEEP=1        don't clean up containers/tmp on exit
set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
STORE_CTR="codchi-store"; STORE_VOL="codchi-store-nix"
STORE_MOUNT="type=volume,src=${STORE_VOL},dst=/nix/store,ro=true,subpath=store"
PREFIX="codchi-argtest"
WORK="$(mktemp -d /tmp/codchi-argtest.XXXXXX)"
NIXPKGS_REFS="${NIXPKGS_REFS:-flake nixos-25.05}"
BOOT_TIMEOUT="${BOOT_TIMEOUT:-120}"; FAIL_TIMEOUT="${FAIL_TIMEOUT:-40}"
PASS=0; FAIL=0; DRIFT=0; LAST_STATE=""; BOOT_ERR=""

say()  { printf '\n\033[1m== %s ==\033[0m\n' "$*"; }
ok()   { printf '  \033[32mPASS\033[0m  %s\n' "$*"; PASS=$((PASS+1)); }
bad()  { printf '  \033[31mFAIL\033[0m  %s\n' "$*"; FAIL=$((FAIL+1)); }
drift(){ printf '  \033[33mDRIFT\033[0m %s\n' "$*"; DRIFT=$((DRIFT+1)); }
note() { printf '        %s\n' "$*"; }

cleanup() {
  [ -n "${KEEP:-}" ] && { echo "KEEP=1: leaving $WORK and ${PREFIX}-* containers"; return; }
  podman ps -aq --filter "name=${PREFIX}-" | xargs -r podman rm -f >/dev/null 2>&1
  # rootfs dirs contain files owned by mapped subuids; remove inside the userns.
  podman unshare rm -rf "$WORK" 2>/dev/null || rm -rf "$WORK" 2>/dev/null
}
trap cleanup EXIT

# timeout-bounded exec: a wedged machine makes `podman exec` hang.
ex() { timeout 8 podman exec "$1" /run/current-system/sw/bin/"$2" "${@:3}" 2>/dev/null; }
cstatus() { podman inspect -f '{{.State.Status}}' "$1" 2>/dev/null; }

# poll until `running`. 0 running · 2 degraded · 3 container died at podman level
# · 1 timeout (still "starting" or exec-unresponsive). Sets LAST_STATE.
poll_running() {
  local name="$1" timeout="$2" t=0 st s
  while [ "$t" -lt "$timeout" ]; do
    st="$(cstatus "$name")"
    if [ "$st" = exited ] || [ "$st" = created ]; then LAST_STATE="podman:$st"; return 3; fi
    s="$(ex "$name" systemctl is-system-running)"; LAST_STATE="${s:-<exec-unresponsive>}"
    case "$s" in running) return 0 ;; degraded) return 2 ;; esac
    sleep 3; t=$((t+3))
  done
  return 1
}

# boot NAME ROOTFS FLAG... — flags precede `--rootfs`; dir + init are positionals.
# Returns nonzero (and sets BOOT_ERR) if podman rejected the args (container never
# leaves "created"), so a CLI/script error is never mistaken for a boot result.
boot() {
  local name="$1" rootfs="$2"; shift 2
  podman rm -f "$name" >/dev/null 2>&1
  BOOT_ERR="$(podman run -d --name "$name" --mount "$STORE_MOUNT" "$@" \
              --rootfs "$rootfs" "$SYSTEM/init" 2>&1 >/dev/null)"
  [ "$(cstatus "$name")" != created ] || return 1
}

# the documented REQUIRED flags (M5), minus store-mount/rootfs/init (always added).
REQUIRED=( --systemd=always --cap-add SYS_ADMIN --env container=podman )

build_system() {   # build_system <ref> → echoes system path, imports into store
  local ref="$1" ov=()
  [ "$ref" != flake ] && ov=(--override-input nixpkgs "github:NixOS/nixpkgs/$ref")
  local sys
  sys="$(nix build --no-link --print-out-paths --accept-flake-config --no-warn-dirty "${ov[@]}" \
        "$REPO#nixosConfigurations.podman-base.config.system.build.toplevel" 2>/dev/null)" || return 1
  nix-store --export $(nix-store -qR "$sys") \
    | podman exec -i "$STORE_CTR" run nix-store --import >/dev/null 2>&1 || return 1
  echo "$sys"
}

#############################################################################
say "Environment (record with every scheduled run)"
podman --version
echo "host: $(uname -srm)"
echo "matrix NIXPKGS_REFS: $NIXPKGS_REFS"

say "Ensure store is up"
if ! podman container exists "$STORE_CTR" || \
   [ "$(podman inspect -f '{{.State.Running}}' "$STORE_CTR" 2>/dev/null)" != true ]; then
  note "store not running; building + starting from baked image"
  IMG="$(nix build --no-link --print-out-paths --no-warn-dirty "$REPO#store-podman-image")"
  LOADED="$(podman load -q -i "$IMG" | awk '{print $NF}')"
  podman rm -f "$STORE_CTR" >/dev/null 2>&1
  podman create --name "$STORE_CTR" --volume "${STORE_VOL}:/nix" "$LOADED" >/dev/null
  podman start "$STORE_CTR" >/dev/null; sleep 5
fi
podman exec "$STORE_CTR" run nix store info --store daemon >/dev/null 2>&1 \
  && ok "store nix-daemon answering" || { bad "store nix-daemon not answering"; exit 1; }

#############################################################################
# Per-version check groups. $SYSTEM and $V are set by the matrix loop.
sufficiency() {
  say "[$V] A. SUFFICIENCY — documented arg set boots a healthy machine"
  local n="${PREFIX}-A" R="$WORK/$V-A"; mkdir -p "$R"
  if ! boot "$n" "$R" "${REQUIRED[@]}"; then bad "[$V] boot rejected by podman: $BOOT_ERR"; return; fi
  poll_running "$n" "$BOOT_TIMEOUT"
  case $? in
    0) ok "[$V] is-system-running = running" ;;
    2) bad "[$V] degraded — failed: $(ex $n systemctl --failed --no-legend | head)"; return ;;
    3) bad "[$V] container died at podman level ($LAST_STATE) — $(podman logs --tail 3 $n 2>&1 | tr '\n' ' ')"; return ;;
    1) bad "[$V] not running within ${BOOT_TIMEOUT}s (last: $LAST_STATE)"; return ;;
  esac
  [ -z "$(ex $n systemctl --failed --no-legend)" ] && ok "[$V] zero failed units" || bad "[$V] failed units present"
  [ "$(ex $n systemctl is-active dbus-broker)" = active ] && ok "[$V] dbus-broker active (proves SYS_ADMIN)" || bad "[$V] dbus-broker not active"
  ex "$n" test -u /run/wrappers/bin/sudo && ok "[$V] /run/wrappers setuid wrapper (specialfs mounted under SYS_ADMIN)" || bad "[$V] /run/wrappers/bin/sudo missing"
  [ -n "$(ex $n id -u codchi)" ] && ok "[$V] codchi user present" || bad "[$V] codchi user missing"
  ex "$n" bash -lc 'touch /nix/store/.__rw 2>/dev/null' && bad "[$V] /nix/store WRITABLE (must be ro)" || ok "[$V] /nix/store read-only"
  local c; c="$(ex "$n" bash -lc 'ls /nix/store | wc -l')"; [ "${c:-0}" -gt 100 ] && ok "[$V] shared store visible ($c paths)" || bad "[$V] store not visible ($c)"
  podman rm -f "$n" >/dev/null 2>&1
}

necessity() {
  say "[$V] B. NECESSITY — each required arg, removed, must still break boot"
  _drop() {   # _drop LABEL FLAG...   (reduced flag set)
    local label="$1"; shift
    local n="${PREFIX}-B-$label" R="$WORK/$V-B-$label"; mkdir -p "$R"
    if ! boot "$n" "$R" "$@"; then bad "[$V] $label: podman rejected args (script bug): $BOOT_ERR"; return; fi
    poll_running "$n" "$FAIL_TIMEOUT"
    if [ $? -eq 0 ]; then drift "[$V] drop $label → STILL reached running; requirement changed (review M4/M5)"
    else ok "[$V] drop $label → broke as expected ($LAST_STATE)"; fi
    podman rm -f "$n" >/dev/null 2>&1
  }
  _drop no-sysadmin --systemd=always                    --env container=podman
  _drop no-systemd                     --cap-add SYS_ADMIN --env container=podman
  # store-mount removal needs a bespoke run (boot() always adds the mount):
  local n="${PREFIX}-B-no-store" R="$WORK/$V-B-no-store"; mkdir -p "$R"
  podman rm -f "$n" >/dev/null 2>&1
  podman run -d --name "$n" "${REQUIRED[@]}" --rootfs "$R" "$SYSTEM/init" >/dev/null 2>&1
  poll_running "$n" "$FAIL_TIMEOUT"
  [ $? -eq 0 ] && drift "[$V] drop store mount → STILL ran (review M2)" || ok "[$V] drop store mount → broke as expected ($LAST_STATE)"
  podman rm -f "$n" >/dev/null 2>&1
}

persistence() {
  say "[$V] C. PERSISTENCE — codchi-owned dir rootfs survives recreation (M6-A)"
  local n="${PREFIX}-C" R="$WORK/$V-persist"; mkdir -p "$R"
  if ! boot "$n" "$R" "${REQUIRED[@]}" || ! poll_running "$n" "$BOOT_TIMEOUT"; then bad "[$V] persistence machine did not boot ($LAST_STATE)"; return; fi
  ex "$n" sh -c 'echo sentinel-v1 > /home/codchi/.argtest && sync'
  podman rm -f "$n" >/dev/null 2>&1                 # simulate a generation switch
  boot "$n" "$R" "${REQUIRED[@]}"; poll_running "$n" "$BOOT_TIMEOUT" >/dev/null
  [ "$(ex $n cat /home/codchi/.argtest)" = sentinel-v1 ] \
    && ok "[$V] user file survived container recreation" || bad "[$V] user file lost across recreation"
  podman rm -f "$n" >/dev/null 2>&1
}

#############################################################################
for V in $NIXPKGS_REFS; do
  say "VERSION: $V"
  SYSTEM="$(build_system "$V")" || { bad "[$V] build/import failed (eval or substitution)"; continue; }
  ok "[$V] built + imported $(basename "$SYSTEM")"
  sufficiency
  necessity
  persistence
done

#############################################################################
say "D. X11 GUI (optional — once, only if host has an X server)"
if [ -n "${DISPLAY:-}" ] && [ -S /tmp/.X11-unix/X0 ] && [ -n "${SYSTEM:-}" ]; then
  XDPY="$(podman exec "$STORE_CTR" run sh -c 'nix build --no-link --print-out-paths nixpkgs#xorg.xdpyinfo 2>/dev/null')"
  n="${PREFIX}-D" R="$WORK/x11"; mkdir -p "$R"
  boot "$n" "$R" "${REQUIRED[@]}" -e DISPLAY --net=host \
    -v /tmp/.X11-unix:/tmp/.X11-unix -v "${XAUTHORITY:-$HOME/.Xauthority}:/root/.Xauthority:Z"
  poll_running "$n" "$BOOT_TIMEOUT" >/dev/null
  if timeout 12 podman exec -e DISPLAY=:0 -e XAUTHORITY=/root/.Xauthority "$n" "$XDPY/bin/xdpyinfo" >/dev/null 2>&1; then
    ok "X11 forwarding works (xdpyinfo reached host display)"
  else bad "X11 probe failed"; fi
  podman rm -f "$n" >/dev/null 2>&1
else
  note "SKIP (no host DISPLAY / X11 socket)"
fi

#############################################################################
say "Summary"
printf "PASS=%d FAIL=%d DRIFT=%d   (matrix: %s)\n" "$PASS" "$FAIL" "$DRIFT" "$NIXPKGS_REFS"
[ "$DRIFT" -gt 0 ] && echo "DRIFT = a documented requirement changed; reconcile v1/phases/07-podman-machine.md."
[ "$FAIL" -eq 0 ] && [ "$DRIFT" -eq 0 ]; exit $?
