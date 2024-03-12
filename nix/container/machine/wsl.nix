{ lib, config, pkgs, consts, ... }:
let inherit (lib) mkEnableOption mkIf pipe concatLines;

  mnt = "/mnt/wsl/codchi";
  # WSL needs imperative mounting. Order is important!
  mounts = [
    { target = "/nix/store"; source = "/nix/store"; }
    { target = "/nix/var/nix/daemon-socket"; source = "/nix/var/nix/daemon-socket"; }
    { target = "/nix/var/nix/db"; source = "/nix/var/nix/db"; }
    { target = "/nix/var/nix/profiles"; source = consts.store.DIR_CONFIG_MACHINE; }
    { target = "/home/${consts.machine.USER}"; source = consts.store.DIR_DATA_MACHINE; }
  ];

  mkMounts = pipe mounts [
    (map ({ target, source }:
      let realSrc = mnt + source;
      in /* bash */ ''
        # while mount | grep -wq "${target}"; do
        #   umount -f "${target}" || true
        # done
        [ ! -d "${realSrc}" ] && mkdir -p "${realSrc}"
        [ ! -d "${target}" ] && mkdir -p "${target}"
        mount --bind "${realSrc}" "${target}"
      ''))
    concatLines
  ];

  INIT_ENV = "${mnt}/.machine-init-env";
  INIT_ENV_LOCAL = "/.machine-init-env";
  INIT_LOG = "${mnt}/.machine-init-log-$CODCHI_MACHINE_NAME";

in
{

  options.machine.driver.wsl = {
    enable = mkEnableOption "WSL specific settings";
  };

  config = mkIf config.machine.driver.wsl.enable {

    # defaults omitted (see https://learn.microsoft.com/en-us/windows/wsl/wsl-config)
    files."etc/wsl.conf" = pkgs.writeText "wsl.conf" (lib.generators.toINI { } {
      automount.mountFsTab = false;
      automount.options = "metadata,uid=1000,gid=100"; # TODO is this needed?
      user.default = consts.machine.USER;
      boot.systemd = true;

      # Symlink grep & systemctl for WSL such that it can determinde if distro is started. 
      # Alternative: boot.initWaitCommand in wsl.conf (undocumented)
      # ln -fs /nix/var/nix/profiles/system/sw/bin/{grep,systemctl} /
      boot.initWaitCommand = "/sbin/init-wait";
    });
    files."/sbin/init-wait" = pkgs.writeShellScriptStatic "init-wait" /* bash */ ''
      set -x
      PATH="$PATH:/nix/var/nix/profiles/system/sw/bin:/nix/var/nix/profiles/system/sw/sbin"
      systemctl is-system-running | grep -E "running|degraded"
    '';

    machine.init.hostSetup = /* bash */ ''
      set -x
      if [ ! -f "${INIT_ENV}" ]; then
        echo "This distribution is only meant to be started by codchi.exe!"
        exit 1
      fi
      # [ -f "${INIT_ENV}" ] && mv "${INIT_ENV}" "${INIT_ENV_LOCAL}"
      # if [ ! -f "${INIT_ENV_LOCAL}" ]; then
      #   echo "This distribution is only meant to be started by codchi.exe!"
      #   exit 1
      # fi
      
      source "${INIT_ENV}"
      rm "${INIT_ENV}"

      if [ -z "''${CODCHI_MACHINE_NAME:-}" ]; then
        echo "CODCHI_MACHINE_NAME not set!"
        exit 1
      fi

      set -E # make trap ERR work with set -e
      trap 'echo ${consts.INIT_EXIT_ERR} >&2; echo' ERR

      # prefix stdout / stderr
      exec 2> >(trap "echo" INT TERM; tee "${INIT_LOG}" >&2) 1>&2

      ${mkMounts}
      ln -fs ${mnt}/config /nix/var/nix/profiles/global

      if [ -n "''${CODCHI_WSL_USE_VCXSRV:-}" ]; then
        export PULSE_SERVER=tcp:$(ip route | awk '/^default/{print $3; exit}');
        export DISPLAY=$(ip route | awk '/^default/{print $3; exit}'):0
        unset WAYLAND_DISPLAY
      fi
    '';
  };
}
