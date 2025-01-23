{ lib, config, pkgs, consts, ... }:
let inherit (lib) mkEnableOption mkIf pipe concatLines;

  cfg = config.machine.driver.wsl;

  mnt = "/mnt/wsl/codchi";
  # WSL needs imperative mounting. Order is important!
  mounts = [
    { target = "/nix/store"; source = "/nix/store"; }
    { target = "/nix/var/nix/daemon-socket"; source = "/nix/var/nix/daemon-socket"; }
    { target = "/nix/var/nix/db"; source = "/nix/var/nix/db"; }
    { target = "/nix/var/nix/profiles"; source = consts.store.DIR_CONFIG_MACHINE; }
  ];

  storeMnts = pipe mounts [
    (map ({ target, source }:
      let realSrc = mnt + source;
      in /* bash */ ''
        mkMnt "${realSrc}" "${target}"
      ''))
    concatLines
  ];

  INIT_LOG = "${mnt}${consts.store.MACHINE_LOG}";
  INIT_ENV_TMP = "/tmp/codchi-env";
  INIT_ENV_BACKUP = "/mnt/wsl/codchi/.machine-init-env";

in
{

  options.machine.driver.wsl = {
    enable = mkEnableOption "WSL specific settings";
    wslConf = lib.mkOption {
      description = "/etc/wsl.conf contents";
      type = lib.types.attrs;
    };
  };

  config = mkIf cfg.enable {

    machine.driver.wsl.wslConf = {
      automount.mountFsTab = false;
      automount.options = "metadata,uid=1000,gid=100"; # TODO is this needed?
      user.default = consts.machine.USER;
      boot.systemd = true;

      # Symlink grep & systemctl for WSL such that it can determinde if distro is started. 
      # Alternative: boot.initWaitCommand in wsl.conf (undocumented)
      # ln -fs /nix/var/nix/profiles/system/sw/bin/{grep,systemctl} /
      boot.initWaitCommand = "/sbin/init-wait";
    };

    # defaults omitted (see https://learn.microsoft.com/en-us/windows/wsl/wsl-config)
    files."etc/wsl.conf" = pkgs.writeText "wsl.conf" (lib.generators.toINI { } cfg.wslConf);
    files."/sbin/init-wait" = pkgs.writeShellScriptStatic "init-wait" /* bash */ ''
      set -x
      PATH="$PATH:/nix/var/nix/profiles/system/sw/bin:/nix/var/nix/profiles/system/sw/sbin"
      systemctl is-system-running | grep -E "running|degraded"
    '';

    machine.init.hostSetup = /* bash */ ''
      [ -d /etc ] || mkdir /etc

      if [ -f "${INIT_ENV_TMP}" ]; then
        mv "${INIT_ENV_TMP}" "${consts.machine.INIT_ENV}"
      elif [ -f "${INIT_ENV_BACKUP}" ]; then
        mv "${INIT_ENV_BACKUP}" "${consts.machine.INIT_ENV}"
      else
        echo "This distribution is only meant to be started by codchi.exe!" >&2
        # /mnt/c/WINDOWS/system32/msg.exe '*' "This distribution is only meant to be started by codchi.exe!" >&2
        exit 1
      fi

      # sometimes they don't get deleted...
      rm -f "${INIT_ENV_TMP}" "${INIT_ENV_BACKUP}" || true

      source "${consts.machine.INIT_ENV}"

      if [ -z "''${CODCHI_MACHINE_NAME:-}" ]; then
        echo "CODCHI_MACHINE_NAME not set!" >&2
        exit 1
      fi

      if [ -n "''${CODCHI_DEBUG:-}" ]; then
        set -x
      fi

      exec 1> >(tee -i "${INIT_LOG}" >&2) 2>&1

      mkMnt() {
        src="$1"
        target="$2"
        echo "Mounting $src on $target" >&2
        [ -d "$src" ] || mkdir -p "$src"
        [ -d "$target" ] || mkdir -p "$target"
        mount --bind "$src" "$target"
      }

      ${storeMnts}
      if [ ! -L "/nix/var/nix/profiles/global" ]; then
        ln -fs "${mnt}/config" "/nix/var/nix/profiles/global"
      fi

      target="${mnt + consts.store.DIR_MACHINE_DATA_MACHINE}"
      while mount | grep -wq "$target"; do
        umount "$target"
      done
      mkMnt "/home/${consts.machine.USER}" "$target"
      mkdir -p "${mnt + consts.store.DIR_DATA}/machine"
      if [ ! -L "${mnt + consts.store.DIR_DATA_MACHINE}" ]; then
        ln -fs "$target" "${mnt + consts.store.DIR_DATA_MACHINE}"
      fi
    '';
  };
}
