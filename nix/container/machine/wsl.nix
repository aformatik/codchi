{ lib, config, pkgs, consts, ... }:
let inherit (lib) mkEnableOption mkIf pipe concatLines;

  mnt = "/mnt/wsl/codchi";
  # WSL needs imperative mounting. Order is important!
  mounts = [
    { target = "/nix/store"; source = "/nix/store"; }
    { target = "/nix/var/nix/daemon-socket"; source = "/nix/var/nix/daemon-socket"; }
    { target = "/nix/var/nix/db"; source = "/nix/var/nix/db"; }
    { target = "/nix/var/nix/profiles"; source = consts.store.DIR_CONFIG_MACHINE; }
    # keep all profiles for GC
    { target = "/nix/var/nix/profiles/codchi"; source = consts.store.DIR_CONFIG; }
    { target = "/home/${consts.machine.USER}"; source = consts.store.DIR_DATA_MACHINE; }
  ];

  mkMounts = pipe mounts [
    (map ({ target, source }:
      let realSrc = mnt + source;
      in /* bash */ ''
        [ ! -d "${target}" ] && mkdir -p "${target}"
        mount --bind "${realSrc}" "${target}"
      ''))
    concatLines
  ];

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
    });

    machine.init.hostSetup = /* bash */ ''
      if [ -z "''${CODCHI_MACHINE_NAME:-}" ]; then
        echo "This distribution is only meant to be started by codchi.exe!"
        exit 1
      fi

      ${mkMounts}

      if [ -n "''${CODCHI_WSL_USE_VCXSRV:-}" ]; then
        export PULSE_SERVER=tcp:$(ip route | awk '/^default/{print $3; exit}');
        export DISPLAY=$(ip route | awk '/^default/{print $3; exit}'):0
        unset WAYLAND_DISPLAY
      fi
    '';
  };
}
