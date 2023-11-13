{ config, pkgs, lib, ... }:
let
  inherit (lib) mkEnableOption mkIf
    generators;
  cfg = config.codchi.internal.wsl;
in
{

  imports = [
    ./driver.nix
    ./browser.nix
  ];

  options.codchi.internal.wsl.enable = mkEnableOption "codchi's WSL driver"
    // { internal = true; readonly = true; };

  config = mkIf cfg.enable {

    codchi.internal.init.rootfsContents = {
      "/bin/" = with pkgs.pkgsStatic; toString (map (pkg: "${pkg}/bin/*") [ busybox bashInteractive ]);

      # See https://learn.microsoft.com/en-us/windows/wsl/wsl-config#configuration-settings-for-wslconf for all options
      "/etc/wsl.conf" = pkgs.writeText "wsl.conf" (generators.toINI { } {
        automount = {
          enabled = true;
          mountFsTab = false;
          root = "/mnt";
          options = "metadata,uid=1000,gid=100";
        };
        boot = {
          command = "";
          systemd = true;
          initPath = "/sbin/init";
          initWaitCommand = pkgs.writeShellScript "nixos-wsl-init-wait" ''
            ${pkgs.systemd}/bin/systemctl is-system-running | ${pkgs.gnugrep}/bin/grep -E "(running|degraded)"
            exit $?
          '';
          initShutdownCommand = pkgs.writeShellScript "nixos-wsl-init-shutdown" ''
            ${pkgs.systemd}/bin/systemctl reboot "$@"
          '';
        };
        network = {
          generateHosts = true;
          generateResolvConf = true;
          hostname = config.networking.hostName;
        };
        interop = {
          enabled = true;
          appendWindowsPath = true;
        };
        user.default = config.codchi.defaultUser;
      });

      "/sbin/init" = pkgs.writeScript "sytemd-wrapper" ''
        #!/bin/bash
        set -e
        set -o pipefail
        # this logs everything to dmesg
        set -x

        PATH="$PATH:/bin"

        if [ ! -d /mnt/wsl/nix/store ] || [ ! -d /mnt/wsl/nix/var/nix/daemon-socket ] ; then
          echo "Remote store is not mounted on /mnt/wsl/nix" >&2
          exit 1
        fi
        if [ ! -S /mnt/wsl/nix/var/nix/daemon-socket/socket ] ; then
          echo "Remote nix-daemon is not running." >&2
          exit 1
        fi

        mnt() {
          echo "Mounting remote $1 on $2..."
          mkdir -p "$2" || true
          mount --bind "/mnt/wsl/$1" "$2" $3

          trap "umount -f \"$2\"" EXIT
        }

        mnt /nix/store /nix/store -r
        mnt /nix/var/nix/daemon-socket /nix/var/nix/daemon-socket
        mnt "/nix/var/nix/profiles/per-instance/${config.codchi.internal.name}" /nix/var/nix/profiles

        mnt /nix/var/nix/profiles /nix/var/nix/profiles/global
        mnt /nix/var/nix/db /nix/var/nix/db

        LANG="C.UTF-8" /nix/var/nix/profiles/system/activate
        exec /nix/var/nix/profiles/system/systemd/lib/systemd/systemd "$@"
      '';

    };

    environment = {
      sessionVariables = {
        # Allow OpenGL in WSL
        LIBGL_ALWAYS_INDIRECT = "1";

        # Don't prompt for VS code server when running `code`
        DONT_PROMPT_WSL_INSTALL = "1";
      };
      shellInit = ''
        export PULSE_SERVER=tcp:$(ip route | awk '/^default/{print $3; exit}');
        export DISPLAY=$(ip route | awk '/^default/{print $3; exit}'):0
        unset WAYLAND_DISPLAY
      '';

    };

  };

}
