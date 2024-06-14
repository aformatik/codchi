{ pkgs, config, lib, ... }:
let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.codchi.driver.lxd;
in
{
  options.codchi.driver.lxd.enable = mkEnableOption "LXD driver"
    // { internal = true; readonly = true; };

  config = mkIf cfg.enable {

    codchi.driver.name = "lxd";
    codchi.driver.iconCommand = lib.mkDefault ''
      cp "$ICON_PATH" "codchi/icons/"
    '';

    # Somehow setfacl -m fails during switch-to-configuration but succeedes on
    # container boot, so we mark them to only run on boot
    environment.etc."tmpfiles.d/systemd.conf".source =
      lib.mkForce (pkgs.runCommandLocal "lxd-setfacl-m-fix.systemd.conf" { } ''
        cp "${pkgs.systemd}/example/tmpfiles.d/systemd.conf" "$out"

        sed -i "s|a+ /var/log/journal|# OVERRIDDEN to setfacl -m on boot only\na+! /var/log/journal|" "$out"
      '');

    # Add the overrides from lxd distrobuilder
    # https://github.com/lxc/distrobuilder/blob/05978d0d5a72718154f1525c7d043e090ba7c3e0/distrobuilder/main.go#L630
    systemd.packages = [
      (pkgs.writeTextFile {
        name = "systemd-lxc-service-overrides";
        destination = "/etc/systemd/system/service.d/zzz-lxc-service.conf";
        text = ''
          [Service]
          ProcSubset=all
          ProtectProc=default
          ProtectControlGroups=no
          ProtectKernelTunables=no
          NoNewPrivileges=no
          LoadCredential=
        '';

        # Additional settings for privileged containers
        # ProtectHome=no
        # ProtectSystem=no
        # PrivateDevices=no
        # PrivateTmp=no
        # ProtectKernelLogs=no
        # ProtectKernelModules=no
        # ReadWritePaths=
      })
    ];

    environment.extraInit = /* bash */ ''
      if [ -n "''${XAUTHORITY:-}" ]; then
        DISPLAY="''${DISPLAY:-0}"
        COOKIE="$(${lib.getExe pkgs.xorg.xauth} list | tr -s ' ' |  cut -f 3 -d ' ' | head -n 1)"
        if [ -z "$COOKIE" ]; then
          echo "[codchi] Failed to extract cookie from xauth" >&2
        else
          echo "[codchi] Adding cookie to xauth: $COOKIE" >&2
          ${lib.getExe pkgs.xorg.xauth} add "$DISPLAY" . "$COOKIE"
        fi
      fi
    '';
  };
}

