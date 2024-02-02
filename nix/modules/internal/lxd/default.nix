{ pkgs, config, lib, ... }:
let
  inherit (lib) mapAttrs' nameValuePair
    mkEnableOption mkIf;

  cfg = config.codchi.internal.lxd;
in
{
  options.codchi.internal.lxd.enable = mkEnableOption "codchi's LXD driver"
    // { internal = true; readonly = true; };

  config = mkIf cfg.enable {

    codchi.internal.init = {
      rootfsContents = {
        "/bin/" = with pkgs.pkgsStatic; toString (map (pkg: "${pkg}/bin/*") [ busybox bash ]);

        "/sbin/init" = pkgs.writeScript "sytemd-wrapper" ''
          #!/bin/bash
          set -e

          LANG="C.UTF-8" /nix/var/nix/profiles/system/activate
          exec /nix/var/nix/profiles/system/systemd/lib/systemd/systemd "$@"
        '';

        "/etc/" = null;
        "/dev/" = null;
        "/proc/" = null;
        "/sys/" = null;
        "/tmp" = null;
        "/var" = null;
      };

      overrideContents = rootfs:
        # prepend /rootfs
        mapAttrs' (path: nameValuePair ("/rootfs" + path)) rootfs
        // {
          "/metadata.yaml" = (pkgs.formats.yaml {}).generate "metadata.yaml" {
            architecture = builtins.elemAt (builtins.match "^([a-z0-9_]+).+" (toString pkgs.system)) 0;
            creation_date = 1;
            # properties = {
            #   description = "${config.system.nixos.distroName} ${config.system.nixos.codeName} ${config.system.nixos.label} ${pkgs.system}";
            #   os = "${config.system.nixos.distroId}";
            #   release = "${config.system.nixos.codeName}";
            # };
          };
        };
    };

    boot.isContainer = true;

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
      })
    ];

    # Allow the user to login as root without password.
    # users.users.root.initialHashedPassword = lib.mkOverride 150 "";

  };
}

