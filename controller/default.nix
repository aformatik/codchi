{ callPackage
, formats
, system
, lib
, runCommand
, fetchFromGitLab
, coreutils

, nixpkgs
, ...
}: {

  wsl-ctrl-rootfs = callPackage ./ctrl-rootfs.nix {
    inherit nixpkgs;
    preServeHook = ''
      [ ! -d "/nix" ] && mkdir -p "/nix"
      [ ! -d "/mnt/wsl/nix" ] && mkdir -p "/mnt/wsl/nix"
      if ! mount | grep -q "/mnt/wsl/nix"; then
        mount --bind "/nix" "/mnt/wsl/nix"
      fi
      trap "umount -f /mnt/wsl/nix" EXIT
    '';
  };

  lxd-ctrl-rootfs = callPackage ./ctrl-rootfs.nix {
    inherit nixpkgs;

    preServeHook = ''
      source /root/.bash_profile

      syslogd
      udhcpc -s /usr/share/udhcpc/default.script
    '';

    overrideContents = prev:
      let
        udhcpcScript =
          let
            # taken from https://github.com/NixOS/nixpkgs/blob/4c8cf44c5b9481a4f093f1df3b8b7ba997a7c760/pkgs/os-specific/linux/busybox/default.nix#L36C3-L46C5:
            debianVersion = "1.30.1-6";
            debianSource = fetchFromGitLab {
              domain = "salsa.debian.org";
              owner = "installer-team";
              repo = "busybox";
              rev = "debian/1%${debianVersion}";
              sha256 = "sha256-6r0RXtmqGXtJbvLSD1Ma1xpqR8oXL2bBKaUE/cSENL8=";
            };
          in
          runCommand "default.script" { buildInputs = [ coreutils ]; } ''
            sed '1 a echo "$@"' ${debianSource}/debian/tree/udhcpc/etc/udhcpc/default.script > $out
            chmod +x $out
          '';

        rootfs = prev // {
          "/sbin/init" = prev."/bin/ctrl-serve";
          "/usr/share/udhcpc/default.script" = udhcpcScript;
          "/tmp/" = null;
        };
      in

      # prepend /rootfs/ to every attr in `prev`
      lib.mapAttrs' (path: lib.nameValuePair ("/rootfs" + path)) rootfs

      //

      {
        "/metadata.yaml" = (formats.yaml { }).generate "metadata.yaml" {
          architecture = builtins.elemAt (builtins.match "^([a-z0-9_]+).+" (toString system)) 0;
          creation_date = 1;
          # properties = {
          #   description = "${config.system.nixos.distroName} ${config.system.nixos.codeName} ${config.system.nixos.label} ${pkgs.system}";
          #   os = "${config.system.nixos.distroId}";
          #   release = "${config.system.nixos.codeName}";
          # };
        };
      };
  };

}
