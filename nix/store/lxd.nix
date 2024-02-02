{ lib, config, pkgs, ... }:
let inherit (lib) mkEnableOption mkIf;

  udhcpcScript =
    let
      # taken from https://github.com/NixOS/nixpkgs/blob/4c8cf44c5b9481a4f093f1df3b8b7ba997a7c760/pkgs/os-specific/linux/busybox/default.nix#L36C3-L46C5:
      debianVersion = "1.30.1-6";
      debianSource = pkgs.fetchFromGitLab {
        domain = "salsa.debian.org";
        owner = "installer-team";
        repo = "busybox";
        rev = "debian/1%${debianVersion}";
        sha256 = "sha256-6r0RXtmqGXtJbvLSD1Ma1xpqR8oXL2bBKaUE/cSENL8=";
      };
    in
    "${debianSource}/debian/tree/udhcpc/etc/udhcpc/default.script";
in
{

  options.driver.lxd = {
    enable = mkEnableOption "LXD specific settings";
  };

  config.system = mkIf config.driver.lxd.enable {
    files."/usr/share/udhcpc/default.script" = udhcpcScript;
    # these should automatically fork to bg
    init.filesystem = lib.mkAfter /* bash */ ''
      syslogd
      udhcpc -S -s /usr/share/udhcpc/default.script
    '';

    build.tarballExtraCommands = /* bash */ ''
      DIRNAME="$(pwd)"
      cd ..
      mv "$DIRNAME" rootfs
      mkdir "$DIRNAME"
      mv rootfs "$DIRNAME"
      cd "$DIRNAME"

      cat <<EOF > metadata.yaml
      ${builtins.toJSON  {
        architecture = builtins.elemAt (builtins.match "^([a-z0-9_]+).+" (toString pkgs.system)) 0;
        creation_date = 1;
        # properties = {
        #   description = "${config.system.nixos.distroName} ${config.system.nixos.codeName} ${config.system.nixos.label} ${pkgs.system}";
        #   os = "${config.system.nixos.distroId}";
        #   release = "${config.system.nixos.codeName}";
        # };
      }}
      EOF
    '';
  };
}
