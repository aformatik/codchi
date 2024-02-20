{ lib, config, pkgs, ... }:
let inherit (lib) mkEnableOption mkIf;
in
{

  options.machine.driver.lxd = {
    enable = mkEnableOption "LXD specific settings";
  };

  config = mkIf config.machine.driver.lxd.enable {
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
