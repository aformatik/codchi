{ lib, config, pkgs, consts, ... }:
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

    machine.init.hostSetup = /* bash */ ''
      [ -d /etc ] || mkdir /etc
      if [ -f "${consts.machine.INIT_ENV}" ]; then
        now=$(date +%s)
        old=$(stat -c %Y "${consts.machine.INIT_ENV}")
        age=$((now - old))

        if [ "$age" -gt 60 ]; then
          rm "${consts.machine.INIT_ENV}"
        fi
      fi

      # wait for fresh file
      echo "Waiting for fresh /etc/codchi-env" >&2
      while [ ! -f "${consts.machine.INIT_ENV}" ]; do
        sleep .25
      done

      source "${consts.machine.INIT_ENV}"

      if [ -z "''${CODCHI_MACHINE_NAME:-}" ]; then
        echo "CODCHI_MACHINE_NAME not set!" >&2
        exit 1
      fi

      exec 1> >(tee -i "/var/log/codchi/machine-$CODCHI_MACHINE_NAME.log") 2>&1
    '';
  };
}
