{ lib, config, pkgs, consts, ... }:
let inherit (lib) mkEnableOption mkIf;
in {

  options.driver.wsl = {
    enable = mkEnableOption "WSL specific settings";
  };

  config = mkIf config.driver.wsl.enable {
    system.files."etc/wsl.conf" = pkgs.writeText "wsl.conf" (lib.generators.toINI { } {
      automount.mountFsTab = false;
      boot.command = "/bin/run /sbin/init";
    });
    system.init.filesystem = lib.mkAfter /* bash */ ''
      ${with lib; pipe 
        {
          ${consts.DIR_CONFIG} = "$WSL_CODCHI_DIR_CONFIG";
          ${consts.DIR_DATA} = "$WSL_CODCHI_DIR_DATA";
        } 
        [
          (mapAttrsToList (path: var: /* bash */ ''
            if [ -z "${var}" ] || [ ! -d "${var}" ]; then
              logE "Environment variable \${var} not set or host directory doesn't exist."
              exit 1
            fi
            [ -d "${path}" ] || mkdir -p "${path}"
            mount --bind "${var}" "${path}"
          ''))
          concatLines
        ]
      }

      # TODO check what happens if store stops and some machines continue to run
      [ -d "/mnt/wsl/codchi" ] || mkdir -p "/mnt/wsl/codchi"
      if ! mount | grep -q "/mnt/wsl/codchi"; then
        mount --bind "/" "/mnt/wsl/codchi"
      fi
      trap "umount -f /mnt/wsl/codchi" EXIT
    '';
  };
}
