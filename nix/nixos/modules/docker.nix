{ config, lib, pkgs, ... }:
let
  inherit (lib) mkOption mkIf types mkMerge;
  cfg = config.codchi.docker;
in
{

  options.codchi.docker = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = lib.mdDoc ''
        Whether to enable the docker daemon. This also adds the codchi user to the docker group and installs docker-compose.
      '';
    };
    enableNvidia = mkOption {
      type = types.bool;
      default = false;
      description = lib.mdDoc ''
        Whether to install nvidia-container-toolkit to enable GPU usage inside docker containers. 
        This requires setting `codchi.gpu.enable = true` and `nixpkgs.config.allowUnfree = true`.
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      virtualisation.docker.enable = true;
      users.users.codchi.extraGroups = [ "docker" ];
      environment.systemPackages = [ pkgs.docker-compose ];

      # Prevent WSL overwriting docker when docker-desktop is installed
      environment.shellAliases = {
        docker = "${pkgs.docker}/bin/docker";
      };
    }
    (mkIf cfg.enableNvidia {
      assertions = [
        {
          assertion = config.codchi.gpu.enable;
          message = "`codchi.docker.enableNvidia` requires `codchi.gpu.enable = true`";
        }
        {
          assertion = config.nixpkgs.config.allowUnfree;
          message = "`codchi.docker.enableNvidia` requires `nixpkgs.config.allowUnfree = true`";
        }
      ];
    })
    (mkIf (cfg.enableNvidia && lib.versionAtLeast config.system.stateVersion "24.11") {
      hardware.nvidia-container-toolkit = {
        enable = true;
        mount-nvidia-executables = false;
      };
      virtualisation.docker.daemon.settings.features.cdi = true;
    })
    (mkIf (cfg.enableNvidia && !lib.versionAtLeast config.system.stateVersion "24.11") {
      hardware.opengl.driSupport32Bit = true;
      virtualisation.docker.enableNvidia = true;
    })
  ]);

}
