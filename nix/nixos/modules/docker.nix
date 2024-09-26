{ config, lib, pkgs, ... }:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.codchi.docker;
in
{

  options.codchi.docker.enable = mkOption {
    type = types.bool;
    default = false;
    description = lib.mdDoc ''
      Whether to enable the docker daemon. This also adds the codchi user to the docker group and installs docker-compose.
    '';
  };

  config = mkIf cfg.enable {
    virtualisation.docker.enable = true;
    users.users.codchi.extraGroups = [ "docker" ];
    environment.systemPackages = [ pkgs.docker-compose ];

    # Prevent WSL overwriting docker when docker-desktop is installed
    environment.shellAliases = {
      docker = "${pkgs.docker}/bin/docker";
    };
  };

}
