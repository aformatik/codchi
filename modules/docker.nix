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
      Whether to enable the docker daemon.
      This also adds the default user to the docker group and installs
      docker-compose.
    '';
  };

  config = mkIf cfg.enable {
    virtualisation.docker.enable = true;
    users.users.${config.codchi.defaultUser}.extraGroups = [ "docker" ];
    environment.systemPackages = [ pkgs.docker-compose ];
  };

}
