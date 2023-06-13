{ config, options, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.devenv.java;
in
{
  options.devenv.batteries.include = mkEnableOption "java" // {
    description = ''
      Adds convenient tools and options to your devenv.
    '';
    default = true;
  };

  config = mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      vim
      git
      gzip
      bzip2
      perl

      nix-index
    ];

    # TODO: switch to programs.nix-index.enable in 23.05
    programs.command-not-found.enable = false;
    programs.bash.interactiveShellInit = ''
      source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
    '';
  };
}
