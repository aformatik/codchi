{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.codchi.batteries;
in
{
  options.codchi.batteries.enable = mkEnableOption "" // {
    description = ''
      Adds convenient tools and options to your codchi.
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
    ];

    programs.nix-index.enable = true;
    programs.command-not-found.enable = false;
  };
}
