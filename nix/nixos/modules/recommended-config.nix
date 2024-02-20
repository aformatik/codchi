{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf;
in
{
  options.codchi.enableRecommendedConfig = mkEnableOption "recommended NixOS options"
    // { default = true; };

  config = mkIf config.codchi.enableRecommendedConfig {
    environment.systemPackages = with pkgs; [
      vim
      git
      gzip
      bzip2
      perl
    ];
  };
}

