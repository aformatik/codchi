{ inputs, lib, config, pkgs, ... }:
let
  inherit (lib) mkOption mkDefault types;
  cfg = config.codchi;
in
{

  imports = [
    ./docker.nix
    ./internal
    ./java.nix
    ./recommended-config.nix
  ];

  options.codchi = {
    defaultUser = mkOption {
      type = types.str;
      readOnly = true;
      default = "nixos";
      description = ''
        The name of the default linux user. This can be used to configure user
        specific things (e.g. add an user to a group) via `''${config.codchi.defaultUser}`
      '';
    };
  };

  config = {
    # disable nixos-rebuild
    system.disableInstallerTools = true;

    environment.variables.NIX_REMOTE = "daemon";
    systemd.services = {
      nix-daemon.enable = false;
      nix-gc.enable = false;
      nix-optimize.enable = false;
    };

    # Setup nix flakes
    nix = {
      package = pkgs.nixFlakes;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      registry.nixpkgs.flake = inputs.nixpkgs;
      nixPath = [ "nixpkgs=/etc/channels/nixpkgs" ];
    };
    environment.etc."channels/nixpkgs".source = inputs.nixpkgs;

    # User stuff
    users.users.${cfg.defaultUser} = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" ];
      initialPassword = "nixos";
    };

    # The default user will not have a password by default
    security.sudo.wheelNeedsPassword = mkDefault false;

    # Desktop stuff
    environment.systemPackages = [
      pkgs.xdg-utils
      pkgs.nixos-icons # needed for gnome and pantheon about dialog, nixos-manual and maybe more
    ];
    fonts.enableDefaultFonts = true;
  };

}
