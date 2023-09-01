{ inputs, lib, config, pkgs, ... }:
let
  inherit (lib) mkOption mkDefault types;
  cfg = config.codchi;
in
{

  imports = [
    ./internal
    ./java.nix
    ./recommended-config.nix
  ];

  options.codchi = {
    defaultUser = mkOption {
      type = types.str;
      internal = true;
      default = "nixos";
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
    xdg = {
      autostart.enable = true;
      menus.enable = true;
      mime.enable = true;
      icons.enable = true;
    };
    environment.sessionVariables = {
      XDG_CACHE_HOME = "\${HOME}/.cache";
      XDG_CONFIG_HOME = "\${HOME}/.config";
      XDG_BIN_HOME = "\${HOME}/.local/bin";
      XDG_DATA_HOME = "\${HOME}/.local/share";

      # https://wiki.archlinux.org/title/Java#Better_font_rendering
      JDK_JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on, -Dswing.aatext=true";
    };
  };

}
