{ lib, config, pkgs, ... }: {

  imports = [
    ./batteries.nix
    ./java.nix
  ];

  options.codchi = {
    defaultUser = lib.mkOption {
      type = lib.types.str;
      internal = true;
      default = "nixos";
    };
    instance = {
      name = lib.mkOption {
        type = lib.types.str;
        internal = true;
      };
    };
  };

  config = {
    users.users.${config.codchi.defaultUser} = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" ];
      initialPassword = "nixos";
    };

    # Setup nix flakes
    nix = lib.mkDefault {
      package = pkgs.nixFlakes;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
      registry.nixpkgs = {
        from = { type = "indirect"; id = "nixpkgs"; };
        to = { type = "path"; inherit (pkgs) path; };
      };
      nixPath = [ "nixpkgs=/etc/channels/nixpkgs" ];
    };
    environment.etc."channels/nixpkgs".source = pkgs.path;

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
