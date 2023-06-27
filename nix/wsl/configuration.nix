{ inputs, ... }:
{ config, pkgs, ... }: {

  imports = [ 
    inputs.nixos-wsl.nixosModules.default
    ./tarball.nix
  ];

  config = {

    # systemd.package = pkgs.systemd.overrideAttrs ({ patches, ... }: {
    #   patches = patches ++ [
    #     ./systemd-systemctl-status-wsl.patch
    #   ];
    # });

    environment = {
      sessionVariables = {
        # Allow OpenGL in WSL
        LIBGL_ALWAYS_INDIRECT = "1";

        # Don't prompt for VS code server when running `code`
        DONT_PROMPT_WSL_INSTALL = "1";
      };
      shellInit = ''
        export PULSE_SERVER=tcp:$(ip route | awk '/^default/{print $3; exit}');
        export DISPLAY=$(ip route | awk '/^default/{print $3; exit}'):0
        unset WAYLAND_DISPLAY
      '';
    };

  };

}
