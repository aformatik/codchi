{ inputs, ... }:
{ config, pkgs, ... }: {

  imports = [ 
    inputs.nixos-wsl.nixosModules.default
    ./tarball.nix
  ];

  config = {

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

      extraSetup = let
        collectShortcuts = pkgs.writeScript "collect-shortcuts" ''
          set -ex
          set -o pipefail

          pushd $out/share

          mkdir -p wsl/{icos,applications}

          for app in applications/*.desktop; do
            NAME="$(grep Icon "$app" | sed 's/Icon=//')"
            ICON="$(find -L icons/ pixmaps/ -name "$NAME.*" | sort -rV | head -n1)"
            if [ ! -z "$ICON" ]; then
              ${pkgs.imagemagick}/bin/convert -background transparent -define icon:auto-resize=16,24,32,48,64,72,96,128,256 "$ICON" "wsl/icos/$NAME.ico"
            fi
            cp "$app" wsl/applications
          done

          popd
        '';
      in toString collectShortcuts;
    };

  };

}
