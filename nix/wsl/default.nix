{ inputs, ... }:
{ config, pkgs, ... }: {

  imports = [ 
    inputs.nixos-wsl.nixosModules.default
    ./tarball.nix
  ];

  config = {

    fonts.enableDefaultFonts = true;
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
  
    nixpkgs.overlays = [
      (_: _: {
        wsl-browser = pkgs.stdenvNoCC.mkDerivation rec{
          pname = "wsl-browser";
          version = "0.0.1";

          dontUnpack = true;
          installPhase = ''
            runHook preInstall

            install -D -t $out/bin $wslBrowser/bin/*
            install -D -t $out/share/applications $desktopItem/share/applications/*

            runHook postInstall
          '';

          desktopItem = pkgs.makeDesktopItem {
            name = pname;
            noDisplay = true;
            exec = "${pname} %u";
            icon = "browser";
            desktopName = "WSL Browser";
            genericName = "Web Browser";
            categories = [ "Network" "WebBrowser" ];
            mimeTypes = [
              "text/html"
              "text/xml"
              "application/xhtml+xml"
              "application/vnd.mozilla.xul+xml"
              "x-scheme-handler/http"
              "x-scheme-handler/https"
              "x-scheme-handler/ftp"
            ];
          };

          wslBrowser = pkgs.writeShellScriptBin "wsl-browser" ''
            if [ "$#" -ne 1 ]; then
              echo "Usage: $0 URL"
              exit 1
            fi

            WIN_BROWSER="$(reg.exe QUERY 'HKEY_CLASSES_ROOT\htmlfile\shell\open\command' /ve |
              grep Default |
              tr -s ' ' |
              sed 's/^.*"\(.*\)".*$/\1/' |
              xargs -I@ wslpath "@")"

            log() {
              echo "$@" >&2 
            }

            errexit() {
              log "Error: $@"
              msg.exe '*' "$@"
              exit 1
            }

            if [ -z "$(type -P "$WIN_BROWSER" || true)" ]; then
              errexit "Could not find default Windows Browser."
            else
              log "Found Windows Browser: $WIN_BROWSER"
            fi

            if [ -e "$1" ] && [[ $1 == /* ]]; then
              WIN_FILE="$(wslpath -w "$1")"
              log "Rewriting linux path to windows path:"
              log "\"$1\" -> \"$WIN_FILE\""
              exec "$WIN_BROWSER" "$WIN_FILE"
            else
              LOCAL_IP="$(ifconfig |
                grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' |
                grep -Eo '([0-9]*\.){3}[0-9]*' |
                grep -v '127.0.0.1')"

              if [ -z "$LOCAL_IP" ]; then
                log "Could not determine local IP. Passing URL \"$1\" as is."
                exec "$WIN_BROWSER" "$1"
              else
                REWRITTEN_URL="$(echo "$1" |
                  sed "s/0.0.0.0/$LOCAL_IP/" |
                  sed "s/127.0.0.1/$LOCAL_IP/")"
                log "Passing rewritten URL: \"$REWRITTEN_URL\""
                exec "$WIN_BROWSER" "$REWRITTEN_URL"
              fi
            fi
          '';
        };
      })
    ];

    environment.systemPackages = [ pkgs.wsl-browser ];
  };

}
