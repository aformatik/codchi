{ config, pkgs, lib, ... }:
let
  inherit (lib) mkIf;
  cfg = config.codchi.driver.wsl;
in
{
  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (_: _: {
        # open files with windows explorer
        wsl-explorer = pkgs.stdenvNoCC.mkDerivation rec{
          pname = "wsl-explorer";
          version = "0.0.1";

          dontUnpack = true;
          installPhase = ''
            runHook preInstall

            install -D -t $out/bin $wslExplorer/bin/*
            install -D -t $out/share/applications $desktopItem/share/applications/*

            runHook postInstall
          '';

          desktopItem = pkgs.makeDesktopItem {
            name = pname;
            noDisplay = true;
            exec = "${pname} %u";
            icon = "inode-directory";
            desktopName = "Windows File Explorer";
            genericName = "File Explorer";
            categories = [ "FileManager" ];
            mimeTypes = [
              "inode/directory"
              "application/x-7z-compressed"
              "application/x-7z-compressed-tar"
              "application/x-bzip"
              "application/x-bzip-compressed-tar"
              "application/x-compress"
              "application/x-compressed-tar"
              "application/x-cpio"
              "application/x-gzip"
              "application/x-lha"
              "application/x-lzip"
              "application/x-lzip-compressed-tar"
              "application/x-lzma"
              "application/x-lzma-compressed-tar"
              "application/x-tar"
              "application/x-tarz"
              "application/x-xar"
              "application/x-xz"
              "application/x-xz-compressed-tar"
              "application/zip"
              "application/gzip"
              "application/bzip2"
              "application/vnd.rar"
              "application/zstd"
              "application/x-zstd-compressed-tar"
            ];
          };

          wslExplorer = pkgs.writeShellScriptBin "wsl-explorer" ''
            exec explorer.exe "$@"
          '';
        };

        # open files with windows default browser
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

            log() {
              echo "$@" >&2 
            }

            # opens in default browser
            WIN_BROWSER="explorer.exe"

            if [ -f "$1" ]; then
              exec "$WIN_BROWSER" "$1"
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

    environment.systemPackages = [ pkgs.wsl-browser pkgs.wsl-explorer ];

    xdg.mime.defaultApplications = {
      "inode/directory" = "wsl-explorer.desktop";
      "x-scheme-handler/http" = "wsl-browser.desktop";
      "x-scheme-handler/https" = "wsl-browser.desktop";
    };
  };

}
