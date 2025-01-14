{ config, pkgs, lib, ... }:
let
  inherit (lib) mkIf mkMerge;
  cfg = config.codchi.driver.wsl;
in
{
  config = mkIf cfg.enable (mkMerge [
    {
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
    }
    (mkIf config.codchi.gpu.enable (
      let
        # src: https://github.com/nix-community/NixOS-WSL/blob/34b95b3962f5b3436d4bae5091d1b2ff7c1eb180/modules/wsl-distro.nix#L77
        wslLib = pkgs.runCommand "wsl-lib" { } ''
          mkdir -p "$out/lib"
              # we cannot just symlink the lib directory because it breaks merging with other drivers that provide the same directory
              ln -s /usr/lib/wsl/lib/libcudadebugger.so.1 "$out/lib"
              ln -s /usr/lib/wsl/lib/libcuda.so "$out/lib"
              ln -s /usr/lib/wsl/lib/libcuda.so.1 "$out/lib"
              ln -s /usr/lib/wsl/lib/libcuda.so.1.1 "$out/lib"
              ln -s /usr/lib/wsl/lib/libd3d12core.so "$out/lib"
              ln -s /usr/lib/wsl/lib/libd3d12.so "$out/lib"
              ln -s /usr/lib/wsl/lib/libdxcore.so "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvcuvid.so "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvcuvid.so.1 "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvdxdlkernels.so "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvidia-encode.so "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvidia-encode.so.1 "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvidia-ml.so.1 "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvidia-opticalflow.so "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvidia-opticalflow.so.1 "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvoptix.so.1 "$out/lib"
              ln -s /usr/lib/wsl/lib/libnvwgf2umx.so "$out/lib"
              ln -s /usr/lib/wsl/lib/nvidia-smi "$out/lib"
        '';
      in
      {
        hardware =
          if lib.versionAtLeast config.system.stateVersion "24.11"
          then {
            graphics = {
              enable = true;
              extraPackages = [ wslLib ];
            };
          }
          else {
            opengl = {
              enable = true;
              driSupport = true;
              extraPackages = [ wslLib ];
            };
          };

        programs.nix-ld = {
          enable = true;
          libraries = [ wslLib ];
        };
        environment.sessionVariables.LD_LIBRARY_PATH = [ "/run/opengl-driver/lib" ];

      }
    ))
  ]);

}
