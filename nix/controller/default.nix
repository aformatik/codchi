{ writeScript
, writeText
, callPackage

, pkgsStatic
, cacert
, ...
}:

let
  binaries = with pkgsStatic; [ busybox bashInteractive nix ];
  writeShellScript = name: text: writeScript name ''
      #!/bin/bash

      set -e
      set -o pipefail

      ${text}
  '';


in callPackage ../make-tarball.nix {
  fileName = "controller";

  contents = {
    "/bin/" = toString (map (pkg: "${pkg}/bin/*") binaries);

    "/etc/profile.d/" = "${pkgsStatic.nix}/etc/profile.d/*";
    "/etc/ssl/certs/ca-bundle.crt" = "${cacert}/etc/ssl/certs/ca-bundle.crt";
    "/etc/" = "${./etc}/*";

    "/root/.bash_profile" = writeText ".bashrc" ''
      set -e
      . /etc/profile.d/nix.sh
      . /etc/profile.d/nix-daemon.sh
    '';

    "/bin/devenv-serve" = writeShellScript "devenv-serve" ''

      if pgrep nix-daemon; then
        echo "Nix-daemon is already running" >&2
        exit 1
      fi

      function unmount() {
        umount -f "/mnt/wsl/$1"
      }

      mnt() {
        [ ! -d "/$1" ] && mkdir -p "/$1"
        [ ! -d "/mnt/wsl/$1" ] && mkdir -p "/mnt/wsl/$1"
        if ! mount | grep -q "/mnt/wsl/$1"; then
          mount --bind "/$1" "/mnt/wsl/$1"
        fi
        trap "unmount \"$1\"" EXIT
      }

      mnt "nix"
      exec nix-daemon
    '';

    "/bin/devenv-install" = writeShellScript "devenv-install" ''
      NAME="$1"

      nix flake update "/instances/$NAME"
      DRV=$(nix build --impure --print-out-paths --no-link "/instances/$NAME#nixosConfigurations.default.config.system.build.devenv.tarball")
      mkdir -p "/nix/var/nix/profiles/per-devenv/$NAME"
      nix-env -p "/nix/var/nix/profiles/per-devenv/$NAME/system" --set $(cat $DRV/system-store-path)

      echo "$DRV"
    '';
  };
}
