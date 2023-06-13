{ writeScript
, writeText
  # , dockerTools
, runCommandLocal
, lib

, docker-nixpkgs
, coreutils
, gzip

, extension ? ".gz"
, compressCommand ? "gzip"
, extraBuildInputs ? [ gzip ]
, ...
}:

let
  inherit (lib) concatStringsSep pipe mapAttrsToList generators;

  contents = {
    "/etc/wsl.conf" = writeText "wsl.conf" (generators.toINI { } {
      automount.mountFsTab = false;
      # boot.command = "/bin/serve";
    });
    "/root/.bash_profile" = writeText ".bashrc" ''
      export SSL_CERT_FILE="/etc/ssl/certs/ca-bundle.crt";
    '';
    "/bin/serve" = writeScript "serve" ''
      #!/bin/bash

      set -e

      if pgrep nix-daemon; then
        echo "Nix-daemon is already running" >&2
        exit 1
      fi

      mnt() {
        [ ! -d "/$1" ] && mkdir -p "/$1"
        [ ! -d "/mnt/wsl/$1" ] && mkdir -p "/mnt/wsl/$1"
        if ! mount | grep -q "/mnt/wsl/$1"; then
          mount --bind "/$1" "/mnt/wsl/$1"
        fi
      }

      mnt "nix/store"
      mnt "nix/var/nix/db"
      mnt "nix/var/nix/gcroots"
      mnt "nix/var/nix/gc-socket"
      mnt "nix/var/nix/daemon-socket"

      nix-daemon
    '';
  };

in
runCommandLocal "controller-rootfs"
{ buildInputs = [ coreutils ] ++ extraBuildInputs; } ''
  set -xe
  mkdir root $out
  cd root

  # copy and clean source rootfs
  cp -a ${docker-nixpkgs.nix-unstable-static.passthru.unpacked}/* .
  chmod -R +w .
  rm run_as_user.sh

  ${pipe contents [
    (mapAttrsToList (target: source: "cp -fa ${source} .${target}"))
    (concatStringsSep "\n")
  ]}

  echo 'substituters = https://cache.nixos.org/ https://nixos-devenv.cachix.org' > etc/nix/nix.conf
  echo 'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nixos-devenv.cachix.org-1:TfcIbSCGLCufAt9UCxzBTi3ekrzgI3HAHX73VWpByoE=' > etc/nix/nix.conf

  tar --sort=name --mtime='@1' --owner=0 --group=0 --numeric-owner -c * | ${compressCommand} > $out/rootfs.tar${extension}
''

