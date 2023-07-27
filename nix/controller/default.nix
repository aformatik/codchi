{ nixpkgs
, lib

, writeScript
, writeText
, callPackage

, pkgsStatic
, cacert

, coreutils
, iputils
, jq
, git
, openssh
, ...
}:

let
  staticBinaries = with pkgsStatic; [
    busybox
    bashInteractive
    nix
  ];
  otherBinaries = map builtins.unsafeDiscardStringContext [
    coreutils
    iputils
    jq
    git
    openssh
  ];

  writeShellScript = name: text: writeScript name ''
    #!/bin/bash

    set -e
    set -o pipefail

    ${text}
  '';
in
callPackage ../make-tarball.nix {
  fileName = "controller";

  contents = {
    "/bin/" = toString (map (pkg: "${pkg}/bin/*") staticBinaries);

    "/etc/profile.d/nix-daemon.sh" = "${pkgsStatic.nix}/etc/profile.d/nix-daemon.sh";
    "/etc/ssl/certs/official-bundle.crt" = "${cacert}/etc/ssl/certs/ca-bundle.crt";

    # docs: https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-registry.html#registry-format
    "/etc/nix/registry.json" = writeText "registry.json" (builtins.toJSON {
      version = 2;
      flakes = [{
        exact = true;
        from = { type = "indirect"; id = "nixpkgs"; };
        to = { type = "path"; path = nixpkgs.outPath; }
          // lib.filterAttrs
          (n: _: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
          nixpkgs;
      }];
    });
    "/etc/" = "${./etc}/*";

    "/root/.bash_profile" = writeText ".bashrc" ''
      set -e
      ctrl-update-certs
      . /etc/profile.d/nix-daemon.sh
    '';

    "/bin/ctrl-serve" = writeShellScript "ctrl-serve" ''

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

    "/bin/ctrl-install" = writeShellScript "ctrl-install" ''
      NAME="$1"

      nix flake update "/instances/$NAME"
      DRV=$(nix build --impure --print-out-paths --no-link "/instances/$NAME#nixosConfigurations.default.config.system.build.wsl-tarball")
      mkdir -p "/nix/var/nix/profiles/per-instance/$NAME"
      nix-env -p "/nix/var/nix/profiles/per-instance/$NAME/system" --set $(cat $DRV/system-store-path)

      echo "$DRV"
    '';
    "/bin/ctrl-update-profile" = writeShellScript "ctrl-update-profile" ''
      INSTALLED="$(nix profile list | cut -d' ' -f 4 | sort)"
      NEW="$(echo ${toString otherBinaries} | xargs -n1 | sort)"

      # remove whats only in $INSTALLED and not in $NEW
      comm -23 <(echo $INSTALLED | xargs -n1) <(echo $NEW | xargs -n1) | xargs -r nix profile remove
      # install whats only in $NEW and not in $INSTALLED
      comm -13 <(echo $INSTALLED | xargs -n1) <(echo $NEW | xargs -n1) | xargs -r nix profile install
    '';
    "/bin/ctrl-update-certs" = writeShellScript "ctrl-update-certs" ''
      # update certs
      mkdir -p /etc/ssl/certs/custom || true
      cp -f /etc/ssl/certs/official-bundle.crt /etc/ssl/certs/ca-bundle.crt
      for cert in $(find /etc/ssl/certs/custom/ -name '*.crt'); do
        cat "$cert" >> /etc/ssl/certs/ca-bundle.crt
      done
    '';
  };
}
