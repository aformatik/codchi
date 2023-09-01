{ nixpkgs
, lib
, writeScript
, writeText
, callPackage

, pkgsStatic
, cacert
, coreutils
, iputils
, git
, openssh

, overrideContents ? prev: prev
, preServeHook ? ""
, ...
}:

let
  # These are binaries which are required by WSL before controller
  # initialization. They are included in the controller.tar.gz
  staticBinaries = with pkgsStatic; [
    busybox
    bashInteractive
    nix
  ];
  # These are binaries required at runtime and wil be installed dynamically
  otherBinaries = map builtins.unsafeDiscardStringContext [
    coreutils
    iputils
    git
    openssh
  ];

  writeShellScript = name: text: writeScript name ''
    #!/bin/bash

    set -e
    set -o pipefail

    export PATH="$PATH:/bin"

    ${text}
  '';
in
callPackage ../nix/make-tarball.nix {
  fileName = "controller";

  inherit overrideContents;
  contents = {
    "/bin/" = toString (map (pkg: "${pkg}/bin/*") staticBinaries);
    "/bin/ctrl-serve" = writeShellScript "ctrl-serve" ''
      ${preServeHook}

      if pgrep nix-daemon; then
        echo "Nix-daemon is already running" >&2
        exit 1
      fi
      exec nix-daemon
    '';

    "/bin/ctrl-install" = writeShellScript "ctrl-install" ''
      NAME="$1"

      nix flake update "/instances/$NAME"
      DRV=$(nix build --impure --print-out-paths --no-link "/instances/$NAME#nixosConfigurations.default.config.system.build.codchi.rootfs")
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
      mkdir -p /etc/ssl/certs/custom || true
      cp -f /etc/ssl/certs/official-bundle.crt /etc/ssl/certs/ca-bundle.crt
      for cert in $(find /etc/ssl/certs/custom/ -name '*.crt'); do
        cat "$cert" >> /etc/ssl/certs/ca-bundle.crt
      done
    '';

    "/etc/profile.d/nix-daemon.sh" = "${pkgsStatic.nix}/etc/profile.d/nix-daemon.sh";
    "/etc/ssl/certs/official-bundle.crt" = "${cacert}/etc/ssl/certs/ca-bundle.crt";

    "/etc/" = "${./etc}/*";
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

    "/root/.bash_profile" = writeText ".bashrc" ''
      ctrl-update-certs
      source /etc/profile.d/nix-daemon.sh
    '';
  };
}
