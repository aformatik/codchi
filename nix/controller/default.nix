{ writeScript
, writeText
, callPackage

, pkgsStatic
, cacert
, ...
}:

let
  binaries = with pkgsStatic; [
    busybox
    bashInteractive
    nix
    jq
    (gitMinimal.overrideAttrs (old: with pkgsStatic; {
      configureFlags = lib.filter (f: !(lib.hasPrefix "ac_cv_prog_CURL_CONFIG" f)) old.configureFlags;
      preConfigure = ''
        export NIX_LDFLAGS="$NIX_LDFLAGS $(${curl.dev}/bin/curl-config --static-libs | sed 's/-pthread //' |  tr ' ' '\n' |  awk '!a[$0]++' |  tr '\n' ' ')"
      '';
      preInstallCheck = old.preInstallCheck + ''
        disable_test t2082-parallel-checkout-attributes
      '';
    }))
    # openssh
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
    "/bin/" = toString (map (pkg: "${pkg}/bin/*") binaries);

    "/etc/profile.d/" = "${pkgsStatic.nix}/etc/profile.d/*";
    "/etc/ssl/certs/official-bundle.crt" = "${cacert}/etc/ssl/certs/ca-bundle.crt";
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
    "/bin/ctrl-update-certs" = writeShellScript "ctrl-update-certs" ''
      mkdir -p /etc/ssl/certs/custom || true
      cp -f /etc/ssl/certs/official-bundle.crt /etc/ssl/certs/ca-bundle.crt
      for cert in $(find /etc/ssl/certs/custom/ -name '*.crt'); do
        cat "$cert" >> /etc/ssl/certs/ca-bundle.crt
      done
    '';
  };
}
