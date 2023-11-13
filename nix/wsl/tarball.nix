{ config, pkgs, lib, ... }:
let
  cfg = config.wsl;
  mnt = cfg.wslConf.automount;

  contents = {
    "/bin/" = with pkgs.pkgsStatic; toString (map (pkg: "${pkg}/bin/*") [ busybox bashInteractive ]);

    "/etc/wsl.conf" = config.environment.etc."wsl.conf".source;

    "/sbin/init" = pkgs.writeScript "sytemd-wrapper" ''
      #!/bin/bash
      set -e
      set -o pipefail
      # this logs everything to dmesg
      set -x

      PATH="$PATH:/bin"

      if [ ! -d ${mnt.root}/wsl/nix/store ] || [ ! -d ${mnt.root}/wsl/nix/var/nix/daemon-socket ] ; then
        echo "Remote store is not mounted on ${mnt.root}/wsl/nix" >&2
        exit 1
      fi
      if [ ! -S ${mnt.root}/wsl/nix/var/nix/daemon-socket/socket ] ; then
        echo "Remote nix-daemon is not running." >&2
        exit 1
      fi

      mnt() {
        echo "Mounting remote /$1..."
        mkdir -p "/$1" || true
        mount --bind "${mnt.root}/wsl/$1" "/$1"

        trap "umount -f \"/$1\"" EXIT
      }

      mnt nix/store

      for file in $(ls "${mnt.root}/wsl/nix/var/nix/"); do
        MNT_DIR="${mnt.root}/wsl/nix/var/nix/$file" 
        DIR="/nix/var/nix/$file" 
        if [ -d "$MNT_DIR" ] && [ "$file" != "profiles" ]; then
          mnt "$DIR"
        fi
      done

      echo "Mounting profile for ${config.codchi.instance.name}..."
      mkdir -p /nix/var/nix/profiles || true
      mount --bind "${mnt.root}/wsl/nix/var/nix/profiles/per-instance/${config.codchi.instance.name}" "/nix/var/nix/profiles"
      trap "umount -f /nix/var/nix/profiles" EXIT

      echo "Starting systemd..."
      exec ${pkgs.wslNativeUtils}/bin/systemd-shim "$@"
    '';
  };

  tarball = pkgs.callPackage ../make-tarball.nix {

    fileName = "rootfs";

    inherit contents;

    compressCommand = "cat";
    compressionExtension = "";
  };

  inherit (lib) mkForce stringAfter;

in
{

  wsl = {
    enable = true;
    nativeSystemd = true;
    startMenuLaunchers = false;
    wslConf.automount.enabled = true;
    inherit (config.codchi) defaultUser;
  };

  environment.variables.NIX_REMOTE = "daemon";
  environment.etc."wsl.conf".enable = mkForce false;
  systemd.services.nix-daemon.enable = mkForce false;

  system.activationScripts = {
    populateBin = mkForce (stringAfter [ "etc" ] ''
      echo "setting up files for WSL..."
      ln -sf /init /bin/wslpath

      pushd /
      ${tarball.passthru.createContents}/bin/create-contents
      popd

    '');

    # disable unneeded upstream NixOS-WSL scripts
    # moved to staticFiles
    shimSystemd.text = mkForce "";
    # TODO: why does upstream use this?
    setupLogin.text = mkForce "";
    # only needed for upstream CI tests
    update-entry-point.text = mkForce "";
  };

  system.build.wsl-tarball = pkgs.runCommandLocal "wsl-tarball" {} ''
    cp -a ${tarball} $out
    chmod +w $out
    echo ${config.system.build.toplevel} > $out/system-store-path
  '';
}
