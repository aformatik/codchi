{ config, pkgs, lib, ... }:
let
  systemdWrapper = pkgs.writeScript "sytemd-wrapper" ''
    #!/bin/sh
    set -ex

    if [ ! -d ${mnt.root}/wsl/nix/store ] || [ ! -d ${mnt.root}/wsl/nix/var/nix/daemon-socket ] ; then
      echo "Remote store is not mounted on ${mnt.root}/wsl/nix" >&2
      exit 1
    fi
    if [ ! -S ${mnt.root}/wsl/nix/var/nix/daemon-socket/socket ] ; then
      echo "Remote nix-daemon is not running." >&2
      exit 1
    fi

    mnt() {
      /bin/mount --bind "${mnt.root}/wsl/$1" "/$1"
    }
    mnt_ro() {
      /bin/mount --bind --ro "${mnt.root}/wsl/$1" "/$1"
    }

    echo "Mounting remote /nix/store..."
    /bin/mkdir -p /nix/store || true
    /bin/mkdir -p /nix/var/nix/{daemon-socket,db,gc-socket,gcroots} || true
    mnt_ro nix/store
    mnt_ro nix/var/nix/db
    mnt_ro nix/var/nix/gcroots
    mnt nix/var/nix/gc-socket
    mnt nix/var/nix/daemon-socket

    echo "Starting systemd..."
    exec ${pkgs.wslNativeUtils}/bin/systemd-shim "$@"
  '';


  staticFiles = [
    { inherit (config.environment.etc."wsl.conf") source; target = "/etc/wsl.conf"; }
    { source = systemdWrapper; target = "/sbin/init"; }
    { source = "${pkgs.pkgsStatic.busybox}/bin/busybox"; target = "/bin/mount"; }
    { source = "${pkgs.pkgsStatic.busybox}/bin/busybox"; target = "/bin/mkdir"; }
    { source = "${pkgs.pkgsStatic.bash}/bin/bash"; target = "/bin/sh"; }
  ];

  cfg = config.wsl;
  mnt = cfg.wslConf.automount;

  inherit (lib) mkForce stringAfter pipe concatStringsSep;
in
{

  wsl = {
    enable = true;
    nativeSystemd = true;
    startMenuLaunchers = false;
    wslConf.automount.enabled = true;
  };

  environment.etc."wsl.conf".enable = mkForce false;
  systemd.services.nix-daemon.enable = mkForce false;

  system.activationScripts = {
    populateBin = mkForce (stringAfter [ "etc" ] ''
      echo "setting up files for WSL..."
      ln -sf /init /bin/wslpath
      ${pipe staticFiles [
        (map ({source, target}: "cp -af ${source} ${target}"))
        (concatStringsSep "\n")
      ]}
    '');

    # disable unneeded upstream NixOS-WSL scripts
    # moved to staticFiles
    shimSystemd.text = mkForce "";
    # TODO: why does upstream use this?
    setupLogin.text = mkForce "";
    # only needed for upstream CI tests
    update-entry-point.text = mkForce "";
  };

  system.build.devenv.tarball = pkgs.callPackage "${pkgs.path}/nixos/lib/make-system-tarball.nix" {

    contents = staticFiles;

    fileName = "devenv-${pkgs.hostPlatform.system}";

    # Build NixOS configuration, but dont add it to tarball since we mount the
    # builders' store into the new configuration
    extraInputs = [ config.system.build.toplevel ];

    compressCommand = "cat";
    compressionExtension = "";
  };

}
