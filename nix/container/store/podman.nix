{ lib, config, pkgs, consts, ... }:
let inherit (lib) mkEnableOption mkIf;

  # udhcpcScript =
  #   let
  #     # taken from https://github.com/NixOS/nixpkgs/blob/4c8cf44c5b9481a4f093f1df3b8b7ba997a7c760/pkgs/os-specific/linux/busybox/default.nix#L36C3-L46C5:
  #     debianVersion = "1.30.1-6";
  #     debianSource = pkgs.fetchFromGitLab {
  #       domain = "salsa.debian.org";
  #       owner = "installer-team";
  #       repo = "busybox";
  #       rev = "debian/1%${debianVersion}";
  #       sha256 = "sha256-6r0RXtmqGXtJbvLSD1Ma1xpqR8oXL2bBKaUE/cSENL8=";
  #     };
  #   in
  #   "${debianSource}/debian/tree/udhcpc/etc/udhcpc/default.script";
in
{

  options.store.driver.podman = {
    enable = mkEnableOption "Podman specific settings";
  };

  config = mkIf config.store.driver.podman.enable {
    # files."/usr/share/udhcpc/default.script" = udhcpcScript;

    store.init.filesystem = lib.mkAfter /* bash */ ''
      mkdir -p ${consts.store.DIR_LOG} || true
      touch "${consts.store.LOGFILE}"
      # syslogd -O "${consts.store.LOGFILE}"
      exec 1> >(tee -i "${consts.store.LOGFILE}") 2>&1
      # udhcpc -S -s /usr/share/udhcpc/default.script
    '';

    build.dockerImage = pkgs.dockerTools.buildImage {
      name = "codchi-store";
      extraCommands = ''
        tar xzf ${config.build.tarball}
      '';
      config = {
        Cmd = [ "/sbin/init" ];
        Entrypoint = [ "/bin/run" ];
        # Volumes = { "/nix" = { }; };
      };
    };
  };
}
