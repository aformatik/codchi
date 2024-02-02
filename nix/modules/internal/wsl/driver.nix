{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkRemovedOptionModule stringAfter;

  cfg = config.codchi.internal.wsl;
in
{

  imports = [
    (mkRemovedOptionModule [ "wsl" "docker-native" ] "WSL specific docker options are no longer supported. Please use `virtualisation.docker` directly.")
    (mkRemovedOptionModule [ "wsl" "docker-desktop" ] "WSL specific options are no longer supported.")
  ];

  config = mkIf cfg.enable {

    system.activationScripts = {
      setup-wsl = stringAfter [ "etc" ] ''
        ln -sf /init /bin/wslpath
      '';
    };

    # Allow executing .exe files from codchi
    boot.binfmt.registrations = {
      WSLInterop = {
        magicOrExtension = "MZ";
        fixBinary = true;
        wrapInterpreterInShell = false;
        interpreter = "/init";
        preserveArgvZero = true;
      };
    };

    # WSL uses its own kernel and boot loader
    boot = {
      initrd.enable = false;
      kernel.enable = false;
      loader.grub.enable = false;
      modprobeConfig.enable = false;
    };
    system.build.installBootLoader = "${pkgs.coreutils}/bin/true";
    console.enable = false; # WSL does not support virtual consoles
    hardware.opengl.enable = true; # Enable GPU acceleration

    environment.etc = {
      hosts.enable = false;
      "resolv.conf".enable = false;
    };

    networking.dhcpcd.enable = false; # dhcp is handled by windows

    # Otherwise WSL fails to login as root with "initgroups failed 5"
    users.users.root.extraGroups = [ "root" ];

    powerManagement.enable = false;

    # useful for usbip but adds a dependency on various firmwares which are combined over 300 MB big
    services.udev.enable = lib.mkDefault false;

    # Disable systemd units that don't make sense on WSL
    systemd = {
      services = {
        firewall.enable = false;
        systemd-resolved.enable = lib.mkDefault false;
        # systemd-timesyncd actually works in WSL and without it the clock can drift
        systemd-timesyncd.unitConfig.ConditionVirtualization = "";
      };

      enableEmergencyMode = false; # Don't allow emergency mode, because we don't have a console.
      oomd.enable = false;
    };
  };


}
