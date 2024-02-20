{ config, pkgs, lib, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.codchi.driver.wsl;
in
{

  imports = [ ./default-tools.nix ];

  options.codchi.driver.wsl.enable = mkEnableOption "WSL driver"
    // { internal = true; readonly = true; };

  config = mkIf cfg.enable {

    codchi.driver.name = "wsl";

    environment.etc = {
      hosts.enable = false;
      "resolv.conf".enable = false;
    };
    networking.dhcpcd.enable = false; # dhcp is handled by windows

    # Otherwise WSL fails to login as root with "initgroups failed 5"
    # TODO check if still issue with systemd
    users.users.root.extraGroups = [ "root" ];

    # Allow executing .exe files from WSL
    boot.binfmt.registrations = {
      WSLInterop = {
        magicOrExtension = "MZ";
        fixBinary = true;
        wrapInterpreterInShell = false;
        interpreter = "/init";
        preserveArgvZero = true;
      };
    };

    # prevent clockshift
    services.timesyncd.enable = true;
    systemd.services.systemd-timesyncd.unitConfig.ConditionVirtualization = "";

    systemd.tmpfiles.settings = {
      # Link the X11 socket into place. This is a no-op on a normal setup,
      # but helps if /tmp is a tmpfs or mounted from some other location.
      "10-wslg-x11" = {
        "/tmp/.X11-unix" = {
          L = {
            argument = "/mnt/wslg/.X11-unix";
          };
        };
      };
      "11-wslpath" = {
        "/bin/wslpath" = {
          L = {
            argument = "/init";
          };
        };
      };
    };

    environment.variables = {
      LIBGL_ALWAYS_INDIRECT = "1"; # Allow OpenGL in WSL
      DONT_PROMPT_WSL_INSTALL = "1"; # Don't prompt for VS code server when running `code`

      # https://wiki.archlinux.org/title/Java#Better_font_rendering TODO include by default?
      JDK_JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on, -Dswing.aatext=true";
    };

  };

}
