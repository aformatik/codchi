{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.codchi.driver.wsl;
in
{

  imports = [ ./host-integration.nix ];

  options.codchi.driver.wsl.enable = mkEnableOption "WSL driver"
    // { internal = true; readonly = true; };

  config = mkIf cfg.enable {

    codchi.driver = {
      name = "wsl";
      iconCommand = lib.mkDefault ''
        ${pkgs.imagemagick}/bin/convert \
          -background transparent \
          -define icon:auto-resize=16,24,32,48,64,72,96,128,256 \
          "$ICON_PATH" "codchi/icons/$APP_NAME.ico"
      '';
      containerCfg = {
        machine.driver.wsl.wslConf.network = {
          generateHosts = !config.environment.etc.hosts.enable;
          generateResolvConf = !config.environment.etc."resolv.conf".enable;
        };
      };
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
    systemd.services.systemd-timesyncd.unitConfig.ConditionVirtualization = "";

    systemd.tmpfiles.settings = {
      # Link the X11 socket into place. This is a no-op on a normal setup,
      # but helps if /tmp is a tmpfs or mounted from some other location.
      "10-wslg-x11" = {
        "/tmp/.X11-unix" = {
          L.argument = "/mnt/wslg/.X11-unix";
        };
      };
      # "11-wslpath" = {
      #   "/bin/wslpath" = {
      #     L = {
      #       argument = "/init";
      #     };
      #   };
      # };
    };
    environment = {
      systemPackages = [
        (pkgs.writeShellScriptBin "wslpath" ''
          ${lib.getExe pkgs.bashInteractive} -c "exec -a wslpath /init $@"
        '')
      ];
      etc = {
        # use our own /etc/hosts by default
        hosts.enable = lib.mkDefault true;
        # DNS by windows
        "resolv.conf".enable = lib.mkDefault false;
      };
      variables = {
        LIBGL_ALWAYS_INDIRECT = "1"; # Allow OpenGL in WSL
        DONT_PROMPT_WSL_INSTALL = "1"; # Don't prompt for VS code server when running `code`

        # https://wiki.archlinux.org/title/Java#Better_font_rendering TODO include by default?
        JDK_JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on, -Dswing.aatext=true";
      };
      extraInit = /* bash */ ''
        # append Windows' path
        export PATH="$PATH:$HOST_PATH"

        # Configure VcXsrv
        if [ -n "''${CODCHI_WSL_USE_VCXSRV:-}" ]; then
          export DISPLAY=$(ip route | awk '/^default/{print $3; exit}'):0
          unset WAYLAND_DISPLAY
          export GDK_BACKEND=x11
        else
          export GDK_BACKEND=wayland
        fi
      '';
    };

    programs.bash.shellInit = lib.mkBefore ''
      # get windows path from WSL
      HOST_PATH="$PATH"
    '';

  };

}
