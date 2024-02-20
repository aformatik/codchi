{ inputs, lib, pkgs, config, consts, ... }:
let
  inherit (lib) mkOption types mkDefault mkForce mkMerge;
in
{
  imports = [
    ./lxd
    ./wsl
  ];

  options.codchi.driver.name = mkOption {
    type = types.str;
    internal = true;
  };

  config = mkMerge [

    # Nix(OS) stuff managed by codchi
    {
      system.build.codchi.container = (import ../../container { inherit inputs pkgs lib; }
        {
          config.machine = {
            enable = true;
            driver.${config.codchi.driver.name}.enable = true;
          };
        }).config.build.tarball;
      # Create files required by the driver
      system.activationScripts.codchi-create-files = lib.stringAfter [ "etc" ] /* bash */ ''
        ( cd / &&
          ${lib.getExe config.system.build.codchi.container.passthru.createFiles}
        )
      '';

      # Make sure all profiles are recorded as gcroots
      systemd.tmpfiles.rules = [
        "L+ /nix/var/nix/gcroots/profiles 0755 root root - /nix/var/nix/profiles"
      ];

      # disable nixos-rebuild
      system.disableInstallerTools = mkForce true;

      systemd = {
        services = {
          nix-daemon.enable = mkForce false;
          nix-gc.enable = mkForce false;
          nix-optimize.enable = mkForce false;
        };
        sockets.nix-daemon.enable = mkForce false;
      };

      environment.variables.NIX_REMOTE = "daemon";
      # Setup nix flakes
      nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
          experimental-features = nix-command flakes
        '';
        registry.nixpkgs.flake = inputs.nixpkgs;
        nixPath = [ "nixpkgs=/etc/channels/nixpkgs" ];
      };
      environment.etc."channels/nixpkgs".source = inputs.nixpkgs;
    }


    # general codchi machine management
    {
      boot.isContainer = true;

      # useful for usbip but adds a dependency on various firmwares which are combined over 300 MB big
      services.udev.enable = mkDefault false;

      networking.firewall.enable = mkDefault false;

      systemd = {
        # Don't allow emergency mode, because we don't have a console.
        enableEmergencyMode = false;
        # systemd-oomd requires cgroup pressure info which WSL doesn't have
        oomd.enable = false;
      };

      users.mutableUsers = mkForce false;
      users.users.${consts.machine.USER} = {
        isNormalUser = mkForce true;
        createHome = mkForce true;
        home = mkForce "/home/${consts.machine.USER}";
        uid = mkForce 1000;
        extraGroups = [ "wheel" ];
        initialPassword = consts.machine.USER;
      };
      security.sudo.wheelNeedsPassword = mkDefault false;
    }

    # Desktop stuff
    {
      xdg = mkDefault {
        autostart.enable = true;
        menus.enable = true;
        mime.enable = true;
        icons.enable = true;
      };
      fonts.enableDefaultPackages = mkDefault true;

      environment = {

        systemPackages = [
          pkgs.xdg-utils
          pkgs.nixos-icons # needed for gnome and pantheon about dialog, nixos-manual and maybe more
        ];

        # Copy .desktop files and corresponding icons (converted to .ico) to
        # /run/current-system/sw/share/codchi. We copy instead of symlink them so
        # that drivers like WSL can see them.
        extraSetup = /* bash */ ''
          pushd $out/share

          mkdir -p codchi/{icos,applications}

          for app_path in applications/*.desktop; do
            if grep -q ^NoDisplay=true "$app_path" || grep -q ^Hidden=true "$app_path"; then
              echo "Skipping hidden $app_path" >&2
            else
              APP_NAME="$(basename "$app_path" | sed 's/\.desktop//')"
              ICON_NAME="$(grep Icon "$app_path" | sed 's/Icon=//')"
              DIRS=
              [ -d icons ] && DIRS="$DIRS icons"
              [ -d pixmaps ] && DIRS="$DIRS pixmaps"
              ICON_PATH="$(find -L $DIRS -name "$ICON_NAME.*" | sort -rV | head -n1)"
              if [ ! -z "$ICON_PATH" ]; then
                ${pkgs.imagemagick}/bin/convert \
                  -background transparent \
                  -define icon:auto-resize=16,24,32,48,64,72,96,128,256 \
                  "$ICON_PATH" "codchi/icos/$APP_NAME.ico"
              fi
              cp "$app_path" codchi/applications
            fi
          done

          popd
        '';
      };

    }
  ];

}
