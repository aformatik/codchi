(import <nixpkgs/nixos> {
  configuration = {
    imports = [
      ({ modulesPath, ... }: { imports = [ (modulesPath + "/profiles/minimal.nix") ]; })
      ({ pkgs, lib, ... }: {
        config = {
          environment.systemPackages = [
            pkgs.coreutils
            pkgs.python3
            pkgs.wget

            pkgs.jetbrains.idea-community
          ];

          boot.specialFileSystems = lib.mkForce { };
        };
      })
    ];

    config = {
      boot.isContainer = true;
      networking.hostName = "";
      services.journald.console = "/dev/console";
      users.mutableUsers = false;
      #users.allowNoPasswordLogin = true;
      services.getty.autologinUser = "root";
      users.users.root.hashedPassword = "";
      #systemd.services.systemd-logind.enable = false;
      #systemd.services.console-getty.enable = false;
      # Setuid wrappers do not work without this hack:


      # Disable a ton of stuff we don't need
      networking.dhcpcd.enable = false;
      systemd.oomd.enable = false;
      services.nscd.enableNsncd = false;
      networking.firewall.enable = false;
      services.openssh.startWhenNeeded = false;
      services.lvm.enable = false;
    };
  };
}).system

