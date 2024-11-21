{ config, lib, pkgs, ... }:
let
  cfg = config.codchi.keyring;
in
{
  options.codchi.keyring = {
    enable = lib.mkEnableOption "a keyring for applications like IntelliJ";
  };

  config = lib.mkIf cfg.enable {

    services.gnome.gnome-keyring.enable = true;

    systemd.user.services.gnome-keyring = {
      wantedBy = [ "default.target" ];
      serviceConfig = {
        ExecStart =
          "${pkgs.gnome.gnome-keyring}/bin/gnome-keyring-daemon  --start --foreground --components=pkcs11,secrets,ssh";
        Restart = "on-abort";
      };
    };
  };
}
