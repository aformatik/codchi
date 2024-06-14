{ pkgs, lib, ... }:
let
  welcomeMessage = pkgs.writeText "nixos-wsl-welcome-message" ''
    Welcome to your new NixOS-WSL system!
    Please run `sudo nix-channel --update` and `sudo nixos-rebuild switch` now, to ensure you're running the latest NixOS and NixOS-WSL versions.
    If you run into issues, please report them on our Github page at https://github.com/nix-community/NixOS-WSL or come talk to us on Matrix at #wsl:nixos.org.
    ❄️ Enjoy NixOS-WSL! ❄️
    Note: this message will disappear after you rebuild your system. If you want to see it again, run `nixos-wsl-welcome`.

    This is your test secret: CODCHI_TEST=$CODCHI_TEST
  '';
  welcome = pkgs.writeShellScriptBin "nixos-wsl-welcome" ''
    cat ${welcomeMessage}
  '';
in
{

  codchi.welcome = ''
  
  '';
  codchi.secrets.env.TEST.description = ''
    This is a example secret.
  '';

  systemd.services.my-secret-service = {
    description = "My Service which reads codchi's secrets";
    wantedBy = [ "multi-user.target" ]; # Start this service in multi-user mode

    script = ''
      source /etc/codchi-env
      echo "Got secret CODCHI_TEST=$CODCHI_TEST from codchi"
    '';
  };

  programs.bash.loginShellInit = ''
    export CODCHI_GITLAB_TOKEN=aaaaaaaaaaaaaaaaaaaaaaaa

    echo Führe bitte dieses Script aus:
  '';

}
