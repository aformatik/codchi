{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkOption mkIf types;
  welcomeMessage = pkgs.writeText "nixos-wsl-welcome-message" ''
    ${config.codchi.welcome.text}
    ${config.codchi.welcome.extraText}
  '';
  welcome = pkgs.writeShellScriptBin "codchi-welcome" ''
    echo -e "$(cat ${welcomeMessage})"
  '';

  CYAN_BOLD = "\\e[1;36m";
  ENDCOLOR = "\\e[0m";
in
{
  options.codchi.welcome = {
    enable = mkEnableOption "Codchi's welcome message"
      // { default = true; };

    text = mkOption {
      description = ''
        Codchi's default message which is shown when opening a shell. Use `codchi.welcome.extraText` to append your own message. Only override this if you don't wan't Codchi to show the default text.
      '';
      type = types.lines;
      default = ''
        ❄️ Welcome to your code machine! ❄️

        To get started with ${CYAN_BOLD}Codchi${ENDCOLOR}, read the docs at <https://codchi.dev/>.
        If you encounter problems please open an issue at <https://github.com/aformatik/codchi/issues>.
      '';
    };
    extraText = mkOption {
      type = types.lines;
      description = ''
        A message which is appended to `codchi.welcome.text` and shown when opening a shell.
      '';
      default = "";
    };
  };

  config = mkIf config.codchi.welcome.enable {
    environment.systemPackages = [ welcome ];
    environment.loginShellInit = /* bash */ ''
      # only display welcome for humans
      if [ -t 0 ] && [ -z "''${__CODCHI_WELCOME:-}" ]; then
        export __CODCHI_WELCOME=done
        ${lib.getExe welcome}
      fi
    '';
  };
}

