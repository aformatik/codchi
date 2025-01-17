{ lib, pkgs, config, ... }:
let
  inherit (lib) types mkOption;

  CYAN_BOLD = "\\e[1;36m";
  ENDCOLOR = "\\e[0m";
  welcomeMessage = ''
    echo -e "❄️ Welcome to your new code machine $CODCHI_MACHINE_NAME! ❄️"
    echo
    echo -e "To get started with ${CYAN_BOLD}Codchi${ENDCOLOR}, read the docs at <https://codchi.dev/>."
    echo -e "If you encounter problems please open an issue at <https://github.com/aformatik/codchi/issues>."
    echo
  '';
in
{
  options.codchi.initScript = mkOption {
    description = ''
      A bash script which will run once on machine creation (init or clone) as the default codchi user. Afterwards it can be run manually via `codchi-init`.

      To override the default welcome message use `lib.mkForce`.
    '';
    type = types.nullOr types.lines;
    example = ''
      cd $HOME
      git clone https://github.com/my/cool-repo
    '';
    defaultText = welcomeMessage;
  };

  config = {
    codchi.initScript = lib.mkBefore welcomeMessage;

    environment.systemPackages = [ (pkgs.writeShellScriptBin "codchi-init" config.codchi.initScript) ];
  };

}
