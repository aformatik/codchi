{ config, lib, ... }:
let
  inherit (lib) mkOption mkIf literalExpression types;

  cfg = config.programs.java;
in
{

  options.programs.java.packages = mkOption {
    type = types.attrsOf types.package;
    description = ''
      List of JDKs which should be symlinked to ~/.jdks (for IDEs lik IntelliJ).
    '';
    example = literalExpression /* nix */ ''
      {
        openjdk19 = pkgs.jdk19;
      }'';
    default = { };
  };

  config = mkIf cfg.enable {
    systemd.tmpfiles.settings."codchi-jdks" =
      {
        "${config.users.users.codchi.home}/.jdks".D = {
          group = "users";
          mode = "0555"; # ro for everyone
          user = "codchi";
        };
      }
      //
      lib.mapAttrs'
        (name: path:
          lib.nameValuePair
            "${config.users.users.codchi.home}/.jdks/${name}"
            { "L+".argument = "${path}/lib/openjdk"; }
        )
        cfg.packages;

  };

}
