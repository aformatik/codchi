{ config, lib, ... }:
let
  inherit (lib) mkOption mkIf literalExpression types;

  jdkPath = "/home/${config.codchi.defaultUser}/.jdks";
  cfg = config.programs.java;
in
{

  options.programs.java.packages = mkOption {
    type = types.attrsOf types.package;
    description = ''
      List of JDKs which should be symlinked to ~/.jdks (for IDEs lik IntelliJ).
    '';
    example = literalExpression ''{
        openjdk19 = pkgs.jdk19;
      }'';
    default = { };
  };

  config = mkIf cfg.enable {
    system.activationScripts.linkJDKs =
      let
        linkJDK = lib.mapAttrsToList (name: path:
          "ln -fs ${path}/lib/openjdk ${jdkPath}/${name}");
      in
      ''
        [ -d "${jdkPath}" ] && rm -rf "${jdkPath}"
        mkdir -p "${jdkPath}"
        ${lib.concatStringsSep "\n" (linkJDK cfg.packages)}
      '';
  };

}
