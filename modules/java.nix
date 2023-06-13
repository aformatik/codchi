{ config, options, lib, pkgs, ... }:
let
  inherit (lib) mkOption mkEnableOption mkMerge mkIf literalExpression types;

  jdkPath = "/home/${config.devenv.user}/.jdks";
  cfg = config.devenv.java;
in
{

  options.devenv.java = {
    enable = mkEnableOption "java" // {
      description = ''
        Adds Java and IDEs to the development environment.
      '';
    };
    package = mkOption {
      type = types.package;
      description = "Adds a JDK to $PATH and sets $JAVA_HOME.";
      default = pkgs.jdk;
    };
    jdks = mkOption {
      type = types.attrsOf types.package;
      description = ''
        List of JDKs which should be symlinked to ~/.jdks (for IDEs).
        In addition to the JDKs in nixpkgs devenv adds openjdk20 and
        temurin-bin-20 to `pkgs`.
      '';
      example = literalExpression ''{
        openjdk19 = pkgs.jdk19;
      }'';
      default = {};
    };
    tools = mkOption {
      type = types.listOf types.package;
      description = ''
        List of build tools and IDEs to install
      '';
      example = literalExpression ''with pkgs; [ maven gradle jetbrains.idea-ultimate ]'';
      default = [];
    };
  };

  config = mkMerge [
    {
      # Add OpenJDK20
      nixpkgs.overlays = [
        (_: _:
          let
            nixpkgs-openjdk20 = import
              (pkgs.fetchFromGitHub {
                owner = "htngr";
                repo = "nixpkgs";
                rev = "openjdk20";
                sha256 = "sha256-TSKcpCS7EDCHGxxMImkhVn3V1Hwxbq9uQ3SWjKMfSnA=";
              })
              { inherit (pkgs) system; };
          in
          {
            inherit (nixpkgs-openjdk20)
              jdk20
              jdk20_headless
              openjdk20
              openjdk20_headless
              temurin-bin-20;
          })
      ];
    }

    (mkIf cfg.enable {
      system.activationScripts.linkJDKs =
        let
          linkJDK = lib.mapAttrsToList (name: path:
            "ln -fs ${path}/lib/openjdk ${jdkPath}/${name}");
        in
        ''
          [ -d "${jdkPath}" ] && rm -rf "${jdkPath}"
          mkdir -p "${jdkPath}"
          ${lib.concatStringsSep "\n" (linkJDK cfg.jdks)}
        '';

      environment.systemPackages = cfg.tools;

      programs.java = {
        enable = true;
        inherit (cfg) package;
      };
    })
  ];

}
