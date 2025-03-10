{ config, lib, pkgs, ... }:
let
  inherit (lib) types mkOption;
in
{
  options = {
    name = mkOption {
      type = types.str;
      description = ''
        Name of this container.
      '';
    };
    binPackages = mkOption {
      type = with types; listOf package;
      default = [ ];
      example = lib.literalExpression "[ pkgs.pkgsStatic.bashInteractive pkgs.pkgsStatic.busybox ]";
      description = ''
        The set of packages that are copied to /bin. These must be static
        binaries as they can not rely on an existing /nix/store.
      '';
    };
    runtimePackages = mkOption {
      type = with types; listOf package;
      default = [ ];
      example = lib.literalExpression "[ pkgs.git pkgs.openssh ]";
      description = ''
        The set of packages that are installed to the nix store. They must be
        installed manually (e.g. via `nix profile install` / `environment.systemPackages`).
      '';
    };
    files = mkOption {
      type = with types; attrsOf (nullOr path);
      description = ''
        Set of files that have to be copied to `/`. Deactivate a file by
        setting it to `null`.
      '';
      default = { };
      example = lib.literalExpression ''
        { 
          "sbin/init" = static.writeShellScript "init" '''
            ...
          ''';
          "etc/wsl.conf" = lib.mkForce null;
        }
      '';
    };

    # from <nixpkgs>/nixos/modules/system/build.nix
    build = lib.mkOption {
      default = { };
      description = ''
        Attribute set of derivations used to set up the system.
      '';
      type = with lib.types; submoduleWith {
        modules = [{
          freeformType = lazyAttrsOf (uniq unspecified);
          options = {
            tarball = mkOption {
              type = package;
              description = "The system tarball.";
              readOnly = true;
            };
            tarballExtraCommands = mkOption {
              type = types.lines;
              description = "Extra shell commands to run on before system tar file generation.";
              default = "";
            };
          };
        }];
      };
    };
  };


  config.build = {
    runtime = pkgs.buildEnv {
      name = "runtime-env";
      pathsToLink = [ "/bin" ];
      paths = config.runtimePackages;
    };
    tarball = pkgs.makeTarball {
      fileName = config.name;
      contents = [
        (pkgs.buildEnvCopy {
          name = "bin";
          pathsToLink = [ "/bin" ];
          paths = config.binPackages;
        })
        (pkgs.buildHierarchy config.files)
      ];
      extraCommands = config.build.tarballExtraCommands;
    };
  };

}
