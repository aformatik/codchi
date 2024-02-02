{ config, lib, pkgs, inputs, ... }:
let
  inherit (lib) types mkOption;

  cfg = config.system;
in
{
  options.system = {
    name = mkOption {
      type = types.str;
      description = ''
        Name of this store system.
      '';
      default = "store";
    };
    binPackages = mkOption {
      type = with types; listOf package;
      default = [ ];
      example = lib.literalExpression "[ pkgs.pkgsStatic.bashInteractive pkgs.pkgsStatic.busybox ]";
      description = lib.mdDoc ''
        The set of packages that are copied to /bin. These must be static
        binaries as they can not rely on an existing /nix/store.
      '';
    };
    runtimePackages = mkOption {
      type = with types; listOf package;
      default = [ ];
      example = lib.literalExpression "[ pkgs.git pkgs.openssh ]";
      description = lib.mdDoc ''
        The set of packages that are installed to the nix store. They're
        installed via `nix profile install`.
      '';
    };
    files = mkOption {
      type = with types; attrsOf (nullOr path);
      description = lib.mdDoc ''
        Set of files that have to be copied to {file}`/`. Deactivate a file by
        setting it to {nix}`null`.
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
      description = lib.mdDoc ''
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


  config.system = {
    binPackages = with pkgs.pkgsStatic; [
      busybox
      bashInteractive
      nix
    ];

    runtimePackages = with pkgs; [
      coreutils
      # iputils
      git
      openssh
    ];

    files = {
      # user & groups required for minimal linux + `nix daemon`
      "/etc/group" = ./etc/group;
      "/etc/passwd" = ./etc/passwd;
      # required for dns / other information lookup systems (mainly glibc)
      "/etc/nsswitch.conf" = ./etc/nsswitch.conf;
      # nix settings
      "/etc/nix/nix.conf" = ./etc/nix/nix.conf;
      # force the nix registry to use the nixpkgs version from this repo
      "/etc/nix/registry.json" = pkgs.writeText "registry.json" (builtins.toJSON {
        version = 2;
        flakes = [{
          exact = true;
          from = { type = "indirect"; id = "nixpkgs"; };
          to = { type = "path"; path = inputs.nixpkgs.outPath; }
            // lib.filterAttrs
            (n: _: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
            inputs.nixpkgs;
        }];
      });
    };

    build = {
      runtime = pkgs.buildEnv {
        name = "runtime-env";
        pathsToLink = [ "/bin" ];
        paths = cfg.runtimePackages;
      };
      tarball = pkgs.makeTarball {
        fileName = cfg.name;
        contents = [
          (pkgs.buildEnvCopy {
            name = "bin";
            pathsToLink = [ "/bin" ];
            paths = cfg.binPackages;
          })
          (pkgs.buildHierarchy cfg.files)
        ];
        extraCommands = config.system.build.tarballExtraCommands;
      };
    };

  };
}
