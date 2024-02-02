{ config, lib, pkgs, ... }:
let
  inherit (lib) types
    literalExpression mdDoc stringAfter;
  mkOption = args: lib.mkOption ({ internal = true; } // args);
  rootfsType = with types; attrsOf (nullOr (oneOf [ str package ]));

  cfg = config.codchi.internal;

  rootfs = pkgs.callPackage ../../nix/make-tarball.nix {
    # this filename ($DRV/rootfs.tar) is used by codchi during instance
    # registration with a driver
    fileName = "rootfs";
    inherit (cfg.init) overrideContents;
    contents = cfg.init.rootfsContents;
    compressCommand = "cat";
    compressionExtension = "";
  };
in
{
  options = {
    codchi.internal = {
      init = {
        rootfsContents = mkOption {
          type = rootfsType;
          description = mdDoc ''
            Files which are copied into the tarball (when building
            `config.system.build.tarball`) or into a code machine during
            activation.
          
            Attribute name is the target path which will be created if non
            existant. Directories must be given with a trailing `/`.
            Attribute value can be either `null` or a space separated
            list of store paths (which can include globs) which are copied to the
            target directory.
          '';
          example = literalExpression ''{
            "/etc/hosts" = pkgs.writeText "hosts" ...;

            # creates empty /dev
            "/dev/" = null;

            # creates /bin with binaries from static busybox and bash
            "/bin/" = "''${pkgs.pkgsStatic.busybox}/bin/* ''${pkgs.pkgsStatic.bash}/bin/*";
          }'';
        };
        overrideContents = mkOption {
          type = types.functionTo rootfsType;
          default = x: x;
          description = mdDoc ''
            Function which takes `codchi.internal.init.rootfsContents` and
            return the contents of the tarball which is installed by a codchi
            driver. This can be used to add metadata or other files which are
            needed by a particular driver.
          '';
        };
      };
    };
    system.build.codchi.rootfs = mkOption {
      type = types.package;
      readOnly = true;
      description = mdDoc ''
        Derivation which can be installed by codchi. Contains following files:

        - `./rootfs.tar`: the code machine rootfs
        - `./system-store-path`: a text file with the store path to
          `config.system.build.toplevel`
      '';
    };
  };

  config = {

    # Sync files required before NixOS boots (files from
    # codchi.internal.init.rootfsContents)
    system.activationScripts.codchi-rootfs = stringAfter [ "etc" ] ''
      pushd /
      ${rootfs.passthru.createContents}/bin/create-contents
      popd
    '';

    system.build.codchi.rootfs = pkgs.runCommandLocal "codchi-instance-rootfs" { } ''
      cp -a ${rootfs} $out
      chmod +w $out
      echo ${config.system.build.toplevel} > $out/system-store-path
    '';
  };
}
