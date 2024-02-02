{ lib, pkgs, inputs }:
{ config, specialArgs ? { } }:

lib.evalModules {
  inherit specialArgs;
  modules = [
    config

    ({ consts, ... }: {
      _module.args = {
        inherit pkgs inputs;

        # TODO unify variables with rust cli
        consts = {
          DIR_CONFIG = "/config";
          DIR_DATA = "/data";
          DIR_CONFIG_STORE = "${consts.DIR_CONFIG}/store";
          PROFILE_STORE = "${consts.DIR_CONFIG_STORE}/profile";
        };
      };
    })

    ./scripts.nix
    ./system.nix

    ./lxd.nix
    ./wsl.nix

  ];
}
