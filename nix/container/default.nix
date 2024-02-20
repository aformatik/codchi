{ lib, pkgs, inputs }:
{ config, specialArgs ? { } }:

lib.evalModules {
  inherit specialArgs;
  modules = [

    {
      _module.args = {
        inherit inputs;
        pkgs = pkgs.extend (import ./pkgs);
      };
    }

    ./module.nix
    ./consts.nix

    ./store
    ./machine

    config

  ];
}
