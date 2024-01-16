rec {
  # stolen from nixpkgs/lib/module.nix
  applyModuleArgs = f: args@{ config, lib, ... }:
    let
      context = name: ''while evaluating the module argument `${name}':'';
      extraArgs = builtins.mapAttrs
        (name: _:
          builtins.addErrorContext (context name)
            (args.${name} or config._module.args.${name})
        )
        (lib.functionArgs f);
    in
    f (args // extraArgs);

  # Overide module args per import
  # overrideModuleArgs  :: Attr Set -> NixOS Module -> NixOS Module
  overrideModuleArgs = specialArgs: module:
    prevArgs:
    let
      importedModuleUncalled =
        if builtins.isPath module then import module
        else if builtins.isAttrs module then _: module
        else module;

      importedModule = applyModuleArgs importedModuleUncalled (prevArgs // specialArgs);
    in
    importedModule //
    { imports = map (overrideModuleArgs specialArgs) (importedModule.imports or [ ]); }
  ;

  # codeMachine :: Nixos Configuration
  codeMachine =
    {
      # driver :: null | "wsl" | "lxd"
      # null is for migrating away from codchi
      driver
      # system :: "x86_64-linux" | "aarch64-linux"
    , system
      # nixpkgs :: Flake
    , nixpkgs
      # codchiModules :: [{ module :: NixOS Module, extraArgs :: Attr Set }]
    , codchiModules
    , ...
    }: nixpkgs.lib.nixosSystem {
      inherit system;
      modules =
        map
          # TODO consider adding specialArgs of other codchiModules for example as global.NAME
          ({ module, specialArgs ? { } }: overrideModuleArgs specialArgs module)
          ([{ module = ../modules; specialArgs.inputs.nixpkgs = nixpkgs; }]
          ++ codchiModules)
        ++
        nixpkgs.lib.optional (driver != null) { codchi.internal.${driver}.enable = true; }
      ;
    };

}