{
  description = "CODe maCHInes - Declarative and Reprodicible Development Environements as Code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      flake = false; # prevent fetching transitive inputs TODO
    };
  };

  outputs = inputs@{ self, nixpkgs, rust-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (import rust-overlay)
          (self: _:
            let

              mkStore = driver: (import ./nix/store
                {
                  inherit inputs;
                  inherit (nixpkgs) lib;
                  pkgs = self;
                }
                {
                  config.driver.${driver}.enable = true;
                }
              );
            in
            {
              codchi = self.callPackage ./codchi { inherit (inputs) self; platform = "linux"; };
              codchi-windows = self.callPackage ./codchi { inherit (inputs) self; platform = "win"; };

              store-lxd = mkStore "lxd";
              store-lxd-tarball = self.store-lxd.config.system.build.tarball;
              store-wsl = mkStore "wsl";
              store-wsl-tarball = self.store-wsl.config.system.build.tarball;
            })
          (import ./nix/pkgs)
        ];
        config.allowUnfree = true;
      };
      drivers = [ "wsl" "lxd" ];

      inherit (nixpkgs.lib) foldl' recursiveUpdate;
      mergeAttrList = foldl' recursiveUpdate { };

      lib = import ./nix/lib.nix;

    in
    mergeAttrList
      [
        {
          inherit lib;

          nixosModules.default = import ./nix/modules;
          nixosModules.codchi = {
            nixpkgs.config.allowUnfree = true;
            environment.systemPackages = [ pkgs.vscodium ];
            programs.direnv = {
              enable = true;
              nix-direnv.enable = true;
            };
          };

          packages.${system} = {
            inherit (pkgs) store-lxd store-wsl;
            default = pkgs.codchi;
            windows = pkgs.codchi-windows;
          };

          devShells.${system} = {
            default = pkgs.callPackage ./codchi/shell.nix { platform = "linux"; };
            windows = pkgs.callPackage ./codchi/shell.nix { platform = "win"; codchi = pkgs.codchi-windows; };
          };

          checks.${system}.populate-cache =
            let
              buildInputs = [
                self.nixosConfigurations.lxd-base.config.system.build.toplevel
                self.nixosConfigurations.wsl-base.config.system.build.toplevel

                # self.packages.${system}.wsl-ctrl-rootfs.passthru.createContents
                # self.packages.${system}.wsl-ctrl-rootfs
                # self.packages.${system}.lxd-ctrl-rootfs.passthru.createContents
                # self.packages.${system}.lxd-ctrl-rootfs

                self.packages.${system}.default
                self.packages.${system}.windows
              ];
            in
            pkgs.runCommandLocal "populate-cache" { } ''
              echo ${toString buildInputs} > $out
            '';

        }
        (
          let

            inherit (nixpkgs.lib) flip mapAttrs mapAttrs' nameValuePair;
            inherit (builtins) readDir;

            examples = flip mapAttrs (readDir ./nix/examples) (path: _: "${./nix/examples}/${path}");
            exampleModules = flip mapAttrs examples (_: path: import "${path}/configuration.nix");
            exampleTemplates = flip mapAttrs examples
              (name: path: {
                inherit path;
                description = "NixOS module for ${name}";
              });
            mkExampleSystems = driver:
              flip mapAttrs exampleModules
                (_: module: lib.codeMachine {
                  inherit system driver nixpkgs;
                  specialArgs.inputs = inputs;
                  codchiModules = [{ inherit module; }];
                });
          in
          {
            templates = exampleTemplates;
            nixosModules = exampleModules;
            nixosConfigurations = mergeAttrList
              (flip map drivers
                (driver:
                  (mapAttrs'
                    (name: nameValuePair "${driver}-${name}"))
                    (mkExampleSystems driver)
                ));
          }
        )
      ];
}
