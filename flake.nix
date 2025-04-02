{
  description = "CODe maCHInes - Declarative and Reprodicible Development Environements as Code";

  nixConfig = {
    extra-substituters = "https://codchi.cachix.org";
    extra-trusted-public-keys = "codchi.cachix.org-1:dVwdzogJgZO2x8kPKW02HNt2dpd/P/z46pY465MkokY=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      flake = false; # prevent fetching transitive inputs TODO
    };
    nix.url = "github:NixOS/nix/2.26.2";
    # nixvim = {
    #   url = "github:nix-community/nixvim";
    # inputs.nixpkgs.follows = "nixpkgs";
    # };

  };

  outputs = inputs@{ self, nixpkgs, rust-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (import rust-overlay)
          (self: _: {
            buildRustCodchi = targetPlatform: self.callPackage ./build/build-rust-package.nix {
              inherit targetPlatform;
              inherit (inputs) self;
            };
            codchi = self.callPackage ./crates { targetPlatform = "linux"; };
            codchi-windows = self.callPackage ./crates { targetPlatform = "windows"; };
            codchi-utils = self.callPackage ./crates/utils { };

            mkContainer = type: driver: (import ./nix/container
              {
                inherit inputs;
                inherit (nixpkgs) lib;
                pkgs = self;
              }
              {
                config.${type} = {
                  enable = true;
                  driver.${driver}.enable = true;
                };
              }
            );
            store-lxd = self.mkContainer "store" "lxd";
            store-lxd-tarball = self.store-lxd.config.build.tarball;
            store-wsl = self.mkContainer "store" "wsl";
            store-wsl-tarball = self.store-wsl.config.build.tarball;

            machine-lxd = self.mkContainer "machine" "lxd";
            machine-lxd-tarball = self.machine-lxd.config.build.tarball;
            machine-wsl = self.mkContainer "machine" "wsl";
            machine-wsl-tarball = self.machine-wsl.config.build.tarball;


            # nixvim = { inherit (inputs.nixvim.legacyPackages.${system}) makeNixvim; };
          })
          #(inputs.nixvim.overlays.default)
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

          nixosModules.default = import ./nix/nixos;
          nixosModules.codchi = {
            imports = [ ./configuration.nix ];
            environment.systemPackages = self.devShells.${system}.default.nativeBuildInputs;
          };

          packages.${system} = {
            inherit (pkgs) store-lxd store-wsl machine-lxd machine-wsl codchi-utils;
            default = pkgs.codchi;
            windows = pkgs.codchi-windows;
            inherit (pkgs.pkgsStatic) busybox;
            # editor = pkgs.nixvim.makeNixvim (import ./editor.nix);
          };

          devShells.${system} = {
            default = pkgs.callPackage ./crates/shell.nix { targetPlatform = "linux"; };
            windows = pkgs.callPackage ./crates/shell.nix { targetPlatform = "windows"; codchi = pkgs.codchi-windows; };
          };

          checks.${system}.populate-cache =
            let
              container = base: [
                base.config.build.tarball.passthru.createFiles
                base.config.build.runtime
              ];
              buildInputs = [
                # self.nixosConfigurations.lxd-base.config.system.build.toplevel
                # self.nixosConfigurations.wsl-base.config.system.build.toplevel

                self.packages.${system}.default
                self.packages.${system}.windows
              ]
              ++ container self.packages.${system}.store-lxd
              ++ container self.packages.${system}.store-wsl
              ++ container self.packages.${system}.machine-lxd
              ++ container self.packages.${system}.machine-wsl
              ;
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
                  # specialArgs.inputs = inputs;
                  modules = [ module ];
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
