{
  description = "CODe maCHInes - Declarative and Reprodicible Development Environements as Code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      flake = false; # prevent fetching transitive inputs TODO
    };
    nixvim = {
      url = "github:nix-community/nixvim";
      # inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs@{ self, nixpkgs, rust-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (import rust-overlay)
          (self: _: {
            codchi = self.callPackage ./codchi { inherit (inputs) self; platform = "linux"; };
            codchi-windows = self.callPackage ./codchi { inherit (inputs) self; platform = "win"; };

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


            nixvim = { inherit (inputs.nixvim.legacyPackages.${system}) makeNixvim; };
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
          nixosModules.codchi = { pkgs, ... }: {
            nixpkgs.config.allowUnfree = true;
            environment.systemPackages = [ pkgs.vscodium ];
            programs.neovim = {
              enable = true;
              package = pkgs.nixvim.makeNixvim (import ./editor.nix);
            };
            programs.direnv = {
              enable = true;
              nix-direnv.enable = true;
            };
          };

          packages.${system} = {
            inherit (pkgs) store-lxd store-wsl machine-lxd machine-wsl;
            default = pkgs.codchi;
            windows = pkgs.codchi-windows;
          };

          devShells.${system} = {
            default = pkgs.callPackage ./codchi/shell.nix { platform = "linux"; };
            windows = pkgs.callPackage ./codchi/shell.nix { platform = "win"; codchi = pkgs.codchi-windows; };
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

                # self.packages.${system}.wsl-ctrl-rootfs.passthru.createContents
                # self.packages.${system}.wsl-ctrl-rootfs
                # self.packages.${system}.lxd-ctrl-rootfs.passthru.createContents
                # self.packages.${system}.lxd-ctrl-rootfs

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
