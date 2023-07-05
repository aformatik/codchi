{
  description = "Development Environment as Code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

    docker-nixpkgs = { url = "github:nix-community/docker-nixpkgs"; flake = false; };
    # nixos-wsl.url = "path:/home/afo/docs/contrib/NixOS-WSL";
    nixos-wsl.url = "github:aformatik/NixOS-WSL";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ ];
        config.allowBroken = true;
      };

      inherit (nixpkgs.lib) pipe mapAttrs nixosSystem concatStrings;
      inherit (builtins) readDir;

      examples = pipe ./examples [
        readDir
        (mapAttrs (path: _: "${./examples}/${path}"))
      ];
      exampleModules = mapAttrs (_: path: import "${path}/configuration.nix") examples;
      exampleTemplates = mapAttrs
        (name: path: {
          inherit path;
          description = "NixOS module for ${name}";
        })
        examples;
      exampleSystems = mapAttrs
        (_: module: nixosSystem {
          inherit system;
          modules = [ self.nixosModules.driver-wsl module { codchi.instance.name = "example"; } ];
        })
        exampleModules;
    in
    rec {

      inherit pkgs;

      nixosModules =
        let
          codchi = import ./nix/modules;
          wsl = import ./nix/wsl { inherit inputs; };
        in
        exampleModules // {
          driver-wsl = { imports = [ codchi wsl ]; };
        };

      nixosConfigurations = exampleSystems;
      templates = exampleTemplates;

      packages.${system} = rec {
        default = pkgs.haskellPackages.developPackage {
          name = "CHANGEME";
          root = ./cli;
          cabal2nixOptions = "--benchmark";
        };
        controller-rootfs = pkgs.callPackage ./nix/controller { };

        # wsld = pkgs.callPackage ./nix/wsl/wsld.nix { };

        populate-cache =
          let
            buildInputs = map (drv: "${drv}") [
              nixosConfigurations.base.config.system.build.toplevel
              controller-rootfs.passthru.createContents
              controller-rootfs
            ];
          in
          pkgs.runCommandLocal "populate-cache" { } ''
            echo ${concatStrings buildInputs} > $out
          '';
      };

      devShells.${system}.default = pkgs.mkShell {
        inputsFrom = [ packages.${system}.default.env ];
        packages = with pkgs.haskellPackages; [
          cabal-install

          haskell-language-server
          ghcid
          # (with pkgs.haskell.lib.compose; pipe cabal-plan [
          #   (appendConfigureFlags [ "-f license-report" "-f exe" ])
            # doJailbreak
          # ])

          cabal-fmt
          # fourmolu
          stylish-haskell
          pkgs.zlib
          pkgs.helix
        ];
      };

      passthru.${system} = { inherit pkgs; };

    };
}
