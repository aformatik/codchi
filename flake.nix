{
  description = "Development Environment as Code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

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
          name = "codchi";
          root = ./cli;
          overrides = _: super: {
            text-builder-linear = super.text-builder-linear.override { text = super.text_2_0_2; };
            strong-path = pkgs.haskell.lib.compose.doJailbreak super.strong-path;
          };
        };
        controller-rootfs = pkgs.callPackage ./nix/controller { };

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

          cabal-fmt
          fourmolu

          pkgs.zlib
        ];
      };

      passthru.${system} = { inherit pkgs; };

    };
}
