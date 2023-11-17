{
  description = "CODe maCHInes - Declarative and Reprodicible Development Environements as Code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      drivers = [ "wsl" "lxd" ];

      inherit (nixpkgs.lib) foldl' recursiveUpdate;
      mergeAttrList = foldl' recursiveUpdate { };

      cli = isProd:
        let
          inherit (pkgs.haskell.lib.compose) markUnbroken addBuildTool;
        in
        pkgs.haskellPackages.developPackage {
          name = "codchi";
          root = ./cli;
          overrides = _self: super: {
            byline = markUnbroken super.byline;
          };
          modifier =
            if isProd
            then
              addBuildTool
                (pkgs.writeShellScriptBin "git" ''
                  echo "${self.rev or (builtins.throw "Can't build codchi: Git tree is dirty")}"
                '')
            else x: x;
          withHoogle = true;
        };
    in
    mergeAttrList
      [
        {
          nixosModules.default = import ./modules;

          packages.${system} = { default = cli true; }
            // pkgs.callPackages ./controller { inherit nixpkgs; };

          devShells.${system}.default = pkgs.mkShell {
            inputsFrom = [ (cli false).env ];
            packages = with pkgs.haskellPackages; [
              cabal-install

              haskell-language-server
              # haskell-debug-adapter
              fast-tags
              ghcid
              # ghci-dap
              # hoogle

              cabal-fmt
              fourmolu

              pkgs.zlib
            ];
            LD_LIBRARY_PATH = "$LD_LIBRARY_PATH:${pkgs.zlib}/lib";
          };

          checks.${system}.populate-cache =
            let
              buildInputs = [
                self.nixosConfigurations.lxd-base.config.system.build.toplevel
                self.nixosConfigurations.wsl-base.config.system.build.toplevel

                self.packages.${system}.wsl-ctrl-rootfs.passthru.createContents
                self.packages.${system}.wsl-ctrl-rootfs
                self.packages.${system}.lxd-ctrl-rootfs.passthru.createContents
                self.packages.${system}.lxd-ctrl-rootfs

                self.packages.${system}.default
              ];
            in
            pkgs.runCommandLocal "populate-cache" { } ''
              echo ${toString buildInputs} > $out
            '';

        }
        (
          let

            inherit (nixpkgs.lib) flip mapAttrs mapAttrs' nixosSystem nameValuePair;
            inherit (builtins) readDir;

            examples = flip mapAttrs (readDir ./examples) (path: _: "${./examples}/${path}");
            exampleModules = flip mapAttrs examples (_: path: import "${path}/configuration.nix");
            exampleTemplates = flip mapAttrs examples
              (name: path: {
                inherit path;
                description = "NixOS module for ${name}";
              });
            mkExampleSystems = driver:
              flip mapAttrs exampleModules
                (_: module: nixosSystem {
                  inherit system;
                  specialArgs.inputs = inputs;
                  modules = [
                    module
                    { codchi.internal = { name = "example"; ${driver}.enable = true; }; }
                    self.nixosModules.default
                  ];
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
