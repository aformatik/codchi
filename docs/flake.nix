{
  inputs.super.url = "path:..";
  inputs.nixpkgs.follows = "super/nixpkgs";

  outputs = { nixpkgs, super, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      inherit (nixpkgs) lib;
    in
    {
      packages.${system}.default =
        let # from https://github.com/nix-community/nixvim/blob/main/docs/default.nix
          transformOptions =
            let
              codchiPath = toString ./..;
              gitHubDeclaration = user: repo: branch: subpath: {
                url = "https://github.com/${user}/${repo}/blob/${branch}/${subpath}";
                name = "<${repo}/${subpath}>";
              };
            in
            opt: opt // {
              declarations =
                map
                  (
                    decl:
                    if lib.hasPrefix codchiPath (toString decl)
                    then
                      gitHubDeclaration "aformatik" "codchi" "master"
                        (lib.removePrefix "/" (lib.removePrefix codchiPath (toString decl)))
                    else if decl == "lib/modules.nix"
                    then gitHubDeclaration "NixOS" "nixpkgs" "master" decl
                    else decl
                  )
                  opt.declarations;
            };
          optionsDoc =
            let
              config = import (pkgs.path + "/nixos/lib/eval-config.nix") {
                inherit system;
                modules = [
                  (import ../nix/nixos)
                  { config._module.check = false; } # Disable option checks because some are missing
                ];
                # Don't render NixOS options
                baseModules = [ ];
              };
            in
            pkgs.nixosOptionsDoc {
              options = removeAttrs config.options [ "_module" ];
              inherit transformOptions;
              markdownByDefault = true;
              warningsAreErrors = false;
            };
        in
        pkgs.buildNpmPackage {
          pname = "codchi-docs";
          version = "0.0.0";
          src = ./.;
          npmDepsHash = "sha256-fj1CeWOap8bKt+S5YrNbi8c04Pa14LUAtvRtn6RZjUg=";
          preBuild = ''
            cp -f ${optionsDoc.optionsCommonMark} ./src/docs/options.md

            # remove symlink (used during development)
            rm  ./src/docs/usage
            cp -r ${super.packages.${system}.default.docs}/usage/codchi ./src/docs/usage
          '';
          installPhase = ''
            mv doc_build $out
          '';
        };

      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          nodejs
          nodePackages.npm
          nodePackages.typescript-language-server
          tailwindcss-language-server
          biome
          microserver
        ];
        shellHook = ''
          export PATH="$PATH:$(pwd)/node_modules/.bin"
        '';
      };
    };
}
