{
  inputs = {
    super.url = "path:../..";
    nixpkgs.follows = "super/nixpkgs";
  };

  outputs = { self, nixpkgs, super, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      inherit (nixpkgs.lib) makeSearchPath makeBinPath;

      tailwindDeps = with pkgs; [
        nodePackages."@tailwindcss/typography"
        nodePackages."@tailwindcss/forms"
        nodePackages."@tailwindcss/line-clamp"
        nodePackages."@tailwindcss/aspect-ratio"
      ];

      data = (pkgs.formats.json { }).generate "data.json" (import ./index.html.nix);

      eval = import (pkgs.path + "/nixos/lib/eval-config.nix") {
        inherit system;
        modules = [
          (import ../../modules)
        ];
        check = false;
        baseModules = [ ];
      };
      rewriteDeclaration = str:
        let
          matches = builtins.match "/nix/store/[0-9a-z]{32}-[-.+_0-9a-zA-Z]+/modules/(.*)$" str;
          rev = if builtins.hasAttr "rev" self then self.rev else "master";
        in
        if matches != null
        then {
          name = "<codchi/${builtins.head matches}>";
          url = "https://github.com/aformatik/codchi/blob/${rev}/modules/${builtins.head matches}";
        }
        else str;
      optionsDoc = pkgs.nixosOptionsDoc {
        inherit (eval) options;
        transformOptions = x: x // {
          declarations = map rewriteDeclaration x.declarations;
        };
        markdownByDefault = true;
      };
    in
    {

      packages.x86_64-linux = rec {
        inherit eval optionsDoc;
        docs = pkgs.runCommandLocal "docs"
          { buildInputs = [ pkgs.mdbook ]; }
          ''
            mkdir docs
            cat << EOF > book.toml
            [book]
            authors = ["codchi.dev"]
            language = "en"
            multilingual = false
            src = "docs"
            title = "Codchi Documentation"
            EOF
            cat << EOF > docs/SUMMARY.md
            # Codchi.dev Documentation
            - [Module Options](module-options.md)
            EOF
            ln -s ${optionsDoc.optionsCommonMark} "./docs/module-options.md"
            mdbook build
            mv book $out
          '';

        tera-cli = pkgs.rustPlatform.buildRustPackage rec {
          pname = "tera-cli";
          version = "2023-07-06-unstable";

          src = pkgs.fetchFromGitHub {
            owner = "chevdor";
            repo = pname;
            rev = "713333f";
            sha256 = "sha256-iWynJJygnxiUqWk6NDiaMsz2egbQTafttTePcTxHK4c=";
          };

          buildFeatures = [ "fluent" ];

          cargoSha256 = "sha256-MVKGE8SJPeWJywKrKNWpQxGga4hWmSrxDHlNGuShIkE=";
        };
        default = pkgs.runCommandLocal "tera-tailwind"
          { buildInputs = [ tera-cli pkgs.nodePackages.tailwindcss ]; }
          ''
            export NODE_PATH=${makeSearchPath "lib/node_modules" tailwindDeps}

            mkdir $out
            cd $out

            cp -a ${./public} public
            chmod +w public

            cp -a ${docs} docs

            tera --template ${./index.html} ${data} > index.html
            tailwindcss -i ${./static/tailwind.css} -c ${./tailwind.config.js} -o public/style.css -m
          '';

        serve = pkgs.writeShellScriptBin "tera-tailwind-serve" ''
          set -x
          export NODE_PATH=${makeSearchPath "lib/node_modules" tailwindDeps}
          export PATH="$PATH:${makeBinPath [ tera-cli pkgs.nodePackages.tailwindcss pkgs.nodePackages.live-server pkgs.entr ]}"

          mkdir dist || true
          cd dist
          rm -rf public
          cp -a ../public .

          live-server --no-browser &

          gen(){
            tera --template ../index.html ${data} > index.html
            tailwindcss -i ../static/tailwind.css -c ../tailwind.config.js -o public/style.css

          }

          while true; do
            gen
            find .. -name '*.html' -not -path "*/dist/*" | entr -pd -s 'kill $PPID'
          done
        '';

      };

      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          self.packages.${system}.tera-cli
          nodePackages.tailwindcss
          nodePackages."@tailwindcss/language-server"
          nodePackages.prettier
        ] ++ tailwindDeps;
      };
    };
}

