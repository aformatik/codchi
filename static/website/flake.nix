{
  inputs = {
    super.url = "path:../..";
    nixpkgs.follows = "super/nixpkgs";
  };

  outputs = { self, nixpkgs, ... }:
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

      data = (pkgs.formats.json {}).generate "data.json" (import ./index.html.nix);
    in
    {

      packages.x86_64-linux = rec {
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

