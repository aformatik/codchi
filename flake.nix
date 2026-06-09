{
  description = "CODe maCHInes - Declarative and Reprodicible Development Environements as Code";

  nixConfig = {
    extra-substituters = "https://codchi.cachix.org";
    extra-trusted-public-keys = "codchi.cachix.org-1:dVwdzogJgZO2x8kPKW02HNt2dpd/P/z46pY465MkokY=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      flake = false; # prevent fetching transitive inputs TODO
    };
    nix.url = "github:NixOS/nix/2.26.2";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # nixvim = {
    #   url = "github:nix-community/nixvim";
    # inputs.nixpkgs.follows = "nixpkgs";
    # };

  };

  outputs = inputs@{ self, nixpkgs, rust-overlay, treefmt-nix, ... }:
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
            store-podman = self.mkContainer "store" "podman";
            store-podman-image = self.store-podman.config.build.dockerImage;
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

      # Formatter: rustfmt (edition 2024 for standalone files / let-chains) + nix.
      # `nix fmt` runs it; `checks.formatting` enforces it in CI.
      treefmtEval = treefmt-nix.lib.evalModule pkgs {
        projectRootFile = "flake.nix";
        programs.rustfmt.enable = true;
        programs.rustfmt.edition = "2024";
        programs.nixpkgs-fmt.enable = true;
        # Phase 0 formats only the active surface: the v1 contract crate
        # (codchi-api), the root flake, and build/ helpers. The deactivated
        # product crates (reference-only) and the legacy nix tree are excluded
        # to avoid churn; re-include each as it is reactivated.
        settings.global.excludes = [
          "*.lock"
          "*.json"
          "*.md"
          "crates/target/**"
          "crates/codchi/**"
          "crates/codchiw/**"
          "crates/codchi-server/**"
          "crates/codchi-gui/**"
          "crates/shared/**"
          "crates/ipc/**"
          "crates/utils/**"
          "nix/**"
          "docs/**"
          "configuration.nix"
          "test.nix"
          "build/build-rust-package.nix"
          "build/flake.nix"
        ];
      };

      # Pinned nightly toolchain (matches the product toolchain via the locked
      # rust-overlay) used by the hermetic codchi-api check. `default` includes
      # clippy + rustfmt.
      ciRust = pkgs.rust-bin.selectLatestNightlyWith (toolchain:
        toolchain.default.override { extensions = [ "rust-src" ]; });
      ciRustPlatform = pkgs.makeRustPlatform { cargo = ciRust; rustc = ciRust; };

    in
    mergeAttrList
      [
        {
          inherit lib;

          formatter.${system} = treefmtEval.config.build.wrapper;

          nixosModules.default = import ./nix/nixos;
          nixosModules.codchi = {
            imports = [ ./configuration.nix ];
            environment.systemPackages = self.devShells.${system}.default.nativeBuildInputs;
          };

          packages.${system} = {
            inherit (pkgs) store-podman store-podman-image store-wsl machine-lxd machine-wsl codchi-utils;
            default = pkgs.codchi;
            windows = pkgs.codchi-windows;
            # oasdiff powers the OpenAPI breaking-change gate (not in nixpkgs).
            oasdiff = pkgs.callPackage ./build/oasdiff.nix { };
            inherit (pkgs.pkgsStatic) busybox;
            # editor = pkgs.nixvim.makeNixvim (import ./editor.nix);
            foo = pkgs.dockerTools.buildImage {
              name = "hello";
              tag = "latest";
              copyToRoot = pkgs.hello;
              config = { cmd = [ "/bin/hello" ]; };
            };
          };

          devShells.${system} = {
            default = pkgs.callPackage ./crates/shell.nix { targetPlatform = "linux"; };
            windows = pkgs.callPackage ./crates/shell.nix { targetPlatform = "windows"; codchi = pkgs.codchi-windows; };
          };

          checks.${system} = {
            # Phase 0 contract gate: lint, test, and verify the committed
            # OpenAPI snapshot for codchi-api, hermetically and without the GUI
            # dev shell.
            codchi-api = ciRustPlatform.buildRustPackage {
              pname = "codchi-api-checks";
              version = (nixpkgs.lib.importTOML ./crates/Cargo.toml).workspace.package.version;
              src = nixpkgs.lib.sourceByRegex ./crates [
                "^codchi-api.*$"
                "^Cargo\\.toml$"
                "^Cargo\\.lock$"
              ];
              cargoLock.lockFile = ./crates/Cargo.lock;
              nativeBuildInputs = [ ciRust ];
              buildPhase = ''
                runHook preBuild
                cargo clippy -p codchi-api --all-targets --offline -- -D warnings
                runHook postBuild
              '';
              checkPhase = ''
                runHook preCheck
                cargo test -p codchi-api --offline
                cargo run -q -p codchi-api --bin gen-openapi --offline > openapi.generated.json
                if ! diff -u codchi-api/openapi.json openapi.generated.json; then
                  echo "openapi.json drift: regenerate with 'cargo run -p codchi-api --bin gen-openapi > crates/codchi-api/openapi.json'" >&2
                  exit 1
                fi
                runHook postCheck
              '';
              installPhase = ''
                runHook preInstall
                mkdir -p $out
                cp codchi-api/openapi.json $out/openapi.json
                runHook postInstall
              '';
            };

            formatting = treefmtEval.config.build.check self;

            populate-cache =
              let
                # container = base: [
                #   base.config.build.tarball.passthru.createFiles
                #   base.config.build.runtime
                # ];
                buildInputs = [
                  # Phase 0: the product crates are excluded from the cargo
                  # workspace, so the rust product and its containers cannot
                  # build. Restore these as crates are reactivated (see
                  # v1/STATUS.md):
                  #   self.packages.${system}.default
                  #   self.packages.${system}.windows
                  #   ++ container self.packages.${system}.store-wsl
                  #   ++ container self.packages.${system}.machine-lxd
                  #   ++ container self.packages.${system}.machine-wsl
                  self.checks.${system}.codchi-api
                  self.checks.${system}.formatting
                  self.packages.${system}.oasdiff
                ];
              in
              pkgs.runCommandLocal "populate-cache" { } ''
                echo ${toString buildInputs} > $out
              '';
          };

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
