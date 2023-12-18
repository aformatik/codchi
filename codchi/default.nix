{ runCommand
, writeShellScript
, fetchFromGitHub
, lib

, platform

, makeRustPlatform
, rust-bin

, pkg-config
, gtk3
, xdotool
  # , libappindicator-gtk3
, libayatana-appindicator
, libxkbcommon
# , xorg
, vulkan-loader
, libGL
  # , webkitgtk_4_1
  # , libsoup
, libdbusmenu

, llvmPackages
, cargo-xwin
, wine64
, ...
}:
let
  Cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);

  rust = rust-bin.stable.latest.default.override {
    extensions = [
      "rust-src"
      "rust-analyzer"
    ];
    targets = [
      "x86_64-unknown-linux-gnu"
      "x86_64-pc-windows-msvc"
      "wasm32-wasi"
    ];
  };
  rustPlatform = makeRustPlatform {
    cargo = rust;
    rustc = rust;
  };


  native = {
    win =
      let
        xwin = rustPlatform.buildRustPackage rec {
          name = "xwin";
          src = fetchFromGitHub {
            owner = "Jake-Shadle";
            repo = "xwin";
            rev = "0.5.0";
            sha256 = "sha256-qHlh1PjEzm8nJW3IemikCaaxLtUCZRQccGQg/DgnJ4k=";
          };
          checkPhase = ":";
          cargoLock.lockFile = "${src}/Cargo.lock";
        };
        splatted = runCommand "splat"
          {
            nativeBuildInputs = [
              xwin
            ];
            outputHashMode = "recursive";
            outputHashAlgo = "sha256";
            outputHash = "sha256-R3a5kYii/0tuspES9bkYE4uphKqG+Tg5Qhjb77t/9Co=";
          }
          '' 
            mkdir -p $out/xwin
            xwin --accept-license splat --output $out/xwin --copy
            echo "x86_64" > $out/xwin/DONE
          '';
      in
      rec {
        inherit splatted;

        # targetCargo = "X86_64_PC_WINDOWS_MSVC";
        CARGO_BUILD_TARGET = "x86_64-pc-windows-msvc";
        CARGO_TARGET_X86_64_PC_WINDOWS_MSVC_RUNNER = writeShellScript "wine-wsl" ''
          if ! command -v wslpath &> /dev/null; then
            ${wine64}/bin/wine64 "$@"
          else
            "$@"
          fi
        '';

        nativeBuildInputs = [
          llvmPackages.llvm
          llvmPackages.bintools
          llvmPackages.clang
          llvmPackages.lld
          cargo-xwin
        ];

        buildPhase = ''
          
          runHook preBuild

          export XWIN_CACHE_DIR=$(mktemp -d) 
          export PATH=${rust}/bin:$PATH
          cp -r ${splatted}/xwin $XWIN_CACHE_DIR
          chmod -R +w $XWIN_CACHE_DIR

          if [ ! -z "''${buildAndTestSubdir-}" ]; then
              # ensure the output doesn't end up in the subdirectory
              export CARGO_TARGET_DIR="$(pwd)/target"

              pushd "''${buildAndTestSubdir}"
          fi

          if [ "''${cargoBuildType}" != "debug" ]; then
              cargoBuildProfileFlag="--profile ''${cargoBuildType}"
          fi

          if [ -n "''${cargoBuildNoDefaultFeatures-}" ]; then
              cargoBuildNoDefaultFeaturesFlag=--no-default-features
          fi

          if [ -n "''${cargoBuildFeatures-}" ]; then
              cargoBuildFeaturesFlag="--features=''${cargoBuildFeatures// /,}"
          fi

          cargo xwin build -j $NIX_BUILD_CORES \
            --xwin-cache-dir $XWIN_CACHE_DIR --xwin-arch x86_64 \
            --frozen \
            ''${cargoBuildProfileFlag} \
            ''${cargoBuildNoDefaultFeaturesFlag} \
            ''${cargoBuildFeaturesFlag} \
            ''${cargoBuildFlags}

            
          if [ ! -z "''${buildAndTestSubdir-}" ]; then
              popd
          fi

          runHook postBuild
        '';

        checkPhase = ''
          runHook preCheck

          export WINEPREFIX=$(mktemp -d)

          
          if [[ -n "''${buildAndTestSubdir-}" ]]; then
              pushd "''${buildAndTestSubdir}"
          fi

          if [ "''${cargoCheckType}" != "debug" ]; then
              cargoCheckProfileFlag="--profile ''${cargoCheckType}"
          fi

          if [ -n "''${cargoCheckNoDefaultFeatures-}" ]; then
              cargoCheckNoDefaultFeaturesFlag=--no-default-features
          fi

          if [ -n "''${cargoCheckFeatures-}" ]; then
              cargoCheckFeaturesFlag="--features=''${cargoCheckFeatures// /,}"
          fi

          if [ "''${cargoCheckType}" != "debug" ]; then
              cargoCheckProfileFlag="--profile ''${cargoCheckType}"
          fi

          argstr="''${cargoCheckProfileFlag} ''${cargoCheckNoDefaultFeaturesFlag} ''${cargoCheckFeaturesFlag}
            --frozen ''${cargoTestFlags}"

          cargo xwin test -j $NIX_BUILD_CORES \
            --xwin-cache-dir $XWIN_CACHE_DIR --xwin-arch x86_64 \
            ''${argstr} -- \
            ''${checkFlags} \
            ''${checkFlagsArray+"''${checkFlagsArray[@]}"}

          if [[ -n "''${buildAndTestSubdir-}" ]]; then
              popd
          fi

          runHook postCheck
        '';

        installPhase = ''
          mkdir -p $out/bin
          cp target/${CARGO_BUILD_TARGET}/*/*.exe $out/bin
        '';

      };
    linux = {
      # targetCargo = "X86_64-UNKNOWN-LINUX-GNU";
      CARGO_BUILD_TARGET = "x86_64-unknown-linux-gnu";
      nativeBuildInputs = [
        pkg-config
        # llvmPackages.llvm
        # llvmPackages.bintools
        # llvmPackages.clang
        # llvmPackages.lld
        # webkitgtk_4_1
        # libsoup
        gtk3
        gtk3.debug
      ];
      buildInputs = [
        gtk3
        gtk3.debug
        xdotool
        # libappindicator-gtk3
        libayatana-appindicator
        libxkbcommon
        # xorg.libX11
        vulkan-loader
        libGL
        libdbusmenu
        # llvmPackages.clang
        # libsoup
        # webkitgtk_4_1
      ];
    };
  }.${platform};


in
rustPlatform.buildRustPackage (lib.recursiveUpdate
{
  pname = Cargo.package.name;
  inherit (Cargo.package) version;

  src = ./.;
  cargoLock.lockFile = ./Cargo.lock;
  cargoLock.outputHashes = {
    # "tray-icon-0.11.0" = "";
  };

  passthru = { inherit rust rustPlatform; } // native;

}
  native)
