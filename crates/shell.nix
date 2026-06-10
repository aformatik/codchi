{ mkShell
, writeShellScriptBin
, lib
, fetchurl
  # , buildFHSUserEnv
  # , system
  # , fetchFromGitHub

, codchi

, nil
, nixpkgs-fmt
, strace
, gdb
, gdbgui

, targetPlatform # one of ["linux" "windows"]
, jetbrains

, cargo-watch
, cargo-edit
  # , cargo-deps
  # , cargo-udeps
, cargo-bloat
, cargo-flamegraph
, cargo-autoinherit
, graphviz

, vscode-with-extensions
, vscodium
, vscode-extensions
, ...
}:
let

  platforms = {
    windows = {
      inherit (codchi)
        CARGO_TARGET_X86_64_PC_WINDOWS_MSVC_RUNNER
        CODCHI_WSL_VERSION_MIN
        CODCHI_WSL_VERSION_MAX
        ;
      shellHook = codchi.passthru.setupXWin "$(git rev-parse --show-toplevel)";
      packages = [
        (writeShellScriptBin "msvc-fetch-manifest" ''
          CACHE="$(mktemp -d)"
          ${codchi.passthru.xwin}/bin/xwin --accept-license --cache-dir "$CACHE" download
          cat "$CACHE"/dl/manifest*.json
        '')
      ];
    };
    linux = {
      # inherit (codchi) CODCHI_PODMAN_STORE_IMAGE;
      LD_LIBRARY_PATH = lib.makeLibraryPath codchi.buildInputs;
    };
  };
  target = platforms.${targetPlatform};


  rustPlatform = codchi.passthru.rust;

in
mkShell (lib.recursiveUpdate target {
  inputsFrom = [ codchi ];

  packages = (target.packages or [ ]) ++ [
    nil
    nixpkgs-fmt

    codchi.passthru.rust
    codchi.passthru.nix-git
    strace
    gdb
    gdbgui

    (jetbrains.rust-rover.overrideAttrs (_: rec {
      version = "2026.1.3";
      src = fetchurl {
        url = "https://download.jetbrains.com/rustrover/RustRover-${version}.tar.gz";
        hash = "sha256-0+v05zxvFqXV13c8oV9dTTwtO+shgywD75cwUiZAab0=";
      };
    }))

    cargo-bloat
    # cargo-deps
    cargo-watch
    cargo-edit
    cargo-flamegraph
    graphviz
    cargo-autoinherit
    # cargo-udeps

    # (buildFHSUserEnv {
    #   name = "zed";
    #   targetPkgs = _: [
    #     # import directly to prevent polluting flake inputs
    #     (import
    #       (fetchFromGitHub {
    #         owner = "nixos";
    #         repo = "nixpkgs";
    #         rev = "nixos-unstable";
    #         sha256 = "sha256-Z/ELQhrSd7bMzTO8r7NZgi9g5emh+aRKoCdaAv5fiO0=";
    #       })
    #       { inherit system; }).zed-editor
    #   ];
    #   runScript = "zed";
    # })
    #
    (vscode-with-extensions.override {
      vscode = vscodium;
      vscodeExtensions = with vscode-extensions; [
        rust-lang.rust-analyzer
        jnoortheen.nix-ide
        mkhl.direnv
        asvetliakov.vscode-neovim
      ];
    })

  ] ++ (codchi.nativeBuildInputs or [ ]);

  shellHook = ''
    # export CODCHI_CONFIG_DIR="$(git rev-parse --show-toplevel)/.codchi/config"
    # export CODCHI_DATA_DIR="$(git rev-parse --show-toplevel)/.codchi/data"
    # export CODCHI_RUNTIME_DIR="$(git rev-parse --show-toplevel)/.codchi/runtime"
    # export CODCHI_NIX_DIR="$(git rev-parse --show-toplevel)/.codchi/nix"

    mkdir -p ~/.rust-rover/toolchain

    # Only touch the symlinks when the toolchain actually changed: `ln -sfn`
    # unlinks + recreates them, which RustRover sees as a root change and
    # re-triggers its (race-prone) library re-scan on every shell entry.
    [ "$(readlink ~/.rust-rover/toolchain/lib)" = "${rustPlatform}/lib" ] || ln -sfn ${rustPlatform}/lib ~/.rust-rover/toolchain
    [ "$(readlink ~/.rust-rover/toolchain/bin)" = "${rustPlatform}/bin" ] || ln -sfn ${rustPlatform}/bin ~/.rust-rover/toolchain

    export RUST_SRC_PATH="$HOME/.rust-rover/toolchain/lib/rustlib/src/rust/library"
  '' + (target.shellHook or "");

  inherit (codchi) CARGO_BUILD_TARGET;

  CARGO_PROFILE_RELEASE_DEBUG = "true";

})
