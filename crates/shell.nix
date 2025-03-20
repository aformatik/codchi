{ mkShell
, writeShellScriptBin
, lib
, buildFHSUserEnv
, system
, fetchFromGitHub

, codchi

, nil
, nixpkgs-fmt
, strace
, gdb
, gdbgui

, platform # "win" or "linux"
  # , jetbrains

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

  native = {
    win = {
      inherit (codchi)
        CARGO_TARGET_X86_64_PC_WINDOWS_MSVC_RUNNER
        CODCHI_WSL_VERSION_MIN
        CODCHI_WSL_VERSION_MAX
        ;
      shellHook = codchi.passthru.setupXWin "$(git rev-parse --show-toplevel)";
    };
    linux = {
      inherit (codchi) CODCHI_LXD_CONTAINER_STORE CODCHI_LXD_CONTAINER_MACHINE;
      LD_LIBRARY_PATH = lib.makeLibraryPath codchi.buildInputs;
    };
  }.${platform};

in
mkShell (lib.recursiveUpdate
  native
{
  inputsFrom = [ codchi ];

  packages = [
    nil
    nixpkgs-fmt

    codchi.passthru.rust
    codchi.passthru.nix-git
    strace
    gdb
    gdbgui

    # (jetbrains.rust-rover.overrideAttrs (_: {
    #   src = fetchTarball {
    #     url = "https://download.jetbrains.com/rustrover/RustRover-233.11799.284.tar.gz";
    #     sha256 = "sha256:0nq62y0cqvhx8a81c7wc1zrm9bp00ljrh96qlsvmy0mwn3s278ym";
    #   };
    # }))

    cargo-bloat
    # cargo-deps
    cargo-watch
    cargo-edit
    cargo-flamegraph
    graphviz
    cargo-autoinherit
    # cargo-udeps

    (writeShellScriptBin "msvc-fetch-manifest" ''
      CACHE="$(mktemp -d)"
      ${codchi.passthru.xwin}/bin/xwin --accept-license --cache-dir "$CACHE" download
      cat "$CACHE"/dl/manifest*.json
    '')

    (buildFHSUserEnv {
      name = "zed";
      targetPkgs = _: [
        # import directly to prevent polluting flake inputs
        (import
          (fetchFromGitHub {
            owner = "nixos";
            repo = "nixpkgs";
            rev = "nixos-unstable";
            sha256 = "sha256-Z/ELQhrSd7bMzTO8r7NZgi9g5emh+aRKoCdaAv5fiO0=";
          })
          { inherit system; }).zed-editor
      ];
      runScript = "zed";
    })

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
  '' + (native.shellHook or "");

  inherit (codchi) CARGO_BUILD_TARGET;

  CARGO_PROFILE_RELEASE_DEBUG = "true";

})
