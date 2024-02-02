{ mkShell
, writeShellScriptBin
, lib

, codchi

, nixpkgs-fmt
, strace
, gdb
, gdbgui

, platform # "win" or "linux"
  # , jetbrains

, cargo-deps
  # , cargo-udeps
, cargo-bloat
, graphviz

, ...
}:
let

  native = {
    win = {
      inherit (codchi) CARGO_TARGET_X86_64_PC_WINDOWS_MSVC_RUNNER;
      shellHook = codchi.passthru.setupXWin "$(git rev-parse --show-toplevel)";
    };
    linux = {
      inherit (codchi) CODCHI_LXD_CTRL_ROOTFS;
      LD_LIBRARY_PATH = lib.makeLibraryPath codchi.buildInputs;
    };
  }.${platform};

in
mkShell (lib.recursiveUpdate
  native
{
  inputsFrom = [ codchi ];

  packages = [
    codchi.passthru.rust
    codchi.passthru.nix-git
    nixpkgs-fmt
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
    cargo-deps
    graphviz
    # cargo-udeps

    (writeShellScriptBin "msvc-fetch-manifest" ''
      CACHE="$(mktemp -d)"
      ${codchi.passthru.xwin}/bin/xwin --accept-license --cache-dir "$CACHE" download
      cat "$CACHE"/dl/manifest*.json
    '')
  ] ++ (codchi.nativeBuildInputs or [ ]);

  shellHook = ''
    export CODCHI_CONFIG_DIR="$(git rev-parse --show-toplevel)/.codchi/config"
    export CODCHI_DATA_DIR="$(git rev-parse --show-toplevel)/.codchi/data"
    export CODCHI_RUNTIME_DIR="$(git rev-parse --show-toplevel)/.codchi/runtime"
    export CODCHI_NIX_DIR="$(git rev-parse --show-toplevel)/.codchi/nix"
  '' + (native.shellHook or "");

  inherit (codchi) CARGO_BUILD_TARGET;

})
