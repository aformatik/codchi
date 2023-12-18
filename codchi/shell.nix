{ mkShell
, lib
, callPackage

, nixpkgs-fmt
, strace
, gdb
, gdbgui

, platform # "win" or "linux"
, jetbrains

, cargo-bloat
# , cargo-udeps

, ...
}:
let

  codchi = callPackage ./. { inherit platform; };

  native = {
    win = {
      inherit (codchi.passthru) CARGO_TARGET_X86_64_PC_WINDOWS_MSVC_RUNNER;
      shellHook = ''
        # export WINE_PREFIX="$(pwd)/.wine"
        export XWIN_ARCH="x86_64"
        export XWIN_CACHE_DIR="$(git rev-parse --show-toplevel)/.xwin"
        if [ ! -d $XWIN_CACHE_DIR ]; then 
          mkdir $XWIN_CACHE_DIR/xwin
          cp -r ${codchi.passthru.splatted} $XWIN_CACHE_DIR
          chmod -R +w $XWIN_CACHE_DIR
        fi
      '';
    };
    linux = {
      LD_LIBRARY_PATH = lib.makeLibraryPath codchi.passthru.buildInputs;
    };
  }.${platform};

in
mkShell (lib.recursiveUpdate
{
  inputsFrom = [ codchi ];

  packages = [
    codchi.passthru.rust
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
    # cargo-udeps
  ] ++ (codchi.passthru.nativeBuildInputs or [ ]);

  inherit (codchi.passthru) CARGO_BUILD_TARGET;

}
  native)
