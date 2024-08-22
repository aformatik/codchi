{ self
, runCommand
, runCommandLocal
, runtimeShell
, writeShellScript
, writeShellScriptBin
, fetchFromGitHub

, lib

, store-lxd-tarball
, machine-lxd-tarball
, platform

, makeRustPlatform
, rust-bin

, makeWrapper
, pkg-config
, gtk3
  # , xdotool
  # , libappindicator-gtk3
, libayatana-appindicator
  # , libxkbcommon
  # , xorg
  # , vulkan-loader
  # , libGL
  # , webkitgtk_4_1
  # , libsoup
  # , libdbusmenu

, llvmPackages
, cargo-xwin
, wine64
, ...
}:
let
  Cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);

  rustConfig = {
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
  # rustOrig = rust-bin.stable.latest.default.override rustConfig;
  rustOrig = rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override rustConfig);
  rustPlatformOrig = makeRustPlatform { cargo = rustOrig; rustc = rustOrig; };
  xwin = rustPlatformOrig.buildRustPackage rec {
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
  nix-git = writeShellScriptBin "nix-git-commit" ''
    echo ${self.rev or self.dirtyRev or "dirty"}
  '';

  native = {
    win = rec {
      passthru = {
        inherit xwin;
        splatted = runCommand "splat"
          {
            nativeBuildInputs = [ passthru.xwin ];
            outputHashMode = "recursive";
            outputHashAlgo = "sha256";
            outputHash = "sha256-5ZZeEBuemx+lRmc9PczMfd13JwTvI6qMNvNmHdtK+1U=";
          }
          '' 
            mkdir -p $out/xwin
            xwin --accept-license --manifest ${./.msvc_manifest.json} splat --output $out/xwin --copy
          '';

        # wrap cargo by checking and injecting xwin where it is needed
        rust = (runCommandLocal "cargo-xwinize" { } ''
          cp -r ${rustOrig} $out
          chmod +w $out/bin
          mv $out/bin/cargo $out/bin/.cargo
          cat << EOF > $out/bin/cargo
          #!${runtimeShell}
          case "\$1" in
            build|check|clippy|run|rustc|test)     
              # replace linux target with msvc
              if [ -z "\$CARGO_ENCODED_RUSTFLAGS" ]; then
                args=()
                prev_was_target=
                for i in "\$@"; do
                  if [ -n "\$prev_was_target" ]; then
                    args+=("${CARGO_BUILD_TARGET}");
                    prev_was_target=
                    continue
                  fi
                  case "\$i" in
                    --target) 
                      prev_was_target=1 
                      ;;
                    --)
                      # duplicate '--' to satisfy xwin test
                      if [ "\$1" = "test" ]; then
                        args+=("\$i");
                      fi
                      ;;
                  esac
                  args+=("\$i");
                done
                exec -a "\$0" $out/bin/.cargo xwin "\''${args[@]}" 
              fi 
              ;;
          esac
          exec -a "\$0" $out/bin/.cargo "\$@"
          EOF
          chmod +x $out/bin/cargo
        '')
        //
        { inherit (rustOrig) meta; };
        rustPlatform = makeRustPlatform { cargo = passthru.rust; rustc = passthru.rust; };

        setupXWin = topDir: /* bash */ ''
          if [ ! -d "${topDir}" ]; then 
            mkdir -p "${topDir}"
          fi
          export WINEPREFIX="${topDir}/.wine"
          export XWIN_ARCH="x86_64"
          export XWIN_CACHE_DIR="${topDir}/.xwin"
          if [ ! -d $XWIN_CACHE_DIR ]; then 
            mkdir -p $XWIN_CACHE_DIR
            cp -r ${passthru.splatted}/xwin $XWIN_CACHE_DIR
            chmod -R +w $XWIN_CACHE_DIR
            echo "x86_64" > $XWIN_CACHE_DIR/xwin/DONE
          fi
        '';
      };

      auditable = false; # disable cargo auditable

      CARGO_BUILD_TARGET = "x86_64-pc-windows-msvc";
      CARGO_TARGET_X86_64_PC_WINDOWS_MSVC_RUNNER = writeShellScript "wine-wsl" ''
        if ! command -v /bin/wslpath &> /dev/null; then
          ${wine64}/bin/wine64 "$@"
        else
          "$@"
        fi
      '';

      # On Windows MSVC, statically link the C runtime so that the resulting EXE does
      # not depend on the vcruntime DLL.
      RUSTFLAGS = "-C target-feature=+crt-static";

      nativeBuildInputs = [
        llvmPackages.llvm
        llvmPackages.bintools
        llvmPackages.clang
        llvmPackages.lld
        cargo-xwin
        nix-git
      ];

      preConfigure = passthru.setupXWin "$(mktemp -d)";

      installPhase = ''
        mkdir -p $out/bin
        cp target/${CARGO_BUILD_TARGET}/*/*.exe $out/bin
      '';

      CODCHI_WSL_VERSION_MIN = "2.0.14";
      CODCHI_WSL_VERSION_MAX = "2.2.4";

    };
    linux = rec {
      # targetCargo = "X86_64-UNKNOWN-LINUX-GNU";
      CARGO_BUILD_TARGET = "x86_64-unknown-linux-gnu";

      CODCHI_LXD_CONTAINER_STORE = store-lxd-tarball;
      CODCHI_LXD_CONTAINER_MACHINE = machine-lxd-tarball;

      passthru = {
        inherit xwin;
        rust = rustOrig;
        rustPlatform = rustPlatformOrig;
      };
      nativeBuildInputs = [
        pkg-config
        # llvmPackages.llvm
        # llvmPackages.bintools
        # llvmPackages.clang
        # llvmPackages.lld
        # webkitgtk_4_1
        # libsoup
        # gtk3
        # gtk3.debug
        nix-git
        makeWrapper
      ];
      buildInputs = [
        gtk3
        # gtk3.debug
        # libappindicator-gtk3
        libayatana-appindicator.out
        # libxkbcommon
        # xorg.libX11
        # vulkan-loader
        # libGL
        # libdbusmenu
        # llvmPackages.clang
        # libsoup
        # webkitgtk_4_1
      ];

      postFixup = ''
        patchelf "$out/bin/codchi" \
          --add-rpath ${lib.makeLibraryPath buildInputs}
        wrapProgram "$out/bin/codchi" \
          --set CODCHI_LXD_CTRL_ROOTFS $CODCHI_LXD_CTRL_ROOTFS
      '';
    };
  }.${platform};


in
native.passthru.rustPlatform.buildRustPackage (lib.recursiveUpdate
{
  pname = "codchi";
  inherit (Cargo.package) version;

  # src = lib.cleanSource ./.;
  src = lib.sourceByRegex ./. [
    "^src.*$"
    "^assets.*$"
    "^build.rs$"
    "^Cargo\..*"
    "^\.msvc_manifest.json$"
  ];
  cargoLock.lockFile = ./Cargo.lock;
  cargoLock.outputHashes = {
    # "tarpc-0.34.0" = "";
     "tray-icon-0.16.0" = "sha256-1l2paLXRlFe/WGWvjz+Y42NHzSUiam3SMOSK5EPnII8=";
  };

  passthru = { inherit nix-git; };
}
  native)
