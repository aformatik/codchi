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
, libayatana-appindicator

, llvmPackages
, cargo-xwin
, wine64

, installShellFiles
, pandoc
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
        pandoc
      ];

      preConfigure = passthru.setupXWin "$(mktemp -d)";

      installPhase = ''
        mkdir -p $out/bin
        cp target/${CARGO_BUILD_TARGET}/*/*.exe $out/bin
      '';

      CODCHI_WSL_VERSION_MIN = "2.0.14";
      CODCHI_WSL_VERSION_MAX = "2.3.24";

    };
    linux = rec {
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
        nix-git
        makeWrapper
        pandoc
        installShellFiles
      ];
      buildInputs = [
        gtk3
        libayatana-appindicator.out
      ];

      outputs = [ "out" "docs" ];

      postInstall = ''
        installManPage ./target/codchi/man/*
        installShellCompletion --cmd codchi \
          --bash ./target/codchi/completions/codchi.bash \
          --fish ./target/codchi/completions/codchi.fish \
          --zsh  ./target/codchi/completions/_codchi

        mkdir -p $docs
        cp -r ./target/codchi/md $docs/usage
      '';

      postFixup = ''
        patchelf "$out/bin/codchi" \
          --add-rpath ${lib.makeLibraryPath buildInputs}
        wrapProgram "$out/bin/codchi" \
          --set CODCHI_LXD_CONTAINER_STORE $CODCHI_LXD_CONTAINER_STORE \
          --set CODCHI_LXD_CONTAINER_MACHINE $CODCHI_LXD_CONTAINER_MACHINE
      '';
    };
  }.${platform};


in
native.passthru.rustPlatform.buildRustPackage (lib.recursiveUpdate
{
  pname = "codchi";
  inherit (Cargo.package) version;

  src = lib.sourceByRegex ./. [
    "^src.*$"
    "^assets.*$"
    "^Cargo\..*"
    "^\.msvc_manifest.json$"
    "^build.*$"
  ];
  cargoLock.lockFile = ./Cargo.lock;
  cargoLock.outputHashes = {
    # "tray-icon-0.16.0" = "sha256-LxkEP31myIiWh6FDOzr9rZ8KAWISbja0jmEx0E2lM44=";
    # "clap-4.5.19" = "sha256-YRuZlp7jk05QLI551shgcVftcqKytTkxHlKbVejT1eE=";
  };

  passthru = { inherit nix-git; };
}
  native)
