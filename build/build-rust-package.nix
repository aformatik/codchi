{ self
, runCommand
, runCommandLocal
, runtimeShell
, writeShellScript
, writeShellScriptBin
, fetchFromGitHub

, lib

, store-podman-image
, targetPlatform # one of ["linux" "windows"]

, makeRustPlatform
, rust-bin

, makeWrapper
, pkg-config
, gtk3
, libayatana-appindicator
, libxkbcommon
, libGL
, libGLU

, llvmPackages
, cargo-xwin
, wine64

, installShellFiles
, pandoc
, ...
}:

let
  rustConfig = {
    extensions = [ "rust-src" "rust-analyzer" ];
    targets = [ "x86_64-unknown-linux-gnu" "x86_64-pc-windows-msvc" ];
  };
  # rustOrig = rust-bin.stable.latest.default.override rustConfig;
  rustOrig = rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override rustConfig);
  rustPlatformOrig = makeRustPlatform { cargo = rustOrig; rustc = rustOrig; };
  nix-git = writeShellScriptBin "nix-git-commit" ''
    echo ${self.rev or self.dirtyRev or "dirty"}
  '';

  platforms = {
    windows =
      let
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
      in
      rec {
        passthru = {
          inherit xwin nix-git;
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
          // { inherit (rustOrig) meta targetPlatforms badTargetPlatforms; };
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
          # Unwrapped clang: cc-rs invokes `clang-cl` for the MSVC target, and the
          # nix cc-wrapper (multi-target-unaware) breaks the `/imsvc` SDK includes.
          llvmPackages.clang-unwrapped
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
        CODCHI_WSL_VERSION_MAX = "2.4.11";

      };
    linux = rec {
      CARGO_BUILD_TARGET = "x86_64-unknown-linux-gnu";

      CODCHI_PODMAN_STORE_IMAGE = store-podman-image;

      passthru = {
        inherit nix-git;
        rust = rustOrig;
        rustPlatform = rustPlatformOrig;
      };
      nativeBuildInputs = [
        nix-git
        makeWrapper
        # GUI (codchi-gui) and the man-page/completion/usage-doc generation
        # return in later phases; restore alongside them:
        # pkg-config
        # pandoc
        # installShellFiles
      ];
      # GUI/tray link deps — restored with codchi-gui in Phase 15:
      # buildInputs = [
      #   gtk3
      #   libayatana-appindicator.out
      #   libxkbcommon.out
      #   libGL.out
      #   libGLU.out
      # ];

      # outputs = [ "out" "docs" ];

      # The v1 CLI does not yet generate man pages, completions or usage docs,
      # so there is nothing to install here. Restore with the doc-generation
      # step in a later phase:
      # postInstall = ''
      #   installManPage ./target/codchi/man/*
      #   installShellCompletion --cmd codchi \
      #     --bash ./target/codchi/completions/codchi.bash \
      #     --fish ./target/codchi/completions/codchi.fish \
      #     --zsh  ./target/codchi/completions/_codchi
      #
      #   mkdir -p $docs
      #   cp -r ./target/codchi/md $docs/usage
      # '';

      # The server now owns the Podman store, so it is the binary that needs the
      # store-image path. The GUI rpath patch returns with codchi-gui (Phase 15).
      postFixup = ''
        wrapProgram "$out/bin/codchi-server" \
          --set CODCHI_PODMAN_STORE_IMAGE $CODCHI_PODMAN_STORE_IMAGE
      '';
    };
  };
  target = platforms.${targetPlatform};
in
lib.makeOverridable (args: target.passthru.rustPlatform.buildRustPackage (lib.recursiveUpdate args target))
