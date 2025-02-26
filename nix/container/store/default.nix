{ inputs, pkgs, lib, config, consts, ... }:
let inherit (lib) mkOption mkEnableOption types mkIf;
  cfg = config.store;
in
{
  imports = [
    ./lxd.nix
    ./wsl.nix
  ];

  options.store = {
    enable = mkEnableOption "store container";
    init =
      let
        mkInitStage = description: mkOption {
          type = types.lines;
          description = ''
            ${description}

            Use `lib.mkBefore` / `lib.mkAfter` to inject scripts before / after.
          '';
          default = "";
        };
      in
      {
        filesystem = mkInitStage "Create directories and mount host directories.";
        ssl = mkInitStage "Setup SSL certs.";
        runtime = mkInitStage "Install / update stores' runtime dependencies via nix.";
        files = mkInitStage "Create / update static files.";
        services = mkInitStage ''
          Start service in the background. This must not terminate before the
          last service (nix-daemon).
        '';
      };
  };

  config = mkIf cfg.enable (lib.mkMerge [
    {

      name = "store";

      runtimePackages = with pkgs; [
        coreutils
        git
        openssh
      ];

      build.shellInit = /* bash */ ''
        set -euo pipefail

        export NIX_VERBOSITY="--log-format internal-json"
        if [ -n "''${CODCHI_DEBUG:-}" ]; then
          set -x
          export NIX_VERBOSITY="$NIX_VERBOSITY -v --print-build-logs"
        fi

        # Use config.system.binPackages and PATH from parent
        export PATH="/bin:${consts.store.PROFILE_STORE}/bin:/root/.nix-profile/bin:$PATH"

        # Ensure a consistent umask.
        umask 022

        # Make nixs' https work
        export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

        # prevent build locks
        export NIX_REMOTE="daemon"
      '';


      files = {
        # user & groups required for minimal linux + `nix daemon`
        "/etc/group" = ./etc/group;
        "/etc/passwd" = ./etc/passwd;
        # required for dns / other information lookup systems (mainly glibc)
        "/etc/nsswitch.conf" = ./etc/nsswitch.conf;
        # nix settings
        "/etc/nix/nix.conf" = ./etc/nix/nix.conf;
        # force the nix registry to use the nixpkgs version from this repo
        "/etc/nix/registry.json" = pkgs.writeText "registry.json" (builtins.toJSON {
          version = 2;
          flakes = [{
            exact = true;
            from = { type = "indirect"; id = "nixpkgs"; };
            to = { type = "path"; path = inputs.nixpkgs.outPath; }
              // lib.filterAttrs
              (n: _: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
              inputs.nixpkgs;
          }];
        });
        # nix runs as root and needs to access user repositories
        "/root/.gitconfig" = pkgs.writeText ".gitconfig" (lib.generators.toINI { } {
          safe.directory = "*";
        });
        "/sbin/init" = pkgs.writeShellScriptStatic "init" (with cfg.init; lib.concatLines [
          /* bash */
          ''
            ${config.build.shellInit}

            logE() {
              echo "$@" >&2
            }

          ''
          filesystem
          ssl
          runtime
          files
          services
        ]);
      };

      binPackages = with pkgs.pkgsStatic; [
        busybox
        bashInteractive
        inputs.nix.packages.${pkgs.system}.nix-everything-static

        pkgs.codchi-utils # ndd

        (pkgs.writeShellScriptBinStatic "run" /* bash */ ''
          ${config.build.shellInit}

          exec "$@"
        '')

        (pkgs.writeShellScriptBinStatic "runin" /* bash */ ''
          ${config.build.shellInit}

          source <(cat -)
        '')
      ];
    }

    {

      store.init.filesystem = lib.concatLines (
        map
          (dir: "[ -d /${dir} ] || mkdir /${dir}")
          [ "dev" "nix" "proc" "sys" "tmp" "var" ]
      );

    }

    {
      # add official ca certificates to enable https
      files."/etc/ssl/certs/nix.crt" = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

      store.init.ssl = /* bash */ ''
        if [ -f "${consts.store.DIR_DATA}/certs/system.crt" ]; then
          cp -f "${consts.store.DIR_DATA}/certs/system.crt" /etc/ssl/certs/ca-certificates.crt
        else
          ln -fs /etc/ssl/certs/nix.crt /etc/ssl/certs/ca-certificates.crt
        fi
      '';
    }
    {

      store.init.runtime = /* bash */ ''
        # use nix daemon to prevent locks
        unset NIX_REMOTE
        # nix daemon &
        # NIX_DAEMON_PID=$!
        # export NIX_REMOTE="daemon"

        if [ ! -f "${consts.store.DIR_CONFIG_STORE}/flake.nix" ]; then
          logE "Stores' flake.nix missing!"
          exit 1
        fi

        if ! command -v git &> /dev/null; then
          mkdir -p "${consts.store.DIR_CONFIG_STORE}"
          # git is needed for first `nix profile install` from flake
          nix $NIX_VERBOSITY profile install nixpkgs#git
        fi
        if [ ! -d "${consts.store.DIR_CONFIG_STORE}/.git" ]; then
          logE "Initializing store..."
          ( cd "${consts.store.DIR_CONFIG_STORE}"
            git init -q
            git add flake.*
          )
        fi
        if [ -n "$(git -C "${consts.store.DIR_CONFIG_STORE}" diff)" ]; then
          logE "Checking for updates..."
          ( cd "${consts.store.DIR_CONFIG_STORE}"
            nix $NIX_VERBOSITY flake update
            git add flake.*
          )
        fi

        if ! nix $NIX_VERBOSITY profile list --profile "${consts.store.PROFILE_STORE}" | grep "${consts.store.DIR_CONFIG_STORE}"; then
          logE "Installing store..."
          mkdir -p "${consts.store.DIR_CONFIG_STORE}"
          ndd $NIX_VERBOSITY profile install --profile "${consts.store.PROFILE_STORE}" "${consts.store.DIR_CONFIG_STORE}"

          # remove impure git from default profile
          nix $NIX_VERBOSITY profile remove '.*'
          nix $NIX_VERBOSITY profile wipe-history
        else
          logE "Updating store..."
          nix flake update $NIX_VERBOSITY "${consts.store.DIR_CONFIG_STORE}"
          ndd $NIX_VERBOSITY profile upgrade --profile "${consts.store.PROFILE_STORE}" '.*'
        fi

        # kill $NIX_DAEMON_PID
        # unset NIX_REMOTE
      '';
    }

    (
      let program = config.build.tarball.passthru.createFiles;
      in {
        runtimePackages = [ program ];
        store.init.files = /* bash */ ''
          cd /
          ${program.meta.mainProgram}
        '';
      }
    )

    {
      store.init.services = lib.mkAfter /* bash */ ''
        nix daemon
      '';
    }
  ]);

}
