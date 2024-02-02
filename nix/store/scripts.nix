{ pkgs, lib, config, consts, ... }:
let

  inherit (lib) mkOption types;
  cfg = config.system;

in
{

  options.system = {
    extraActivation = mkOption {
      type = types.lines;
      description = lib.mdDoc ''
        Shell scripts which are executed every time a process is launched in
        store. Therefore they must run very quick (at least most of the time). 

        Only `config.system.binPackages` (busybox, nix without git or ssh, and
        bash) are available here.

        Use `lib.mkBefore` / `lib.mkAfter` to influence script order.
      '';
      default = "";
    };
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

  config.system = lib.mkMerge [

    {
      build.shellInit = /* bash */ ''
        set -euo pipefail

        # Use config.system.binPackages and PATH from parent
        export PATH="/bin:${consts.PROFILE_STORE}/bin:/root/.nix-profile/bin:$PATH"

        # Ensure a consistent umask.
        umask 0022

        # Make nixs' http work
        export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

        # extra activation scripts
        ${cfg.extraActivation}
      '';

      binPackages = [
        (pkgs.writeShellScriptBinStatic "run" /* bash */ ''
          ${config.system.build.shellInit}

          exec "$@"
        '')

        (pkgs.writeShellScriptBinStatic "runin" /* bash */ ''
          ${config.system.build.shellInit}

          source <(cat -)
        '')
      ];

      files."/sbin/init" = pkgs.writeShellScriptStatic "init" (with cfg.init; lib.concatLines [
        /* bash */
        ''
          ${config.system.build.shellInit}

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
    }

    {

      init.filesystem = lib.concatLines (
        map
          (dir: "[ -d /${dir} ] || mkdir /${dir}")
          [ "dev" "nix" "proc" "sys" "tmp" "var" ]
      );

    }

    {
      # add official ca certificates to enable https
      files."/etc/ssl/certs/nix.crt" = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

      # TODO append custom certs to main bundle
      init.ssl = /* bash */ ''
        if [ -f "${consts.DIR_DATA}/certs/system.crt" ]; then
          cp -f "${consts.DIR_DATA}/certs/system.crt" /etc/ssl/certs/ca-certificates.crt
        else
          ln -fs /etc/ssl/certs/nix.crt /etc/ssl/certs/ca-certificates.crt
        fi
      '';
    }
    {

      init.runtime = /* bash */ ''
        if [ ! -f "${consts.DIR_CONFIG_STORE}/flake.nix" ]; then
          logE "Stores' flake.nix missing!"
          exit 1
        fi
        _GIT_IMPURE=
        if ! command -v git &> /dev/null; then
          # git is needed for first `nix profile install` from flake
          _GIT_IMPURE=1
          mkdir -p "${consts.DIR_CONFIG_STORE}"
          nix profile install nixpkgs#git
        fi
        if [ ! -d "${consts.DIR_CONFIG_STORE}/.git" ]; then
          logE "Initializing store..."
          ( cd "${consts.DIR_CONFIG_STORE}"
            git init -q
            git add flake.*
          )
        fi
        if [ -n "$(git -C "${consts.DIR_CONFIG_STORE}" diff)" ]; then
          logE "Checking for updates..."
          ( cd "${consts.DIR_CONFIG_STORE}"
            nix flake update
            git add flake.*
          )
        fi
        if [ -n "$_GIT_IMPURE" ]; then
          logE "Installing store..."
          mkdir -p "${consts.DIR_CONFIG_STORE}"
          nix profile install --profile "${consts.PROFILE_STORE}" "${consts.DIR_CONFIG_STORE}"

          # remove impure git from default profile
          nix profile remove '.*'
          nix profile wipe-history
        else
          logE "Updating store..."
          nix profile upgrade --profile "${consts.PROFILE_STORE}" '.*'
        fi
      '';
    }

    (
      let program = config.system.build.tarball.passthru.createFiles;
      in {
        runtimePackages = [ program ];
        init.files = /* bash */ ''
          ${program.meta.mainProgram}
        '';
      }
    )

    {
      init.services = lib.mkAfter /* bash */ ''
        nix daemon
      '';

    }
  ];

}
