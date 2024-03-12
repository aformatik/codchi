{ inputs, pkgs, lib, config, consts, ... }:
let
  inherit (lib) mkOption mkEnableOption types mkIf;
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

        if [ -n "''${CODCHI_DEBUG:-}" ]; then
          set -x
        fi

        # Use config.system.binPackages and PATH from parent
        export PATH="/bin:${consts.store.PROFILE_STORE}/bin:/root/.nix-profile/bin:$PATH"

        # Ensure a consistent umask.
        umask 0022

        # Make nixs' https work
        export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
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
        nix

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

      # TODO append custom certs to main bundle
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
        if [ ! -f "${consts.store.DIR_CONFIG_STORE}/flake.nix" ]; then
          logE "Stores' flake.nix missing!"
          exit 1
        fi
        _GIT_IMPURE=
        if ! command -v git &> /dev/null; then
          # git is needed for first `nix profile install` from flake
          _GIT_IMPURE=1
          mkdir -p "${consts.store.DIR_CONFIG_STORE}"
          nix profile install nixpkgs#git
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
            nix flake update
            git add flake.*
          )
        fi
        if [ -n "$_GIT_IMPURE" ]; then
          logE "Installing store..."
          mkdir -p "${consts.store.DIR_CONFIG_STORE}"
          nix profile install --profile "${consts.store.PROFILE_STORE}" "${consts.store.DIR_CONFIG_STORE}"

          # remove impure git from default profile
          nix profile remove '.*'
          nix profile wipe-history
        else
          logE "Updating store..."
          nix profile upgrade --profile "${consts.store.PROFILE_STORE}" '.*'
        fi
      '';
    }

    (
      let program = config.build.tarball.passthru.createFiles;
      in {
        runtimePackages = [ program ];
        store.init.files = /* bash */ ''
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
