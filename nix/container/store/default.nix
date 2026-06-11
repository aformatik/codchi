{ inputs, pkgs, lib, config, consts, ... }:
let inherit (lib) mkOption mkEnableOption types mkIf;
  cfg = config.store;
in
{
  imports = [
    ./podman.nix
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

      # v1 (01-podman-store.md S3): the store ships static `nix` only as its
      # substantive runtime. git/openssh/coreutils are NOT baked — nix's built-in
      # fetchers (libcurl/libgit2/libssh2) cover flake inputs, and the rare
      # dirty-local-config case is served on demand via `nix shell` (S3a). With
      # nothing installed at runtime, `runtimePackages` is empty.
      runtimePackages = [ ];

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
        "/etc/protocols" = "${pkgs.iana-etc}/etc/protocols";
        "/etc/services" = "${pkgs.iana-etc}/etc/services";
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
            # to = { type = "path"; path = inputs.nixpkgs.outPath; }
            #   // lib.filterAttrs
            #   (n: _: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
            #   inputs.nixpkgs;
            to = {
              type = "github";
              owner = "NixOS";
              repo = "nixpkgs";
              inherit (inputs.nixpkgs) rev;
            };
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

        pkgs.codchi-container-utils # ndd

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
      # v1 (01-podman-store.md S1/S2): no runtime provisioning. The image is
      # fully self-contained — there is no host-written flake.nix, no
      # `nix profile install` from github, no `create-files` at init. The
      # `runtime` and `files` init stages stay empty; `create-files` is a
      # build-time step only (the tarball already bakes /etc, /sbin/init, …).
      store.init.services = lib.mkAfter /* bash */ ''
        # The store's /nix may be a fresh named volume (Podman) or persistent
        # VHD (WSL). `nix daemon` initializes the db on first start.
        # shellInit exports NIX_REMOTE=daemon for *clients*; the daemon itself
        # must open the local store directly.
        unset NIX_REMOTE
        exec nix daemon
      '';
    }
  ]);

}
