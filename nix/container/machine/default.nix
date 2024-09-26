{ pkgs, lib, config, ... }:
let
  inherit (lib) mkOption mkEnableOption types mkIf;
  cfg = config.machine;
in
{
  imports = [
    ./lxd.nix
    ./wsl.nix
  ];

  options.machine = {
    enable = mkEnableOption "machine container";
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
        hostSetup = mkInitStage "Create directories and mount host directories.";
      };
  };

  config = mkIf cfg.enable {

    name = "machine";

    binPackages = with pkgs.pkgsStatic; [
      busybox
      bashInteractive

      (pkgs.writeLoginShellScriptBinStatic "run" /* bash */ ''
        exec "$@"
      '')

      (pkgs.writeLoginShellScriptBinStatic "runin" /* bash */ ''
        source <(cat -)
      '')
    ];

    # modeled after <nixos>/nixos/modules/system/boot/stage-2-init.sh
    files."/sbin/init" = pkgs.writeShellScriptStatic "init"
      /* bash */
      ''
        set -euo pipefail

        # Use config.system.binPackages and PATH from host
        export LANG="C.UTF-8" HOME=/root PATH="/bin:$PATH"
        ${cfg.init.hostSetup}

        if [ -n "''${CODCHI_DEBUG:-}" ]; then
          set -x
        fi

        # Required by the activation script
        install -m 0755 -d /etc /etc/nixos
        install -m 01777 -d /tmp

        # Run the script that performs all configuration activation that does
        # not have to be done at boot time.
        echo "running activation script..."
        /nix/var/nix/profiles/system/activate

        # Record the boot configuration.
        ln -sfn "$(readlink -f /nix/var/nix/profiles/system)" /run/booted-system

        # Ensure systemd doesn't try to populate /etc, by forcing its first-boot
        # heuristic off. It doesn't matter what's in /etc/machine-id for this purpose,
        # and systemd will immediately fill in the file when it starts, so just
        # creating it is enough. This `: >>` pattern avoids forking and avoids changing
        # the mtime if the file already exists.
        : >> /etc/machine-id

        # TODO ?? Reset the logging file descriptors.

        echo "starting systemd..."
        exec /run/current-system/systemd/lib/systemd/systemd "--log-target=kmsg" "$@"
      '';
  };

}
