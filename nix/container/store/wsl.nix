{ lib, config, pkgs, consts, ... }:
let inherit (lib) mkEnableOption mkIf;
  # INIT_ENV = "/.store-init-env";
  # INIT_LOG = "/.store-init-log";
in
{

  options.store.driver.wsl = {
    enable = mkEnableOption "WSL specific settings";
  };

  config = mkIf config.store.driver.wsl.enable {
    files = {
      "etc/wsl.conf" = pkgs.writeText "wsl.conf" (lib.generators.toINI { } {
        automount.mountFsTab = false;
        # boot.command = "/bin/run /sbin/init";
      });
      "etc/profile" = pkgs.writeText "profile" /* bash */ ''
        if [ -z "''${CODCHI_IS_STORE:-}" ]; then
          echo "This distribution is managed by codchi.exe. DO NOT USE OR DELETE IT, or you might loose all of your data inside codchi!"
          exit 1
        fi
      '';
      "etc/bashrc" = pkgs.writeText "bashrc" /* bash */ ''
        . /etc/profile
      '';
    };

    # Print everything to stderr and also to the log file
    # exec > >(tee -i "${consts.store.INIT_LOG}" >&2) 1>&2
    # trap 'echo ${consts.INIT_EXIT_ERR} >&2' ERR

    # WSL doesn't supply WSLENV to the init, so we have to use a temporary
    # file (/store-start-env)

    # while [ ! -f "${consts.store.INIT_ENV}" ]; do
    #     echo -e '\e[1A\e[KWaiting for store init env...'
    #     sleep .25
    # done

    # source "${consts.store.INIT_ENV}"
    # rm "${consts.store.INIT_ENV}"

    store.init.filesystem = lib.mkAfter /* bash */ ''
      # init is started manually in WSL, so we dont need to manually write to a logfile
      
      if [ -z "''${CODCHI_IS_STORE:-}" ]; then
        /mnt/c/WINDOWS/system32/msg.exe '*' "This distribution is only meant to be started by codchi.exe!" >&2
        logE "This distribution is only meant to be started by codchi.exe!"
        exit 1
      fi

      if [ -z "''${WSL_CODCHI_DIR_CONFIG:-}" ] || [ ! -d "''${WSL_CODCHI_DIR_CONFIG:-}" ]; then
        /mnt/c/WINDOWS/system32/msg.exe '*' \
          "Environment variable \$WSL_CODCHI_DIR_CONFIG not set or host directory doesn't exist." >&2
        logE "Environment variable \$WSL_CODCHI_DIR_CONFIG not set or host directory doesn't exist."
        exit 1
      fi
      
      mkLink() {
        src="$1"
        target="$2"
        echo "Linking $src to $target" >&2
        [ -d "$(dirname $target)" ] || mkdir -p "$(dirname $target)" 
        [ -L "$target" ] && rm "$target"
        ln -fs "$src" "$target"
      }

      mkLink "$WSL_CODCHI_DIR_CONFIG" "${consts.store.DIR_CONFIG}" 
      mkLink "$WSL_CODCHI_DIR_CONFIG" "/mnt/wsl/codchi${consts.store.DIR_CONFIG}"

      mkMnt() {
        src="$1"
        target="$2"
        echo "Mounting $src on $target" >&2
        while mount | grep -wq "$target"; do
          umount "$target"
        done
        [ -d "$src" ] || mkdir -p "$src"
        [ -d "$target" ] || mkdir -p "$target"
        mount --bind "$src" "$target"
      }

      mkMnt "/data" "/mnt/wsl/codchi/data"
      mkMnt "/nix"  "/mnt/wsl/codchi/nix"
    '';

    # this is needed to keep WSL running (otherwise it shuts down after 8 sec.)
    runtimePackages = with pkgs; [ daemonize ];

    store.init.services = lib.mkForce /* bash */ ''
      setup_bridge() {
        echo "Creating codchibr..." >&2
        BR_ADDR="${consts.store.NETNS_BRIDGE_ADDR}"
        BR_DEV="codchibr"

        # setup bridge
        ip link delete "$BR_DEV" &>/dev/null || true
        ip link add "$BR_DEV" type bridge
        ip link set "$BR_DEV" up
        ip addr add "$BR_ADDR/24" dev "$BR_DEV"

        # enable ip forwarding
        echo 1 > /proc/sys/net/ipv4/ip_forward

        # flush & apply nat rules
        nix run nixpkgs#iptables -- -t nat -F
        nix run nixpkgs#iptables -- -t nat -A POSTROUTING -s "$BR_ADDR/24" ! -o "$BR_DEV" -j MASQUERADE
      }

      setup_bridge || echo "Failed to setup codchibr. Network namespaces will be disabled." >&2
    
      daemonize $(which nix) daemon
    '';
  };
}

