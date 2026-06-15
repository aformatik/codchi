{ pkgs, config, lib, ... }:
let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.codchi.driver.podman;
in
{
  options.codchi.driver.podman.enable = mkEnableOption "Podman driver"
    // { internal = true; readonly = true; };

  config = mkIf cfg.enable {

    codchi.driver.name = "podman";

    # The Linux host understands standard freedesktop icons directly, so just
    # copy them through (same as LXD).
    codchi.driver.iconCommand = lib.mkDefault ''
      cp "$ICON_PATH" "codchi/icons/"
    '';

    # Rootless Podman runs the machine's systemd as an unprivileged user inside a
    # user namespace. systemd's per-service sandboxing directives (ProtectProc,
    # ProtectControlGroups, ProtectKernelTunables, NoNewPrivileges, …) require
    # privileges the container does not have; left on, services such as
    # dbus-broker fail to set up their sandbox and crash-loop, wedging boot.
    # Relax them container-wide, mirroring the LXD distrobuilder overrides.
    systemd.packages = [
      (pkgs.writeTextFile {
        name = "systemd-podman-service-overrides";
        destination = "/etc/systemd/system/service.d/zzz-podman-service.conf";
        text = ''
          [Service]
          ProcSubset=all
          ProtectProc=default
          ProtectControlGroups=no
          ProtectKernelTunables=no
          NoNewPrivileges=no
          LoadCredential=
        '';
      })
    ];

    # X11 forwarding: the host's X socket and Xauthority are bind-mounted in by
    # `codchi-server` (see the podman option matrix). Register the host magic
    # cookie under the in-container DISPLAY so GUI apps authenticate. Same
    # approach as the LXD driver.
    environment.extraInit = /* bash */ ''
      if [ -n "''${XAUTHORITY:-}" ]; then
        DISPLAY="''${DISPLAY:-0}"
        COOKIE="$(${lib.getExe (pkgs.xauth or pkgs.xorg.xauth)} list | tr -s ' ' | cut -f 3 -d ' ' | head -n 1)"
        if [ -z "$COOKIE" ]; then
          echo "[codchi] Failed to setup xauth (no cookie found). You might not be able to run GUI apps." >&2
        else
          ${lib.getExe (pkgs.xauth or pkgs.xorg.xauth)} add "$DISPLAY" . "$COOKIE"
        fi
      fi
    '';
  };
}
