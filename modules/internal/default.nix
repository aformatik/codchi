{ pkgs, ... }:
{
  imports = [
    ./rootfs.nix

    ./lxd
    ./wsl
  ];

  config = {

    # Copy .desktop files and corresponding icons (converted to .ico) to
    # /run/current-system/sw/share/codchi. We copy instead of symlink them so
    # that drivers like WSL can see them.
    environment.extraSetup = ''
      pushd $out/share

      mkdir -p codchi/{icos,applications}

      for app_path in applications/*.desktop; do
        if grep -q ^NoDisplay=true "$app_path" || grep -q ^Hidden=true "$app_path"; then
          echo "Skipping hidden $app_path" >&2
        else
          APP_NAME="$(basename "$app_path" | sed 's/\.desktop//')"
          ICON_NAME="$(grep Icon "$app_path" | sed 's/Icon=//')"
          DIRS=
          [ -d icons ] && DIRS="$DIRS icons"
          [ -d pixmaps ] && DIRS="$DIRS pixmaps"
          ICON_PATH="$(find -L $DIRS -name "$ICON_NAME.*" | sort -rV | head -n1)"
          if [ ! -z "$ICON_PATH" ]; then
            ${pkgs.imagemagick}/bin/convert \
              -background transparent \
              -define icon:auto-resize=16,24,32,48,64,72,96,128,256 \
              "$ICON_PATH" "codchi/icos/$APP_NAME.ico"
          fi
          cp "$app_path" codchi/applications
        fi
      done

      popd
    '';

    # Make sure all profiles are recorded as gcroots
    system.activationScripts."add-gcroots" = ''
      mkdir -p /nix/var/nix/gcroots
      ln -fs /nix/var/nix/profiles /nix/var/nix/gcroots/
    '';
  };
}
