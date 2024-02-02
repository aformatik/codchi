{ runCommandLocal
, lib
, ...
}:

# contents :: { [Target Path]: Source Path }
contents:

runCommandLocal "build-hierarchy" { } ''
  set -euo pipefail

  makeEntry() {
    target="$1"
    src="$2"

    if [[ "$src" = *'*'* ]]; then
      # If the source name contains '*', perform globbing.
      mkdir -p "$out/$target"
      for fn in $src; do
          ln -s "$fn" "$out/$target/"
      done
    else
      mkdir -p "$out/$(dirname "$target")"
      if ! [ -e "$out/$target" ]; then
        ln -s "$src" "$out/$target"
      else
        echo "duplicate entry $target -> $src"
        if [ "$(readlink "$out/etc/$target")" != "$src" ]; then
          echo "mismatched duplicate entry $(readlink "$out/etc/$target") <-> $src"
          ret=1

          continue
        fi
      fi
    fi
  }

  ${with lib; pipe contents [
    (filterAttrs (_: src: src != null))
    (mapAttrsToList (target: src: escapeShellArgs ["makeEntry" target "${src}" ]))
    concatLines
  ]}
''
