{ buildEnv
, findutils
, ...
}:

# Passthru args to pkgs.buildEnv
args:

buildEnv (
  args
    // {
    postBuild = (args.postBuild or "") + /* bash */ ''
      set -euo pipefail
      for f in $(${findutils}/bin/find "$out" -type l); do
        real_file="$(readlink "$f")"
        rm "$f"
        cp -af "$real_file" "$f"
      done
    '';
  }
)
