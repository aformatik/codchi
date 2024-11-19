{ runCommand
, writeShellScriptBin
, lib
, findutils
, gzip
, ...
}:
{ fileName
  # A list of derivations which are copied inside the tarball. Use
  # pkgs.buildEnvCopy and pkgs.writeTextFileDir to create the file hierarchy.
  # **Important**: Absolute symlinks are dereferenced to avoid references to
  # the nix store which means that when creating intermediate derivations like
  # with `pkgs.buildEnv`, all files are copied. Packages like busybox have many
  # symlinks pointing to the same file which results in duplicate binaries. To
  # avoid this use pkgs.buildEnvCopy instead.
  # contents :: [ Package ]
, contents

  # Use this to create directories, move files, etc.
  # extraCommands :: Bash script
, extraCommands ? ""

, compressCommand ? "gzip"
, compressionExtension ? ".gz"

, extraBuildInputs ? [ gzip ]

, ...
}:

let
  createFiles = writeShellScriptBin "create-files" ''
    set -euo pipefail

    if [ ! -e .files ]; then
      touch .files
    fi
    mv .files .files_old

    # We force overwrite $f. Alternative: Adapt setup-etc.pl from NixOS
    for pkg in ${toString contents}; do

      pushd "$pkg" > /dev/null
      rel_links="$(${findutils}/bin/find . -type l ! -lname '/*')"
      files="$(${findutils}/bin/find . -type f -o \( -type l -a -lname '/*' \))"
      popd > /dev/null

      # Copy relative symlinks as is
      for f in $rel_links; do
        mkdir -p "$(dirname "$f")"
        [ -e "$f" ] && rm -f "$f"
        cp -af "$pkg/$f" "$f"
        echo "$f" >> .files
      done

      # Copy files and derefenrence absolute symlinks as is
      for f in $files; do
        mkdir -p "$(dirname "$f")"
        [ -e "$f" ] && rm -f "$f"
        cp -afL "$pkg/$f" "$f"
        echo "$f" >> .files
      done
    done

    # remove old files
    comm -23 <(cat .files_old | sort) <(cat .files | sort) | xargs -r rm -f
    rm .files_old
  '';
in
runCommand "${fileName}.tar${compressionExtension}"
{
  allowedReferences = [ ];
  buildInputs = extraBuildInputs;
  passthru.createFiles = createFiles;
}
  ''
    set -euo pipefail

    mkdir build && cd build
    
    ${lib.getExe createFiles}

    ${extraCommands}

    tar --sort=name --mtime='@1' --owner=0 --group=0 --numeric-owner -c . | ${compressCommand} > $out
  ''
