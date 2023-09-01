{ runCommand
, writeShellScriptBin
, lib

, fileName

  # See documentation for `codchi.internal.init.rootfsContents` in modules/internal/rootfs.nix.
  # These are included as-is in passthru.createContents
, contents
  # Function which transforms contents. The result is included in the final tarball
, overrideContents ? x: x

, compressCommand ? "gzip"
, compressionExtension ? ".gz"

, gzip
, extraBuildInputs ? [ gzip ]
, extraCommands ? ""

, ...
}:

let
  inherit (lib) concatStringsSep pipe mapAttrsToList;

  createContents = contents: writeShellScriptBin "create-contents" ''
    for file in ${toString (builtins.attrNames contents)}; do
      if [[ "$file" == */ ]]; then
        mkdir -p ".$file"
      else
        mkdir -p ".$(dirname $file)"
      fi
    done

    ${(pipe contents [
      (mapAttrsToList 
        (path: content: 
          if content != null
            then "cp -af ${content} .${path} && chmod -R +w .${path}"
            else "#dir ${path} stays empty"
        )
      )
      (concatStringsSep "\n")
    ])}
  '';
in
runCommand fileName
{
  buildInputs = extraBuildInputs;
  passthru.createContents = createContents contents;
}
  ''
    set -e
    set -o pipefail

    mkdir build && cd build
    
    ${createContents (overrideContents contents)}/bin/create-contents

    ${extraCommands}

    mkdir $out
    tar --sort=name --mtime='@1' --owner=0 --group=0 --numeric-owner -c * | ${compressCommand} > $out/${fileName}.tar${compressionExtension}

  ''
