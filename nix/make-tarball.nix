{ runCommand
, writeShellScriptBin
, lib

, contents
, fileName
, compressCommand ? "gzip"
, compressionExtension ? ".gz"

, gzip
, extraBuildInputs ? [ gzip ]

, ...
}:

let
  inherit (lib) concatStringsSep pipe mapAttrsToList;

  createContents = writeShellScriptBin "create-contents" ''
    for file in ${toString (builtins.attrNames contents)}; do
      if [[ "$file" == */ ]]; then
        mkdir -p ".$file"
      else
        mkdir -p ".$(dirname $file)"
      fi
    done

    ${(pipe contents [
      (mapAttrsToList (path: content: "cp -af ${content} .${path} && chmod -R +w .${path}"))
      (concatStringsSep "\n")
    ])}
  '';
in
runCommand fileName
{
  buildInputs = extraBuildInputs;
  passthru.createContents = createContents;
}
  ''
    set -e
    set -o pipefail

    mkdir build && cd build
    
    ${createContents}/bin/create-contents

    mkdir $out
    tar --sort=name --mtime='@1' --owner=0 --group=0 --numeric-owner -c * | ${compressCommand} > $out/${fileName}.tar${compressionExtension}

  ''
