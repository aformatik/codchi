{ lib
, targetPlatform # one of ["linux" "windows"]
, buildRustCodchi
}:
let
  Cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
buildRustCodchi targetPlatform {
  pname = "codchi";
  inherit (Cargo.workspace.package) version;

  src = lib.sourceByRegex ./. [
    "^codchi-api.*$"
    "^codchi-server.*$"
    "^codchi-cli.*$"
    "^codchi-shared.*$"
    "^codchi-container-utils.*$"
    "^Cargo\..*"
  ];
  cargoBuildFlags = [ "-p" "codchi-server" "-p" "codchi-cli" ];
  cargoLock.lockFile = ./Cargo.lock;
}
