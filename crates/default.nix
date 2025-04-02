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
    "^codchi.*$"
    "^codchi-server.*$"
    "^codchi-gui.*$"
    "^codchiw.*$"
    "^shared.*$"
    "^ipc.*$"
    "^utils.*$"
    "^Cargo\..*"
  ];
  cargoBuildFlags = [ "-p codchi" "-p codchi-server" "-p codchi-gui" ]
    ++ lib.optional (targetPlatform == "windows") "-p codchiw";
  cargoLock.lockFile = ./Cargo.lock;
  cargoLock.outputHashes = {
    # "tray-icon-0.16.0" = "sha256-LxkEP31myIiWh6FDOzr9rZ8KAWISbja0jmEx0E2lM44=";
    # "clap-4.5.19" = "sha256-YRuZlp7jk05QLI551shgcVftcqKytTkxHlKbVejT1eE=";
    "git-url-parse-0.4.5" = "sha256-q3lrdWE+WpAI0FSbpzUabk9aPCjzoqIHvNoDmqRl2BY=";
  };
}
