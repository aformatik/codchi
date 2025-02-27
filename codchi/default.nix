{ callPackage
, lib
, self
, platform
}:
let
  Cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
callPackage ../build/build-rust-package.nix
{ inherit self platform; }
{
  pname = "codchi";
  inherit (Cargo.package) version;

  src = lib.sourceByRegex ./. [
    "^src.*$"
    "^assets.*$"
    "^Cargo\..*"
    "^build.*$"
  ];
  cargoLock.lockFile = ./Cargo.lock;
  cargoLock.outputHashes = {
    # "tray-icon-0.16.0" = "sha256-LxkEP31myIiWh6FDOzr9rZ8KAWISbja0jmEx0E2lM44=";
    # "clap-4.5.19" = "sha256-YRuZlp7jk05QLI551shgcVftcqKytTkxHlKbVejT1eE=";
    "git-url-parse-0.4.5" = "sha256-q3lrdWE+WpAI0FSbpzUabk9aPCjzoqIHvNoDmqRl2BY=";
  };
}
