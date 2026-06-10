{ callPackage
, lib
, self
, platform
, ...
}:
let
  Cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
callPackage
  ../build/build-rust-package.nix
{ inherit self platform; }
{
  pname = "codchiw";
  inherit (Cargo.package) version;
  src = lib.sourceByRegex ./. [
    "^src.*$"
    "^Cargo\..*"
  ];
  cargoLock.lockFile = ./Cargo.lock;
  passthru = { };
}
