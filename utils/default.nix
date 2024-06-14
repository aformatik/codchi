{ lib
, makeRustPlatform
, rust-bin
, ...
}:
let
  Cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);

  rustConfig = {
    extensions = [ "rust-src" ];
    targets = [ "x86_64-unknown-linux-musl" ];
  };
  rust = rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override rustConfig);
  rustPlatform = makeRustPlatform {
    cargo = rust;
    rustc = rust;
  };

in
rustPlatform.buildRustPackage rec {
  pname = Cargo.package.name;
  inherit (Cargo.package) version;

  src = lib.sourceByRegex ./. [
    "^src.*$"
    "^Cargo\..*"
  ];
  cargoLock.lockFile = ./Cargo.lock;

  CARGO_BUILD_RUSTFLAGS = [ "-C" "target-feature=+crt-static" ];
  CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";

  buildPhase = ''
    runHook preBuild
    cargo build --release
    runHook postBuild
  '';

  checkPhase = ''
    runHook preCheck
    cargo check --release
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp target/${CARGO_BUILD_TARGET}/release/ndd $out/bin
    runHook postInstall
  '';

}
