{ lib
, makeRustPlatform
, rust-bin
, ...
}:
let
  Cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);
  CargoWorkspace = builtins.fromTOML (builtins.readFile ../Cargo.toml);

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
  inherit (CargoWorkspace.workspace.package) version;

  src = lib.sourceByRegex ./.. [
    "^codchi.*$"
    "^codchi-server.*$"
    "^codchi-gui.*$"
    "^codchiw.*$"
    "^shared.*$"
    "^ipc.*$"
    "^utils.*$"
    "^Cargo\..*"
  ];
  cargoLock.lockFile = ../Cargo.lock;
  cargoLock.outputHashes = {
    "git-url-parse-0.4.5" = "sha256-q3lrdWE+WpAI0FSbpzUabk9aPCjzoqIHvNoDmqRl2BY=";
  };

  CARGO_BUILD_RUSTFLAGS = [ "-C" "target-feature=+crt-static" ];
  CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";

  buildPhase = ''
    runHook preBuild
    cargo build --release -p utils
    runHook postBuild
  '';

  checkPhase = ''
    runHook preCheck
    cargo check --release -p utils
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp target/${CARGO_BUILD_TARGET}/release/ndd $out/bin
    runHook postInstall
  '';

}
