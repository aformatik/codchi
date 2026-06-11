{ lib
, makeRustPlatform
, rust-bin
, removeReferencesTo
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
    "^codchi-api.*$"
    "^codchi-server.*$"
    "^codchi-cli.*$"
    "^codchi-shared.*$"
    "^codchi-container-utils.*$"
    "^Cargo\..*"
  ];
  cargoLock.lockFile = ../Cargo.lock;

  CARGO_BUILD_RUSTFLAGS = [ "-C" "target-feature=+crt-static" ];
  CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";

  buildPhase = ''
    runHook preBuild
    cargo build --release -p codchi-container-utils
    runHook postBuild
  '';

  checkPhase = ''
    runHook preCheck
    cargo check --release -p codchi-container-utils
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp target/${CARGO_BUILD_TARGET}/release/ndd $out/bin
    runHook postInstall
  '';

  nativeBuildInputs = [ removeReferencesTo ];

  # ndd is a static-musl binary, but the `rust-src` extension bakes std
  # panic-location source paths (".../rust-default-*/lib/rustlib/src/...") into
  # it as string data. Stripping leaves them, so Nix retains a ~1.8 GB reference
  # to the whole Rust toolchain. The paths are display-only; null them out so the
  # deployed/closure footprint is just the static binary (S8).
  postInstall = ''
    remove-references-to -t ${rust} $out/bin/ndd
  '';

  disallowedReferences = [ rust ];
}
