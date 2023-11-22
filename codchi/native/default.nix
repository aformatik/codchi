{ rustPlatform
, pkg-config
, gtk3
, python3
, xorg
, libxkbcommon
}:
let
  Cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);
in
rustPlatform.buildRustPackage {
  pname = Cargo.package.name;
  inherit (Cargo.package) version;

  src = ./.;
  cargoLock.lockFile = ./Cargo.lock;

  nativeBuildInputs = [ pkg-config python3 ];
  buildInputs = [
    gtk3
    xorg.libxcb
    xorg.libX11
    libxkbcommon
  ];
}
