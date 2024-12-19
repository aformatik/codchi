{ pkgs, ... }: {

  # https://github.com/oxalica/rust-overlay
  nixpkgs.overlays = [
    (self: super:
      let
        overlay = super.fetchFromGitHub {
          repo = "rust-overlay";
          owner = "oxalica";
          sha256 = "1bp1k5qla5gwh6vc50m5pcwdfxn6g703yx1i8qrjs4l7kgh3y507";
          rev = "573c674a3ad06e8a525263185ebef336a411d1d5";
        };
      in
      {
        inherit (import overlay self super) rust-bin;
      }
    )
  ];

  environment.systemPackages = [

    pkgs.gcc

    # https://github.com/oxalica/rust-overlay
    (
      let
        rustConfig = {
          extensions = [
            "rust-src"
            "rust-analyzer"
          ];
          targets = [
            "x86_64-unknown-linux-gnu"
            "wasm32-wasi"
          ];
        };
      in
      # stable
      pkgs.rust-bin.stable.latest.default.override rustConfig
      # nightly
      # pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override rustConfig);
    )

    # You can also install IJ with plugins (experimental).
    # For more information see <https://github.com/NixOS/nixpkgs/tree/nixos-24.05/pkgs/applications/editors/jetbrains>
    (with pkgs.jetbrains; plugins.addPlugins rust-rover [
      "nixidea"
    ])

    (pkgs.vscode-with-extensions.override {
      vscode = pkgs.vscodium;
      vscodeExtensions = with pkgs.vscode-extensions; [
        rust-lang.rust-analyzer
        jnoortheen.nix-ide
      ];
    })
    pkgs.bashInteractive # fix terminal in VSCode
  ];

  # For proprietary apps like RustRover
  nixpkgs.config.allowUnfree = true;
}
