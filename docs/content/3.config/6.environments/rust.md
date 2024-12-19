# Rust

## Rust via flakes

::alert{type="info"}
Try it out with `codchi clone <NAME> https://github.com/aformatik/codchi nixosModules.codchi`.
::

There is already great support for Rust development environments via Nix Flakes. Additionally to providing a development environment, this method also allows building the Rust project itself. Codchi itself uses this approach.

To use this method with Codchi, just install an IDE (and optionally nix-direnv) like [Codchi's code machine](https://github.com/aformatik/codchi/blob/master/configuration.nix) does: 

```nix
{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.vscode-with-extensions.override {
      vscode = pkgs.vscodium;
      vscodeExtensions = with pkgs.vscode-extensions; [
        rust-lang.rust-analyzer
        jnoortheen.nix-ide
        mkhl.direnv
        asvetliakov.vscode-neovim
      ];
    })
    pkgs.bashInteractive # fix terminal in VSCode
  ];
  programs.neovim.enable = true; # needed for vscode-neovim plugin
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
```

When not using direnv and its IDE integration, the IDE must be started from within the Nix Flakes devshell:
```bash
cd my/project
nix develop
codium .
```

## Rust via NixOS

Cargo can also be installed globally. For this we recommend [oxalica/rust-overlay](https://github.com/oxalica/rust-overlay), a rustup-like distribution of rust within the Nix ecosystem.

::alert{type="info"}
Try it out with `codchi init <NAME> https://github.com/aformatik/codchi nixosModules.rust`.
::


```nix
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

    # When using VSCode instead of RustRover
    # (pkgs.vscode-with-extensions.override {
    #   vscode = pkgs.vscodium;
    #   vscodeExtensions = with pkgs.vscode-extensions; [
    #     rust-lang.rust-analyzer
    #     jnoortheen.nix-ide
    #   ];
    # })
    # pkgs.bashInteractive # fix terminal in VSCode
  ];

  # For proprietary apps like RustRover
  nixpkgs.config.allowUnfree = true;
}
```
