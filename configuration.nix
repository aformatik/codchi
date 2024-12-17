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
  programs.neovim.enable = true;
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
