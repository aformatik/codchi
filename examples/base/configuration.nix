{ pkgs, ... }: {
  programs.neovim.enable = true;
  environment.systemPackages = [ pkgs.lf pkgs.gimp pkgs.xorg.xeyes ];
}
