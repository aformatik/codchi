{ pkgs, ... }: {

  nixpkgs.config.allowUnfree = true;
  codchi.docker.enable = true;
  codchi.docker.enableNvidia = true;
  environment.systemPackages = [ pkgs.nvtop ];
}
