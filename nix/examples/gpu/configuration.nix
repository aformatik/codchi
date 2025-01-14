{ pkgs, ... }: {

  nixpkgs.config.allowUnfree = true;
  codchi.docker.enable = true;
  codchi.docker.enableNvidia = true;
  environment.systemPackages = [ pkgs.nvtop ];

  # You must configure `hardware.nvidia.open` on NVIDIA driver versions >= 560.
  # It is suggested to use the open source kernel modules on Turing or later
  # GPUs (RTX series, GTX 16xx), and the closed source modules otherwise.
  hardware.nvidia.open = false;
}
