{ lib, ... }:
let
  inherit (lib) mkEnableOption;
in
{
  options.codchi = {
    gpu.enable = mkEnableOption "OpenGL / GPU driver from the host" // { default = true; };
  };

}
