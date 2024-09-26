{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { ... }: {
    nixosModules.default = import ./configuration.nix;
  };
}
