{
  inputs.super.url = "path:..";
  inputs.nixpkgs.follows = "super/nixpkgs";

  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.vcxsrv-bin = pkgs.fetchurl {
        url = "https://github.com/marchaesen/vcxsrv/releases/download/21.1.13/vcxsrv-64.21.1.13.0.installer.exe";
        sha256 = "sha256-lwJDsOLGUpSSJdJQvzfofi8yFDXU03V+lP92LkV4ooE=";
      };
      # packages.${system}.vcxsrv-bin = pkgs.fetchurl {
      #   url = "https://github.com/marchaesen/vcxsrv/releases/download/21.1.10/vcxsrv-64.21.1.10.0.installer.exe";
      #   sha256 = "sha256-8L3D8XoqTAkXLH0Pndays/lbH9uyeUvY/9pHTGNsztM=";
      # };
    };
}
