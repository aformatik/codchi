{
  inputs.super.url = "path:..";
  inputs.nixpkgs.follows = "super/nixpkgs";

  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [ nodejs nodePackages.npm ];
      };
    };
}
