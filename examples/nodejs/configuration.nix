{ pkgs, ... }: {

  programs.npm.enable = true;
  environment.systemPackages = [ pkgs.nodejs ];

}
