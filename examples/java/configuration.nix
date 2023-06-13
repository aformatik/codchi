{ pkgs, ... }: {

  devenv.java = {
    enable = true;
    package = pkgs.jdk17;
    jdks = { inherit (pkgs) jdk17 jdk20; };
    tools = [ pkgs.maven pkgs.jetbrains.idea-community ];
  };
}
