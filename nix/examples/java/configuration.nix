{ pkgs, ... }: {

  programs.java = {
    enable = true;

    # default JDK which is added to $PATH and $JAVA_HOME
    package = pkgs.jdk17;

    # JDKs which are linked to ~/.jdks where IntelliJ can find them.
    packages = {
      openjdk17 = pkgs.jdk17;
    #   jdk19 = pkgs.jdk19;
    };
  };

  environment.systemPackages = [
    pkgs.maven
    # pkgs.jetbrains.idea-community
  ];
}
