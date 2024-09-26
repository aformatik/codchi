{ pkgs, ... }: {
  programs.java = {
    enable = true;
    # Set default JDK
    package = pkgs.jdk21;
    packages = {
      # Additional JDKs which get symlinked to ~/.jdks/<attribute name> (for e.g. IntelliJ)
      openjdk8 = pkgs.jdk8;
      # ...
    };
  };


  environment.systemPackages = [
    pkgs.maven
    # If you want gradle globally installed
    # pkgs.gradle

    # IntelliJ Community
    # pkgs.jetbrains.idea-community
    # You can also install IJ with plugins (experimental).
    # For more information see <https://github.com/NixOS/nixpkgs/tree/nixos-24.05/pkgs/applications/editors/jetbrains>
    (with pkgs.jetbrains; plugins.addPlugins idea-community [
      "nixidea"
      "ideavim"
    ])

    # Scala:
    # pkgs.scala 
    # pkgs.sbt 

    # Eclipse:
    # pkgs.eclipses.eclipse-java
    # for JEE:
    # pkgs.eclipses.eclipse-jee 
  ];

  # For proprietary apps like IntelliJ Ultimate
  # nixpkgs.config.allowUnfree = true;
}
