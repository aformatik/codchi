# JVM

::: tip
Try it out with `codchi init <NAME> https://github.com/aformatik/codchi nixosModules.jvm`.
:::


## General, Java & Kotlin

IntelliJ wants to download JDKs automatically, but they don't work on NixOS. Therefore you need to enable `programs.java` which sets `$JAVA_HOME` to the default JDK (OpenJDK) which IntelliJ can use. If multiple JDKs are needed, use [`programs.java.packages`](../99.Codchi specific NixOS Options.md#programsjavapackages) (an option added by Codchi) which installes them in `~/.jdks`.

```nix
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

  ];

  # For proprietary apps like IntelliJ Ultimate
  # nixpkgs.config.allowUnfree = true;
}
```

## Scala

```nix
{ pkgs, ... }: {
  # Will be recognized by IntelliJ
  environment.systemPackages = [ pkgs.scala pkgs.sbt ];
}
```

## Eclipse

```nix
{ pkgs, ... }: {
  environment.systemPackages = [ 
    pkgs.eclipses.eclipse-java
    # for JEE
    # pkgs.eclipses.eclipse-jee 
  ];
}
```

