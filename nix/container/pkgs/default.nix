_self: super: {

  buildEnvCopy = super.callPackage ./buildenv-copy.nix { };

  makeTarball = super.callPackage ./make-tarball.nix { };

  buildHierarchy = super.callPackage ./build-hierarchy.nix { };

  writeShellScriptStatic = name: text: super.writeTextFile {
    inherit name;
    executable = true;
    text = ''
      #!/bin/bash
      ${text}
    '';
    checkPhase = ''
      ${super.stdenv.shellDryRun} "$target"
    '';
  };

  writeShellScriptBinStatic = name: text: super.writeTextFile {
    inherit name;
    executable = true;
    destination = "/bin/${name}";
    text = ''
      #!/bin/bash
      ${text}
    '';
    checkPhase = ''
      ${super.stdenv.shellDryRun} "$target"
    '';
    meta.mainProgram = name;
  };

  writeLoginShellScriptBinStatic = name: text: super.writeTextFile {
    inherit name;
    executable = true;
    destination = "/bin/${name}";
    text = ''
      #!/bin/bash -l
      ${text}
    '';
    checkPhase = ''
      ${super.stdenv.shellDryRun} "$target"
    '';
    meta.mainProgram = name;
  };

}
