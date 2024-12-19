# Python

::alert{type="info"}
Try it out with `codchi init <NAME> https://github.com/aformatik/codchi nixosModules.python`.
::

```nix
{ pkgs, ... }: {
  environment.systemPackages = [
    pkgs.python39
    # Pip on NixOS only works in virtualenv
    pkgs.python39Packages.pip
    pkgs.python39Packages.virtualenv

    # Editor:
    # See <https://codchi.dev/config/editor> for more info
    pkgs.jetbrains.pycharm-community
    # Or VSCode(ium)
    (pkgs.vscode-with-extensions.override {
      vscode = pkgs.vscodium;
      vscodeExtensions = with pkgs.vscode-extensions; [
        ms-python.python
      ];
    })
  ];
}
```
