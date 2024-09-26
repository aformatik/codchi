# Nix

If you're editing Nix files often (e.g when working on a local code machine module), it makes sense to add editor support for Nix:

```nix
{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    # Formatter
    pkgs.nixpkgs-fmt

    # VSCode
    (vscode-with-extensions.override {
      vscodeExtensions = with vscode-extensions; [
        bbenoist.nix
      ];
    })

    # A language server if you have an editor with LSP support:
    pkgs.nixd
    # or
    pkgs.nil
  ];
}
```

For more options see <https://wiki.nixos.org/wiki/Editor_Modes_for_Nix_Files>
