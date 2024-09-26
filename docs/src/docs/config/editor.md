# Editor

## VSCode

VSCode can be had in two flavours: The proprietary `pkgs.vscode` from Microsoft and the free `pkgs.vscodium`. On top of that VSCode can be installed preconfigured with plugins. When manually installing plugins via VSCode, there can be issues if a plugin tries to install native binaries. In this case `pkgs.vscode-fhs` and `pkgs.vscodium-fhs` provide a FHS environment in which most plugins should work.
See <https://wiki.nixos.org/wiki/Vscode> for more info.

```nix
{ pkgs, ... }: {
  environment.systemPackages = [
    # Plain VSCode
    pkgs.vscodium

    # With plugins
    (pkgs.vscode-with-extensions.override {
      vscode = pkgs.vscodium;
      vscodeExtensions = with vscode-extensions; [
        bbenoist.nix
        ms-python.python
        ms-azuretools.vscode-docker
        ms-vscode-remote.remote-ssh
      ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "remote-ssh-edit";
          publisher = "ms-vscode-remote";
          version = "0.47.2";
          sha256 = "1hp6gjh4xp2m1xlm1jsdzxw9d8frkiidhph6nvl24d0h8z34w49g";
        }
      ];
    })

    # FHS env
    pkgs.vscodium-fhs
  ];
}
```

## Jetbrains IDEs

See chapter [JVM](./examples/jvm.md#general-java--kotlin) on how to setup JDKs for Java IDEs like IntelliJ.

All Jetbrains IDEs are available under `pkgs.jetbrains.<IDE>`. See the [NixOS Search](https://search.nixos.org/packages?channel=unstable&from=0&size=51&sort=relevance&type=packages&query=jetbrains) for a list of available IDEs.

There is also the experimental `pkgs.jetbrains.plugins.addPlugidea-communityins` which can add plugins to a Jetbrains IDE. See <https://github.com/NixOS/nixpkgs/tree/nixos-24.05/pkgs/applications/editors/jetbrains> for more details.

```nix
{ pkgs, ... }: {

  # Setup JDKs if neccessary

  environment.systemPackages = [
    pkgs.jetbrains.idea-community

    pkgs.jetbrains.idea-ultimate

    # With plugins
    (with pkgs.jetbrains; plugins.addPlugins pycharm-community [
      "nixidea"
      "ideavim"
    ])

  ];

  # For proprietary apps like IntelliJ Ultimate
  nixpkgs.config.allowUnfree = true;
}
```

## Vim

There are a few options available:

- Plain `pkgs.vim` / `pkgs.neovim`
- Configuring NeoVim via NixOS options: <https://wiki.nixos.org/wiki/Neovim#Configuration>
- [LunarVim](https://www.lunarvim.org) `pkgs.lunarvim`
- [nixvim](https://nix-community.github.io/nixvim/)
