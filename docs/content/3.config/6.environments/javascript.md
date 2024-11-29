# Javascript

::: tip
Try it out with `codchi init <NAME> https://github.com/aformatik/codchi nixosModules.javascript`.
:::

```nix
{ pkgs, ... }: {
  programs.npm = {
    enable = true;
    # If you want a specific version
    # package = pkgs.nodePackages_13_x.npm;

    # Configure npm if needed:
    # npmrc = ''
    #   prefix = ''${HOME}/.npm
    #   https-proxy=proxy.example.com
    #   init-license=MIT
    #   init-author-url=https://www.npmjs.com/
    #   color=true
    # '';
  };

  environment.systemPackages = [
    pkgs.nodejs
    # or a specific version:
    # pkgs.nodejs_22

    # Deno:
    pkgs.deno
    
    # Bun:
    pkgs.bun

    # Other tools:
    # pkgs.biome
    # pkgs.nodePackages.prettier

    # Editor:
    # See <https://codchi.dev/docs/config/editor.html> for more info
    pkgs.vscodium
    # Or, if you have problems with manually installed plugins:
    # pkgs.vscodium-fhs
  ];
}
```
