<img align="right" alt="codchi logo" src="static/logo.webp" width="120px">

# [codchi](https://codchi.dev) - Declarative, Reproducible, Cross Platform Development Environments as Code for Windows, Linux[^1] and MacOS[^1]

[![build](https://github.com/aformatik/codchi/actions/workflows/windows.yml/badge.svg)](https://github.com/aformatik/codchi/actions/workflows/windows.yml)


## 1. Getting started

### 1.1. Installation

#### Windows 10/11

1. Make sure that WSL2 is installed and up to date. For install instructions visit the [official site](https://learn.microsoft.com/en-us/windows/wsl/install). You need WSL2 1.2.5.0 or newer. Check yours' with `wsl.exe --version`. Update with `wsl.exe --update --web-download`.
2. Import [Cert.crt](https://github.com/aformatik/codchi/releases/latest/download/codchi.crt) to `LocalMachine/Trusted People` (see [Arch-WSL docs](https://wsldl-pg.github.io/ArchW-docs/Install-Certificate/)). Requires *Admin Privileges*.
3. Install [codchi.appinstaller](https://github.com/aformatik/codchi/releases/latest/download/codchi.AppInstaller)
   (if the app installer fails, install the [latest codchi.msix](https://github.com/aformatik/codchi/releases/latest/download/codchi.msix) manually.)


#### Linux
Coming soon.

#### MacOS
Coming soon.

### 1.2. Installing your first code machine

After you've installed the codchi app, its time to install your first code
machine. Take a look at some pre-configured machines:

- Base image (no programs): [examples/base/configuration.nix](examples/base/configuration.nix)
- OpenJDK 17 and 20 + IntelliJ CE: [examples/java/configuration.nix](examples/java/configuration.nix)
- NodeJS and NPM: [examples/nodejs/configuration.nix](examples/nodejs/configuration.nix)

Each of these is exposed as ready to use code machine. Run 
`codchi install <name> github:aformatik/codchi#<machine>` 
where `<name>` is a name of your choice and `<machine>` is one of `base`,
`java` and `nodejs`.

Another way to create a code machine is to extend the existing images. To do
this, initialize an empty folder with 
`nix flake init -t github:aformatik/codchi#<machine>` or just copy
`configuration.nix` and `flake.nix` from `./examples/<machine>`. You can find
available options in the [NixOS Options
Search](https://search.nixos.org/options), the [NixOS
Wiki](https://nixos.wiki/) or install packages from the [NixOS Package
Search](https://search.nixos.org/packages).
To install a custom code machine, upload it to a public Github repository and run
`codchi install <name> github:<user>/<repo>#default`. 

### 1.3. Using codchi

```
Usage: codchi COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  status                   Show codchi status
  start                    Start codchi controller
  install                  Install a code machine
  uninstall                Uninstall a code machine
  run                      Run command in a code machine
  update                   Update a code machine
```

# License

Codchi is licensed under a mixed open source license. The tool codchi (files in
folder [cli](./cli)) are released under the [MPL-2.0](./cli/LICENSE).
Everything else is licensed under [MIT](./LICENSE).

[^1]: Coming soon
