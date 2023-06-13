# Installation

## Windows 10/11

1. Import [Cert.crt](https://gitlab.com/afojhr/devenv-packaging/-/raw/master/Cert.crt?inline=false) to `LocalMachine/Trusted People` (see [Arch-WSL docs](https://wsldl-pg.github.io/ArchW-docs/Install-Certificate/)). Requires *Admin Privileges*.
2. Install [devenv.appinstaller](https://gitlab.com/api/v4/projects/45140827/packages/generic/devenv/latest/devenv.AppInstaller)
   (if the app installer fails, install the [latest devenv.msix](https://gitlab.com/api/v4/projects/45140827/packages/generic/devenv/latest/devenv.msix) manually.)
