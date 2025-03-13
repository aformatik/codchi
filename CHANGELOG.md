# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Documentation

- added changelog and contributing guidelines
    
## [v0.3.1.6] - 2025-03-10

### Features

- added `codchi visudo` & `codchi recover` (WSL only)
- added network namespaces on WSL
- increased the timeout of NDD for LXD
- updated to NixOS 24.11 and Nix 2.26
- added initial code for `codchi gui`

### Documentation

- removed lib.mdDoc

    
## [v0.3.1.5] - 2025-02-05

### Features

- added `codchi secrets`
- added support for VM export

### Bug Fixes

- added codchiw.exe to $PATH so that shortcuts survive MSIX updates

    
## [v0.3.1.4] - 2025-01-23

### Bug Fixes

- Disabled "This distribution is only meant to be started by codchi" message box (fixes #33)
- codchistore now restarts automatically if started incorrectly (for example by native IntelliJ)

    
## [v0.3.1.3] - 2025-01-22

### Features

- added `codchi duplicate`
- added `codchi store recover` (WSL only)
- added flag `--keep-remote` to `codchi clone`
- added init scripts for code machines
- **Linux: added initial support for incus**
- **WSL: added wsl_vpnkit**
- added codchi's binary cache to flake.nix (closes #31)

### Changes
- renamed `codchi debug-store` to `codchi store debug`
- renamed flag `-r` to `-t` (`--tag`)
- removed `codchi.welcome` in favor of `codchi.initScript`
- renamed option '--token' to '--auth'

### Bug Fixes

- fixed various NixOS module warnings and errors, enabled using nixos-24.11
- machines are now deleted if `init` / `clone` fails
- MSIX: dont auto update to lower versions

### Documentation

- Added WSL troubleshooting section
- Added Rust example
    
    
## [v0.3.0.0] - 2024-12-18

Removed VcXsrv
Beta release v0.3.0
Added demo video
env: VSCode with plugins
feat: `codchi tar`
feat: `codchi debug-store`

**Full Changelog**: https://github.com/aformatik/codchi/compare/v0.2.4.8...v0.3.0.0
