# Internals

This page describes the internals of Codchi. For an overview of what Codchi is read [What is Codchi?](..//start/intro.md).

## Overview

Technically Codchi is just a "driver" for any NixOS module, such as:
```nix
{
}
```
That is just an empty NixOS module. To build this into a bootable machine without Codchi, you need to provide the hardware configuration (e.g. file system layout, boot loader, kernel, kernel module & options, desktop environment, ...). This can vary a lot from machine to machine, although virtual machines alleviate some of this.

With Codchi, however, the empty module above is a buildable machine configuration on Windows 10 & 11 and Linux:
```bash
codchi init empty-machine
codchi exec empty-machine -- uname -a
> Linux nixos 6.6.32 #1-NixOS SMP PREEMPT_DYNAMIC Sat May 25 14:22:56 UTC 2024 x86_64 GNU/Linux
```

This works because Codchi provides the hardware configuration, the drivers (i.e. WSL, LXD and so on), as well as host integration such as GUI and sound support, file system sharing between code machine and host, and much more:

![Codchi architecture diagram](/architecture.png)

The goal is to provide an easy to use NixOS driver for different platforms, where everything hardware related just works.



## Capabilities

These are the capabilities provided by Codchi. Every new driver must at least fully implement the "must" section.



| Category         | Capability                                                                                                                                                                                                                              | Must / Should  | Windows + WSL  | Linux + LXD                                              |
| -------------    | --------------                                                                                                                                                                                                                          | -------------- | -------------- | --------------                                           |
| Architecture     | Each code machine on a host uses a shared store accessible via `nix-daemon`                                                                                                                                                             | Must           | ✅             | ✅                                                       |
| Run full NixOS   | SystemD working properly                                                                                                                                                                                                                | Must           | ✅             | ✅                                                       |
| Run full NixOS   | Proper login shell, i.e. `codchi exec` creates a proper login shell with `$DISPLAY` set.  This is essential for the environment variables to be set correctly and for everything to start up correctly, including XDG autostart entries | Must           | ✅             | ❔                                                       |
| Host Integration | GUI & Sound: `codchi exec <MACHINE_NAME> -- nix run nixpkgs#xorg.xeyes` must open xeyes on the host's desktop. X11 and wayland apps (with sound) must work.                                                                             | Must           | ✅             | ❔                                                       |
| Host Integration | Tray icon: Codchi must be controllable through a tray icon on the host.                                                                                                                                                                 | Must           | ✅             | ✅                                                       |
| Host Integration | Shortcuts to each program in each code machine must be visible in the host's start menu, such that GUI and terminal applications can be started with a single click.                                                                    | Must           | ✅             | ✅                                                       |
| Host Integration | Terminal: Codchi must be integrated with the host's terminal, such that a terminal with a shell into each code machine can be opened from the codchi tray.                                                                              | Must           | ✅             | ✅                                                       |
| Host Integration | Secrets: A code machine can define secrets, which the codchi driver must store (preferably encrypted) on the host and provide it on code machine start.                                                                                 | Must           | ✅             | ✅                                                       |
| Host Integration | File system sharing: The file system of each code machine should be accessible from the host, e.g. as with WSL.                                                                                                                         | Should         | ✅             | 🚧 (Generally works through LXD, but UX can be improved) |
| Host Integration | GPU: If the host has a GPU driver installed, it should be usable from within a code machine.                                                                                                                                            | Should         | ❔             | ❔                                                       |
| Host Integration | USB: USBs plugged into the host should be accessible from within a code machine.                                                                                                                                                        | Should         | 📝             | 📝                                                       |
| Host Integration | SSL certificates: The hosts trust chain should be used inside a code machine, so that self signed certificates work (often used in enterprises).                                                                                        | Should         | 📝             | 📝                                                       |

**Legend**
- ✅ = Fully implemented and tested
- ❔ = Implemented, but untestet / buggy
- 🚧 = WIP
- 📝 = Planned
- ❌ = Not implemented

## Drivers


### WSL

### LXD

### VM Export

## Guidelines

- reliable & reproducible
- fast
- easy to use
- Building on "standards" (NixOS modules)


<!--
Codchi = driver
    - 1. OS => WSL, LXD - Linux
        - 
        - Nix store sharing
    - 2. NixOS

Code / design guidelines
    - Easy to use
    - Simplicity
        - no daemon neccessary
    - Performant
    - plain NixOS
    - No vendor lockin



- Testing

## Codchi Drivers

### Responsibilities

#### Nix

- A Running nix-daemon (`/bin/ctrl-serve`)
    - [X] WSL
    - [ ] LXD

- Mounts per code machine
    - Needed directories
        - /nix/store ro
        - /nix/var/nix/daemon-socket rw
            - nix-daemon does builds, gc, ...
        - /nix/var/nix/profiles/per-instance/&lt;NAME> -> /nix/var/nix/profiles rw
            - needed for /run/current-system & gc
        - /nix/var/nix/db ro
            - needed for gc
        - ln /nix/var/nix/profiles /nix/var/nix/gcroots/
            - needed for gc
    - [X] WSL
        - [X] codchi-controller: mount /nix in /mnt/wsl/nix
        - [X] code machine: do all mounts from /mnt/wsl/nix pre systemd
    - [X] LXD: lxd devices

- Code machine installation
    - Controller must install `config.system.build.toplevel` per code machine as a profile: `nix-env -p "/nix/var/nix/profiles/per-instance/$NAME/system" --set $(cat $DRV/system-store-path)`. This adds gc roots and could allow rollbacks in the future
    - [ ] All: `nix run github:aformatik/codchi#ctrl-install`
    - [ ] WSL: `wsl --import`
    - [ ] LXD: `lxc image import && lxc init && lxc config {devices, security.nesting} && lxd image delete`

- Reverse instance mounts
    - [ ] WSL
        - in instance pre systemd start OR
        - on `codchi rebuild` with `/bin/mount` (to avoid systemd start)
        - `mount <instance dir> /mnt/wsl/codchi-instances/$NAME`
    - [ ] LXD full: root device as bind mount from controller accessible dir


## Codchifile

Idea: A single source of truth for a codchi module which exist in a repository.
This allows the `codchi add` command to only take the url to the repository and
figure everything out from there.

### Implementation

- Nix Flake with nixosModules.NAME / codchiModules.NAME
    - Plain Old NixOS Module inside flake.nix
    - Pros:
        - Compatible with Nix(OS)
        - Flake: Required for pinning nixpkgs
    - Cons: 
        - Tooling not obvious from filename: "flake" != "codchi"
        - Requires manual updating of flake.lock
            => Maybe add `codchi lock update MACHINE MODULE`, autocommit with saved credentials
- Goals:
    - Dont invent new file format
        => Move Codchi config into NixOS modules (e.g. secrets, capabilities)
    - provide `codchi.addNixpkgs`, `codchi.injectInputs` as special arg to allow extending
        => User may provide custom flake inputs directly inside his
           flake.nix which doesn't need additional magic.

### `codchi init NAME MODULE_URL`

- Alternative: `codchi init --empty`

### `codchi add NAME MODULE_URL`


## Code Machines

### Concept

A code machines is an instance or container inside codchi on a particular
computer. They consist of zero to `n` codchi modules (plain NixOS modules).
Also there is the hidden, internal codchi module itself which configures the
driver (LXD, WSL, ...). In previous version there were also some non
reproducible option (like the local name of the code machine) which are set
dynamically now (TBD).

### Intended Usage

1. The development environment for a software project is defined in a codchi
   module inside the project repository. Optionally nixpkgs is locked to a
   fixed commit / revision.
2. Each developer creates a code machine with this module on his local machine.
3. Optionally, each developer can include a personal module with for example
   his git config / editor setup. This can be shared between different
   projects.

## Nixpkgs

Every code machine needs a particular version of nixpkgs. There are two
possibilities to choose from:

1. A code machine is just a NixOS with some preconfigured, platform specific
   NixOS options and therefore needs nixpkgs anyway. The version of the builtin
   nixpkgs is also needed by the codchi controller and therefore already
   present when codchi is installed, so reusing this nixpkgs decreases
   duplication. But there is one catch: When using the local nixpkgs, exact
   reproducibility isn't guaranteed anymore because even if multiple persons
   use the exactly same codchi modules, their codchi version might differ.
2. Pin nixpkgs of a code machine to that of a codchi module. This guarantees
   exact reproducibility when using the same codchi module revision across
   different machines but increases duplication and installation time.

### Secrets

There are some things which don't belong into a NixOS Module since they would
land in the world readable store.

#### Host secrets (TBD)

These differ from user to user and are loaded dynamically. Also they're needed
by both the codchi controller and its code machines. TODO: Should they
atomatically be loaded in each code machine or only when requested via the
NixOS Module?

Examples are: 
    - CA Certificates
    - SSH Keys
    - Authentication Tokens


#### User defined secrets

User defined secrets are specific to a codchi module and can be requested by
setting `codchi.secrets.<name>`. When an user adds a module with a secret, he
will be prompted to add it interactively.

TODO: Possible Implemtations:
    - ENV
    - File

#### Capabilities (TBD)

Capabilities already exist in Nix: `nix.settings.system-features`. A codchi
module can request them by setting `codchi.requiredCapabilities`. When an user
adds a module, codchi will check if the neccessary system drivers are
installed.

Examples for capabilities:
    - GPU / Cuda
    - USB?
    - SSH Agent?
    - Sound / Video?

    -->
