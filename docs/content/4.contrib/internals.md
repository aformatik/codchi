# Internals

This page describes the internals of Codchi. For an overview of what Codchi is read [What is Codchi?](../1.introduction/0.what-is-codchi.md)

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

This works because Codchi provides the hardware configuration, the drivers (i.e. WSL, LXD and so on), as well as host integration such as GUI and sound support, file system sharing between code machine and host, and much more.

The goal is to provide an easy to use NixOS driver for different platforms (currently Windows & Linux), where everything hardware related just works.



## Capabilities

These are the capabilities provided by Codchi. Every new driver must at least fully implement the "must" items.



| Category         | Capability                                                                                                                                                                                                                              | Must / Should  | Windows + WSL  | Linux + LXD                                              |
| -------------    | --------------                                                                                                                                                                                                                          | -------------- | -------------- | --------------                                           |
| Architecture     | Each code machine on a host uses a shared store accessible via `nix-daemon`                                                                                                                                                             | Must           | ✅             | ✅                                                       |
| Architecture     | The file system of each code machine on a host can be accessed by the store for local NixOS configuration                                                                                                                               | Must           | ✅             | ✅                                                       |
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

A Codchi driver has three parts:

1. **The host driver:** This is a part of the Codchi executable that runs natively on the host and interfaces between generic code, the operating system and the virtualization software. Codchi's configuration and machine data files also reside on the host and must be shared with `codchistore` and each code machine. Repository: `codchi`, `codchiw`
2. **The Store Driver:** The `codchistore` is a minimal container in which the shared store resides and machines are built. This is where `nix-daemon` runs. `codchistore` must share the `/nix` directory with each code machine via a bind mount. Repository: `nix/container/store`
3. **The Machine Driver:** The Codchi machine driver is just a NixOS module that acts as a bridge between a hardware agnostic NixOS module and the host's virtualization software (Windows: WSL, Linux: LXD). It must make the file system of the code machine available to `codchistore` in order to allow local NixOS configuration. The WSL part is heavily inspired by [NixOS-WSL](https://github.com/nix-community/NixOS-WSL). Repository: `nix/nixos/driver`

::alert{type="info"}

**Note**

<br>
<br>

One thing to note is that since a code machine doesn't have its own store, it can only run if `codchistore` is also running, and should therefore only be started by Codchi.

::


### WSL

- **SystemD:** WSL natively supports SystemD.
- **GUI:** Windows natively supports GUI applications within WSL with WSLg using a mixture of Wayland and RDP. Unfortunately, because of the latter, this doesn't really provide a native experience for Linux GUI applications. Therefore, by default, [VcXsrv](https://github.com/marchaesen/vcxsrv), a native Windows X server, is included and enabled in codchi. It can be disabled in [configuration](../1.introduction/3.config.md).
- **Sound:** WSLg provides a PulseAudio server by default which works well enough.
- **File sharing:** Files are shared accross WSL instances via bind mounts in `/mnt/wsl/codchi`.
- **Environment variables, secrets:** Shared via `$env:WSLENV` and a file which is written by the host driver to `\\wsl$\codchi-*\...`.
- **Shortcuts:** Although WSL creates shortcuts itself, they only work if the code machine is already running. Therefore they are disabled and rather created by codchi itself so we can ensure that `codchistore` is running.
- **Terminal integration:** <https://learn.microsoft.com/en-us/windows/terminal/json-fragment-extensions>
- **GPU:** WSL provides access to the GPU installed on the host via dynamic libraries and executables. Since a code machine is a NixOS system some LD hacks are neccessary.
- **Host Integration:** The default applications inside each code machine are configured such that files or webpages open in the default Windows browser


::alert{type="warning"}

**Issues with WSL**

<br>
<br>

In the Windows tradition, WSL can sometimes fail for no apparent reason. For example, a Codchi command might fail, only to work when the same command is issued again. More work is needed in this area to catch all WSL hickups.

::

### LXD

- **SystemD:** LXD doesn't care about the init system.
- **GUI:** X11 and wayland sockets are shared via bind mounts. Also `.Xauthority` is copied into each machine.
- **Sound:** PulseAudio / PipeWire sockets are shared.
- **File sharing:** All files reside on the host and are bind-mounted into `codchistore` and each machine.
- **Environment variables, secrets:** Shared via `lxc exec --env` and a file which is copied into the machine.
- **Shortcuts:** Desktop entries and menus are generated in the corresponding XDG directories.

### VM Export

::alert{type="info"}

**Planned**

<br>
<br>

Integration with [nixos-generators](https://github.com/nix-community/nixos-generators) is planned to allow exporting a code machine to any / some VM format(s).

::

## Codchi Modules

Codchi also provides its own set of [NixOS Options](../3.config/99.Codchi specific NixOS Options.md) in order to provide
- Codchi-specific options (`codchi.secrets`, `codchi.welcome`)
- options to easy the creation of a development environment (`codchi.enableRecommendedConfig`, `codchi.docker`).

Repository: `nix/nixos/modules`

## Guidelines

These are the general design and contribution guidelines for Codchi. The goal is to make Codchi / NixOS easy to use and enjoyable while reducing the maintanence burden.

- **Reliable & Reproducible:** The native Codchi executable, `codchistore` and Codchi's NixOS configuration must be reliable. Quality of life features such as secrets must not affect the reproducibility of NixOS accross different hosts.
- **Fast**
- **Easy to Use:** 
    - Features should be kept to a minimum.
    - Codchi should feel familiar (`git clone`, `docker exec`).
- **Building on Standards (NixOS modules):** No custom configuration format / vendor lock-in.
- **Simplicity:** 
    - Fewer but better features.
    - Simple design without a central Codchi-daemon.

