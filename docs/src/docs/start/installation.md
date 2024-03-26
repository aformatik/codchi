# Installation

## Windows 10 and Windows 11

### Prerequisites

On Windows, Codchi uses the [Windows Subsystem for Linux](https://learn.microsoft.com/en-us/windows/wsl), which is available for free from Microsoft.

::: warning
Installing WSL enables Microsoft's Hyper-V Hypervisor, which might cause issues with older virtualisation technologies like VMware. Don't worry, you can [disable or re-enable Hyper-V](https://learn.microsoft.com/en-us/troubleshoot/windows-client/application-management/virtualization-apps-not-work-with-hyper-v) at any time.
:::

Make sure that WSL2 is installed and up to date. In a terminal, `wsl.exe
--version` should output something like the following, where WSL-Version
should be `2.0.14` or greater:
```ps1
WSL-Version: 2.0.14.0
Kernelversion: 5.15.133.1-1
WSLg-Version: 1.0.59
MSRDC-Version: 1.2.4677
Direct3D-Version: 1.611.1-81528511
DXCore-Version: 10.0.25131.1002-220531-1700.rs-onecore-base2-hyp
Windows-Version: 10.0.22631.3296
```
If WSL is not installed:
```ps1
wsl.exe --install --no-distribution --web-download
```
If WSL is installed but too old:
```ps1
wsl.exe --update --web-download
```
Now recheck `wsl.exe --version`.

### Install Codchi

Download, open and install the latest
[codchi.msix](https://github.com/aformatik/codchi/releases/latest). `MSIX` or
`AppX` is the packaging format for Windows Apps and should work out of the box.
Windows even handles subsequent updates of Codchi automatically, but you can
download and install a newer codchi.msix at any time.

## Linux

### Prerequisites

On Linux Codchi uses [LXD](https://canonical.com/lxd) for virtualisation.
Follow the official [installation instructions](https://canonical.com/lxd/install)
until `lxd init`.

### Install Codchi

Currently, Codchi is only packaged with [Nix](https://nixos.org) on Linux. 

Install Nix (uses the [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer)):
```bash
# with SystemD:
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
# without SystemD:
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --init none
```
Now install codchi:
```bash
nix profile install github:aformatik/codchi
```

## NixOS

### Prerequisites

Flakes need to be activated.

Install LXD:
```nix
virtualisation.lxd.enable = true;
virtualisation.lxc.lxcfs.enable = true;
virtualisation.lxd.recommendedSysctlSettings = true;

# If you use a local DNS server. See https://github.com/NixOS/nixpkgs/issues/263359
firewall.interfaces.lxdbr0.allowedTCPPorts = [ 53 ];
firewall.interfaces.lxdbr0.allowedUDPPorts = [ 53 67 ];

# Allow your user to use LXD
users.users.<your user>.extraGroups = [ "lxd" ];
```
You might also need to run `lxd init` manually.

### Install Codchi

In your flake.nix:
```nix
inputs.codchi.url = "github:aformatik/codchi";
inputs.codchi.nixpkgs.follows = "nixpkgs";

outputs = inputs@{ ... }: {
    # Pass inputs to your NixOS configuration (e.g. via extraArgs)
};
```
In your NixOS configuration:
```nix
{ inputs, ... }: {
  environment.systemPackages = [ inputs.codchi.packages.x86_64-linux.default ];
}
```

## MacOS

::: details
Currently not implemented. Possible Drivers are <https://orbstack.dev/>
(proprietary) and <https://lima-vm.io/> (open source, limited support for the
newer faster virtualisation on Macs)
:::
