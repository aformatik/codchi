# Codchi Drivers

## Responsibilities

### Nix

- A Running nix-daemon (`/bin/ctrl-serve`)
    - [X] WSL
    - [ ] LXD

- Mounts per code machine
    - Needed directories
        - /nix/store ro
        - /nix/var/nix/daemon-socket rw
            - nix-daemon does builds, gc, ...
        - /nix/var/nix/profiles/per-instance/<NAME> -> /nix/var/nix/profiles rw
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
    - [ ] All: `nix run github:aformatik/codchi#ctrl-install
    - [ ] WSL: `wsl --import`
    - [ ] LXD: `lxc image import && lxc init && lxc config {devices, security.nesting} && lxd image delete`

- Reverse instance mounts
    - [ ] WSL
        - in instance pre systemd start OR
        - on `codchi rebuild` with `/bin/mount` (to avoid systemd start)
        - `mount <instance dir> /mnt/wsl/codchi-instances/$NAME`
    - [ ] LXD full: root device as bind mount from controller accessible dir
