# Roadmap

- Docs
    - [ ] Demo Video
    - NixOS options
        - [ ] hosts / resolv.conf
    - auto update
        - update available notification / working auto update
    

- MVP Features
    - [ ] consolidate --help, man & docs
    - [ ] Codchi doctor (periodical check)
    - [ ] codchi clone

- [ ] License

- Future
    - Custom certs auto adding
        - => how to handle inside code machines?
    - Announcement Post (Discourse, HN?, Product Hunt?)
    - [/] GPU
    - WSL:
        Terminal integration (#14)
    - [ ] completions
        - Linux
        - Powershell
    - Docs
        - [ ] Internal Docs / Contrib
        - [ ] Uninstalling Codchi, Migrating away

- Bugs
    - update required with local repos => invalidate nix cache flag?
    - (testing: dont prompt)
    - LXD: error on first start

- open questions:
    - WSL
        - what happens if store stops and machines runs?
        - how to handle network reset (VcXsrv)? 
            => wslg as default?
    - ~Linux & auto roots (e.g. ./result, direnv): native nix~ 
        => garbage collection: how to add roots inside machines to global nix
    - LXD
        - Wayland support / xwayland?
