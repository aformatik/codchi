# Roadmap

- Docs
    - [x] Usage
    - [ ] Examples (Top 10 Languages?, Starter Templates?)
    - [ ] Demo Video
    - NixOS options
        - [x] timezone
        - [ ] hosts / resolv.conf

    

- MVP Features
    - [ ] Linux: native nix
    - [/] LXD:
        - [X] 
            - [X] X11, 
            - [ ] Pulse, Wayland
        - [x] Nix package
    - gc
        - [/] keep user-roots by direnv / nix build
    - [ ] Codchi doctor (periodical check)

- Release v0.2.0
    - [/] Internal Testing & Release

- Stability
    - WSL
        - what happens if store stops and machines runs?
        - how to handle network reset? => VcXsrv

- [ ] License

- Future
    - Custom certs auto adding
        - => how to handle inside code machines?
    - Announcement Post (Discourse, HN?, Product Hunt?)
    - [/] GPU
    - WSL:
        Terminal integration (#14)
    - Devenv / nix develop / home-manager machines?
        - => inside special code machine (only docs? / special command?)
    - Automatic git fetch of project repo?
    - Docs
        - [/] Internal Docs / Contrib
        - Uninstalling Codchi, Migrating away

- Bugs
    - update required with local repos
    - testing: dont prompt
    - LXD: error on first start
