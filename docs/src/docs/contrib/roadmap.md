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
    - WSL
       - [X] Host integration via `explorer.exe 'FILE'` 
    - [X] Graphical Launcher
        - codchiw.exe?
        - [x] Logging / Error message via popup?
    - gc
        - [/] keep user-roots by direnv / nix build
    - [/] simple tray icon
        - Per machine status
        - doctor status
        - config.toml
    - [ ] Codchi doctor (periodical check)

- Release v0.2.0
    - Packages
        - [X] Linux: Nix
        - [X] NixOS
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
