# Roadmap

- Docs
    - [x] Usage
    - [x] Option Reference
    - Examples (Top 10 Languages?, Starter Templates?)
    - Demo Video
    - [X] Configuring Codchi
        - ~~[x] Config / State dirs~~
    - NixOS options
        - [x] timezone
        - [ ] hosts / resolv.conf

    

- MVP Features
    - [X] Shortcuts => Windows test
    - [x] Local NixOS Config
    - [X] Secrets (File / Env?)
    - LXD:
        - [X] 
            - [X] X11, 
            - Pulse, Wayland
        - [/] Switch to native nix
        - [x] Nix package
    - WSL
       - [X] Host integration via `explorer.exe 'FILE'` 
    - [X] Graphical Launcher
        - codchiw.exe?
        - [ ] Logging / Error message via popup?
    - gc
        - [X] WSL: sparse vhdx => Docs
        - [/] keep user-roots by direnv / nix build
    - [/] simple tray icon
    - [X] WSL
        - [x] switch vcxsrv / wslg
        - [X] vcxsrv tray icon switch

- Release v0.2.0
    - Packages
        - [X] MSIX: 🔒 Azure Code Signing Release
        - [X] Linux: Nix
        - [X] NixOS
    - [/] Internal Testing & Release

- Stability
    - WSL
        - what happens if store stops and machines runs?
        - how to handle network reset? => VcXsrv

- License

- Future
    - Custom certs auto adding
        - => how to handle inside code machines?
    - Announcement Post (Discourse, HN?, Product Hunt?)
    - [/] GPU
    - Better Logging
        - [X] nix log integration (=> Progress Bar)
    - WSL:
        Terminal integration (#14)
    - Devenv / nix develop / home-manager machines?
        - => inside special code machine (only docs? / special command?)
    - Automatic git fetch of project repo?
    - Docs
        - [/] Internal Docs / Contrib
        - Uninstalling Codchi, Migrating away
