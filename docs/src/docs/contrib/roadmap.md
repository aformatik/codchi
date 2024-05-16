# Roadmap

- Docs
    - [x] Usage
    - [x] Option Reference
    - Examples (Top 10 Languages?, Starter Templates?)
    - Demo Video
    - [ ] Configuring Codchi
        - ~~[x] Config / State dirs~~

- MVP Features
    - [/] Shortcuts => Windows test
    - [x] Local NixOS Config
    - [ ] Secrets (File / Env?)
    - LXD:
        - [/] 
            - [X] X11, 
            - Pulse, Wayland
        - [/] Switch to native nix
        - [x] Nix package
    - [ ] Graphical Launcher
        - codchiw.exe?
        - Logging / Error message via popup?
    - [X] gc
        - WSL: shrink vhdx => Docs
    - [/] simple tray icon
    - [/] WSL
        - switch vcxsrv / wslg
        - (pulseaudio switch)
        - vcxsrv tray icon switch

- Release v0.2.0
    - Packages
        - [X] MSIX: 🔒 Azure Code Signing Release
        - [/] Linux: Nix
        - [/] NixOS
    - Internal Testing & Release

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
        - nix log integration (=> Progress Bar)
    - WSL:
        Terminal integration (#14)
    - Devenv / nix develop / home-manager machines?
        - => inside special code machine (only docs? / special command?)
    - Automatic git fetch of project repo?
    - Docs
        - [/] Internal Docs / Contrib
        - Uninstalling Codchi, Migrating away
