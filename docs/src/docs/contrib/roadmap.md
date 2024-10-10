# Roadmap

- Docs
    - [ ] Demo Video
    - NixOS options
        - [ ] hosts / resolv.conf
    - auto update
        - update available notification / working auto update
    

- MVP Features
    - [ ] Codchi doctor (periodical check)

- [ ] License

- Future
    - Custom certs auto adding
        - => how to handle inside code machines?
    - Announcement Post (Discourse, HN?, Product Hunt?)
    - [/] GPU
    - WSL:
        Terminal integration (#14)
    - Docs
        - [ ] Internal Docs / Contrib
        - [ ] Uninstalling Codchi, Migrating away

- Bugs
    - update required with local repos => invalidate nix cache flag?
    - (testing: dont prompt)

- open questions:
    - WSL
        - what happens if store stops and machines runs?
        - how to handle network reset (VcXsrv)? 
            => wslg as default?
    - LXD
        - Wayland support / xwayland?
