# Roadmap

- Bugs
    - [ ] shortcuts / terminal fragments win
        - update path on every update => autostart codchi tray => run "migration"

- Future
    - Announcement Post (Discourse, Reddit)

- Features
    - [ ] `codchi status` on windows is slow
        => move wsl status checking / store container starting to scheduled / time based task
    - [ ] `codchi recover` => fs tar export
    - [ ] `codchi debug` => machine / store debug shell
    - [ ] `codchi export {vmware,virtualbox,qemu,hyperv}` 
        => export machine to VM image
        => backup / migrating away
    - [ ] (`codchi state export`) => creating a base fs for files like IJ / eclipse config
    - [ ] (codchi daemon inside each code machine)
        - current bash solution too fragile
        => Does init (secret / config fetching, log reporting)
    - [ ] (Custom certs auto adding)
        - => how to handle inside code machines?
    - [ ] (Linux Wayland support)
    - [ ] Shortcuts
        - Config option
        - tray icon
        - ip kopieren

- open questions:
    - WSL
        - what happens if store stops and machines runs?
        - how to handle network reset (VcXsrv)? 
            => wslg as default?
