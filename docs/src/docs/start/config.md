# Configuring Codchi

Codchi itself can also be configured (mostly platform specific).

<!-- ## Directories -->

<!-- Codchi adheres to platform standards when storing data. Each path can be -->
<!-- overriden with environment variables: -->

<!-- | What?                                    | Windows                                | Linux                                                 | Overridden by         | -->
<!-- | ----------------                         | ---------------                        | ---------------                                       | ---------------       | -->
<!-- | Configuration files, machine definitions | `%USERPROFILE%/AppData/Roaming/codchi` | `$XDG_CONFIG_HOME/codchi` or `~/.config/codchi`       | `$CODCHI_CONFIG_DIR`  | -->
<!-- | Machine data and files                   | `%USERPROFILE%/AppData/Local/codchi`   | `$XDG_DATA_HOME/codchi` or `~/.local/share/codchi`    | `$CODCHI_DATA_DIR`    | -->
<!-- | Temporary files                          | `%TEMP%/codchi`                        | `$XDG_RUNTIME_DIR/codchi` or `/tmp/codchi`            | `$CODCHI_RUNTIME_DIR` | -->
<!-- | Nix store                                | Stored inside WSL                      | `$XDG_CACHE_HOME/codchi/nix` or `~/.cache/codchi/nix` | `$CODCHI_NIX_DIR`     | -->
 

<!-- TODO 

- toggle tray icon

Linux:
- use native nix

WSL:
- toggle WSLg & VcXsrv + Pulse
- VcXsrv: toggle tray icon

-->

