# Configuring Codchi

Codchi itself can be configured by editing:

- **Windows:** `%APPDATA%\codchi\config.toml` (`%APPDATA%` is most likely `C:\Users\NAME\AppData\Roaming`)
- **Linux:** `$XDG_CONFIG_HOME/codchi/config.toml` (`$XDG_CONFIG_HOME` is most likely `~/.config`)

## `data_dir`

The path where codchi stores files from code machines.

Default:
- **Windows:** `%LOCALAPPDATA%\codchi` (`%LOCALAPPDATA%` is most likely `C:\Users\NAME\AppData\Local`)
- **Linux:** `$XDG_DATA_HOME/codchi` (`$XDG_DATA_HOME` is most likely `~/.local/share`)


## `[vcxsrv]` (Windows only)

### `vcxsrv.enable`

Whether to use [VcXsrv](https://github.com/marchaesen/vcxsrv), a X-Server for Windows, instead of Windows' own RDP solution.
VcXsrv mostly has a better user experience and better performance but still has some bugs.

Default: `true`

### `vcxsrv.tray_icon`

Whether to show VcXsrv's tray icon.

Default: `false`

<!-- TODO 

Linux:
- use native nix


-->

