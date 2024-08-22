use crate::platform::{CommandExt, DesktopEntry, Host};
use crate::{consts, util::PathExt};
use anyhow::{bail, Result};
use indoc::formatdoc;
use std::process::Command;
use std::{env, fs};

pub struct HostImpl;
impl HostImpl {}

impl Host for HostImpl {
    fn write_shortcuts<'a, I>(machine_name: &str, apps: I) -> Result<()>
    where
        I: Iterator<Item = &'a DesktopEntry>,
    {
        let folder = consts::host::BASE_DIR
            .data_dir()
            .join("applications")
            .join("codchi")
            .join(machine_name)
            .cleanup_and_get()?;

        let codchi_exe = env::current_exe()?.to_string_lossy().to_string();

        for DesktopEntry {
            app_name,
            name,
            exec,
            icon,
            is_terminal,
        } in apps
        {
            let icon = icon
                .as_ref()
                .map(|path| format!("Icon={}", path.to_string_lossy()))
                .unwrap_or_default();
            let exec = format!("{codchi_exe} exec {machine_name} {exec}");
            fs::write(
                folder.join(format!("{app_name}.desktop")),
                formatdoc! {"
                    [Desktop Entry]
                    Version=1.0
                    Type=Application
                    Name={name}
                    Exec={exec}
                    Terminal={is_terminal}
                    Categories=X-codchi-{machine_name}
                    {icon}
                "},
            )?
        }

        let menu_dir = consts::host::BASE_DIR
            .config_dir()
            .join("menus/applications-merged");
        let menu_dir = menu_dir.get_or_create()?;

        fs::write(
            menu_dir.join(format!("codchi-{machine_name}.menu")),
            formatdoc! {r#"
                <!DOCTYPE Menu PUBLIC "-//freedesktop//DTD Menu 1.0//EN"
                "http://www.freedesktop.org/standards/menu-spec/menu-1.0.dtd">
                <Menu>
                  <Name>Applications</Name>
                  <Menu>
                    <Name>Codchi</Name>
                    <Directory>codchi.directory</Directory>
                    <Menu>
                      <Name>{machine_name}</Name>
                      <Include>
                        <Category>X-codchi-{machine_name}</Category>
                      </Include>
                    <Menu>
                  </Menu>
                </Menu>
            "#},
        )?;

        // let dirs_dir = consts::host::BASE_DIR
        //     .data_dir()
        //     .join("desktop-directories");
        // let dirs_dir = dirs_dir.get_or_create()?;
        // fs::write(
        //     dirs_dir.join("codchi.directory"),
        //     formatdoc! {r#"
        //         [Desktop Entry]
        //         Version=1.0
        //         Type=Directory
        //         Name=Codchi
        //         Icon=TODO
        //     "#},
        // )?;

        Ok(())
    }

    fn delete_shortcuts(machine_name: &str) -> Result<()> {
        consts::host::BASE_DIR
            .data_dir()
            .join("applications")
            .join("codchi")
            .join(machine_name)
            .remove();
        consts::host::BASE_DIR
            .config_dir()
            .join("menus/applications-merged")
            .join(format!("codchi-{machine_name}.menu"))
            .remove();
        Ok(())
    }

    fn open_terminal(&self, cmd: &[&str]) -> Result<()> {
        let mut terms: Vec<(String, Vec<String>)> = [
            "x-terminal-emulator",
            "mate-terminal",
            "gnome-terminal",
            "terminator",
            "xfce4-terminal",
            "urxvt",
            "rxvt",
            "termit",
            "Eterm",
            "aterm",
            "uxterm",
            "xterm",
            "roxterm",
            "termite",
            "lxterminal",
            "terminology",
            "st",
            "qterminal",
            "lilyterm",
            "tilix",
            "terminix",
            "konsole",
            "kitty",
            "guake",
            "tilda",
            "alacritty",
            "hyper",
            "wezterm",
            "rio",
        ]
        .into_iter()
        .map(|cmd| (cmd.to_string(), Vec::new()))
        .collect();
        if let Ok(term) = env::var("TERMINAL") {
            // handle multiple arg $TERMINAL
            let mut words = term.split(' ');
            if let Some(cmd) = words.next() {
                let args = words.map(|s| s.to_string()).collect();
                terms.insert(0, (cmd.to_string(), args))
            }
        }

        for (term, args) in terms {
            if let Ok(path) = which::which(term) {
                dbg!(Command::new(path).args(args).arg("-e").args(cmd)).wait_ok()?;
                return Ok(());
            }
        }
        bail!("Could not find a terminal.");
    }
}
