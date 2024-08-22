use super::*;
use crate::{
    cli::DEBUG,
    consts::{self, ToPath},
    util::PathExt,
};
use anyhow::{anyhow, Context, Result};
use freedesktop_entry_parser::{self as _, parse_entry};
use itertools::Itertools;
use std::{
    collections::HashMap,
    env,
    fs::{self, File},
    path::PathBuf,
    process::{Command, Stdio},
    result,
};
use sysinfo::System;

pub trait Host: Sized {
    fn write_shortcuts<'a, I>(machine_name: &str, apps: I) -> Result<()>
    where
        I: Iterator<Item = &'a DesktopEntry>;

    fn delete_shortcuts(name: &str) -> Result<()>;

    fn write_machine_shortcuts(machine: &Machine) -> Result<()> {
        let nix_path = Driver::store().cmd().realpath(
            &consts::store::DIR_CONFIG
                .join_machine(&machine.config.name)
                .join_str("system/sw/share/codchi"),
        )?;
        let rc_path = Driver::store().store_path_to_host(&nix_path)?;

        let icons = fs::read_dir(rc_path.join("icons"))?
            .map_ok(|file| {
                let path = file.path();
                (
                    path.file_stem()
                        .or(path.file_name())
                        .unwrap_or_else(|| panic!("Icon without filename: '{path:?}'?!"))
                        .to_string_lossy()
                        .to_string(),
                    path,
                )
            })
            .collect::<result::Result<HashMap<String, PathBuf>, std::io::Error>>()
            .with_context(|| {
                format!(
                    "Failed to read icons from '{}/icons'.",
                    rc_path.to_string_lossy()
                )
            })?;

        let mut desktop_entries = Vec::new();
        for file in fs::read_dir(rc_path.join("applications"))? {
            let file = file?.path();
            if !file.extension().is_some_and(|ext| ext == "desktop") {
                continue;
            }
            let entry = parse_entry(&file).with_context(|| {
                format!("Failed to parse desktop file {}.", file.to_string_lossy())
            })?;
            let get_entry = |name: &str| {
                entry.section("Desktop Entry").attr(name).ok_or(anyhow!(
                    "Missing entry '{name}' in desktop entry from '{}'.",
                    file.to_string_lossy()
                ))
            };
            let name = get_entry("Name")?;
            let exec = get_entry("Exec")
                .or(get_entry("TryExec"))?
                .to_string()
                .split_whitespace()
                // remove XDG field codes
                .filter(|arg| !(arg.len() == 2 && arg.starts_with('%')))
                .join(" ");
            let is_terminal = get_entry("Terminal").is_ok_and(|term| term == "true");

            let app_name = file
                .file_stem()
                .or(file.file_name())
                .unwrap()
                .to_string_lossy()
                .to_string();
            desktop_entries.push(DesktopEntry {
                app_name: app_name.clone(),
                name: format!("codchi-{} {}", machine.config.name, name),
                exec,
                icon: icons.get(&app_name).cloned(),
                is_terminal,
            });
        }

        Self::write_shortcuts(&machine.config.name, desktop_entries.iter())
    }

    fn open_terminal(&self, cmd: &[&str]) -> Result<()>;

    fn start_tray(&self, kill_running: bool) -> Result<()> {
        let exe = env::current_exe()?;

        if let Some(p) = System::new_all()
            .processes_by_name("codchi".as_ref())
            .find(|p| {
                p.exe().is_some_and(|p| p == exe) && p.cmd().get(1).is_some_and(|arg| arg == "tray")
            })
        {
            if kill_running {
                log::debug!("Killing running tray");
                p.kill();
            } else {
                log::debug!("Tray is already running");
                return Ok(());
            }
        }

        let mut cmd = Command::new(&exe);
        cmd.arg("tray");
        if *DEBUG {
            cmd.arg("--verbose");
        }
        cmd.stdin(Stdio::null());
        let log_file = File::create(consts::host::DIR_RUNTIME.get_or_create()?.join("tray.log"))?;
        cmd.stdout(Stdio::from(log_file.try_clone()?));
        cmd.stderr(Stdio::from(log_file));

        #[cfg(target_os = "windows")]
        {
            use std::os::windows::process::CommandExt;
            use windows::Win32::System::Threading::*;
            cmd.creation_flags(CREATE_NEW_PROCESS_GROUP.0 | CREATE_NO_WINDOW.0);
        }
        // #[cfg(target_family = "unix")]
        // {
        // cmd.creation_flags();
        // }
        cmd.spawn()?;
        anyhow::Ok(())
    }

    #[cfg(target_os = "windows")]
    fn start_vcxsrv(&self, kill_running: bool) -> Result<()>;
}

#[derive(Clone, Debug)]
pub struct DesktopEntry {
    pub app_name: String,
    pub name: String,
    pub exec: String,
    pub icon: Option<PathBuf>,
    pub is_terminal: bool,
}
