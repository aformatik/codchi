use crate::{
    cli::DEBUG,
    config::CodchiConfig,
    consts,
    platform::{DesktopEntry, Host},
    util::PathExt,
};
use anyhow::{Context, Result};
use itertools::Itertools;
use known_folders::{get_known_folder_path, KnownFolder};
use mslnk::ShellLink;
use std::{env, fs, os::windows::process::CommandExt, process::Command};
use sysinfo::System;
use windows::Win32::System::Threading::{CREATE_NEW_PROCESS_GROUP, CREATE_NO_WINDOW};

pub struct HostImpl;
impl HostImpl {}

impl Host for HostImpl {
    fn write_shortcuts<'a, I>(machine_name: &str, apps: I) -> Result<()>
    where
        I: Iterator<Item = &'a DesktopEntry>,
    {
        let lnk_folder = get_known_folder_path(KnownFolder::Programs)
            .expect("FOLDERID_Programs missing")
            .join(format!("Codchi - {machine_name}"))
            .cleanup_and_get()?;
        let ico_folder = consts::host::DIR_DATA
            .join("icos")
            .join(machine_name)
            .cleanup_and_get()?;

        let codchi_exe = env::current_exe()?;

        for DesktopEntry {
            app_name,
            name,
            exec,
            icon,
            is_terminal,
        } in apps
        {
            let mut lnk = ShellLink::new(&codchi_exe)?;
            if let Some(ico_path) = icon {
                let target = ico_folder.join(format!("{app_name}.ico"));
                fs::copy(ico_path, &target)?;
                lnk.set_icon_location(Some(target.to_string_lossy().to_string()));
            }
            // lnk.set_name(Some(name.clone()));
            lnk.set_arguments(Some(format!(
                "--terminal {is_terminal} exec {machine_name} {exec}"
            )));
            lnk.set_working_dir(
                get_known_folder_path(KnownFolder::Profile)
                    .map(|p| p.to_string_lossy().to_string()),
            );
            lnk.create_lnk(lnk_folder.join(format!("{name}.lnk")))?;
        }
        Ok(())
    }

    fn delete_shortcuts(machine_name: &str) -> Result<()> {
        get_known_folder_path(KnownFolder::Programs)
            .expect("FOLDERID_Programs missing")
            .join(format!("Codchi - {machine_name}"))
            .remove();
        consts::host::DIR_DATA
            .join("icos")
            .join(machine_name)
            .remove();
        Ok(())
    }

    fn start_vcxsrv(&self, kill_running: bool) -> Result<()> {
        let cfg = CodchiConfig::get();
        if cfg.vcxsrv.enable {
            if let Some(p) = System::new_all()
                .processes_by_name("codchi_vcxsrv.exe".as_ref())
                .next()
            {
                if kill_running {
                    log::debug!("Killing VcXsrv...");
                    p.kill();
                } else {
                    log::debug!("VcXsrv is already running.");
                    return Ok(());
                }
            }
            let vcxsrv_exe = known_folders::get_known_folder_path(KnownFolder::ProgramFilesX64)
                .expect("Missing FOLDERID_ProgramFilesX64.")
                .join("VcXsrv")
                .join("codchi_vcxsrv.exe");
            vcxsrv_exe
                .assert_exists()
                .context("VcXsrv executable not found.")?;

            // dbg_duration("regedit for VcXsrv", || {
            //     let hkcu = RegKey::predef(HKEY_CURRENT_USER);
            //     let (key, _disp) = hkcu.create_subkey(
            //         r"Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers",
            //     )?;

            //     let val = key.get_value::<String, _>(&vcxsrv_exe);
            //     if val.as_ref().is_ok_and(|val| val != "~ HIGHDPIAWARE")
            //         || val.is_err_and(|err| err.kind() == io::ErrorKind::NotFound)
            //     {
            //         log::debug!("Setting {vcxsrv_exe:?} to HIGHDPI mode");
            //         key.set_value(&vcxsrv_exe, &"~ HIGHDPIAWARE")?;
            //     };

            //     anyhow::Ok(())
            // })?;

            let mut cmd = Command::new(vcxsrv_exe);
            cmd.args([
                "-ac",          // disable access control
                "-noreset",     // dont restart after last client exits
                "-wgl",         // native opengl
                "-compositewm", // previews for windows
                "-dpi",
                "auto",
                "-multiwindow", // seamless mode
                "-clipboard",
                "-noprimary",
                "-logfile",
                &consts::host::DIR_DATA
                    .get_or_create()?
                    .join("vcxsrv.log")
                    .to_string_lossy(),
                "-logverbose",
                if *DEBUG { "3" } else { "2" },
                // , "-vmid"
                // , "{" <> toText wslVmId <> "}"
                "-vsockport",
                "6000",
            ]);
            if !cfg.vcxsrv.tray {
                cmd.arg("-notrayicon");
            }
            cmd.env("__COMPAT_LAYER", "HighDpiAware");
            log::debug!("Starting VcXsrv with arguments: {cmd:?}");
            cmd
                // .stdout(stdout)
                // .stderr(stderr)
                .creation_flags(CREATE_NEW_PROCESS_GROUP.0 | CREATE_NO_WINDOW.0)
                .spawn()?;
        }
        Ok(())
    }

    fn open_terminal(&self, cmd: &[&str]) -> Result<()> {
        let terms = vec![
            ("wt.exe", vec!["nt"]),
            ("alacritty.exe", vec!["-e"]),
            ("wezterm.exe", vec!["-e"]),
        ];
        for (term, args) in &terms {
            if let Ok(path) = which::which(term) {
                Command::new(path).args(args).args(cmd).spawn()?;
                return Ok(());
            }
        }

        anyhow::bail!(
            "Could not find a terminal. Tried: {}",
            terms.iter().map(|(term, _)| term).join(", ")
        )
    }
}
