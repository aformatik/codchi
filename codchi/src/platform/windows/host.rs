use crate::{
    cli::DEBUG,
    config::CodchiConfig,
    consts::{self, APP_NAME},
    platform::{DesktopEntry, Host},
    util::PathExt,
};
use anyhow::{Context, Result};
use itertools::Itertools;
use known_folders::{get_known_folder_path, KnownFolder};
use mslnk::{FileAttributeFlags, LinkFlags, MSLinkError, ShellLink};
use std::{env, fs, os::windows::process::CommandExt, path::Path, process::Command};
use sysinfo::System;
use windows::Win32::System::Threading::{
    CREATE_NEW_CONSOLE, CREATE_NEW_PROCESS_GROUP, CREATE_NO_WINDOW,
};

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

        let codchi_exe = get_known_folder_path(KnownFolder::LocalAppData)
            .expect("FOLDERID_LocalAppData missing")
            .join("Microsoft")
            .join("WindowsApps")
            .join("codchi.exe");
        let codchiw_exe = codchi_exe
            .parent()
            .with_context(|| format!("Missing parent of {codchi_exe:?}"))?
            .join("codchiw.exe");

        fn shell_link_new<P: AsRef<Path>>(target: P) -> Result<ShellLink, MSLinkError> {
            let meta = fs::metadata(&target)?;
            let working_dir_path = target
                .as_ref()
                .parent()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned();
            let mut sl = ShellLink::default();

            let mut flags = LinkFlags::IS_UNICODE;
            sl.header_mut().set_link_flags(flags);
            if meta.is_dir() {
                sl.header_mut()
                    .set_file_attributes(FileAttributeFlags::FILE_ATTRIBUTE_DIRECTORY);
            } else {
                flags |= LinkFlags::HAS_WORKING_DIR
                    | LinkFlags::HAS_RELATIVE_PATH
                    | LinkFlags::HAS_LINK_TARGET_ID_LIST;
                sl.header_mut().set_link_flags(flags);
                sl.set_relative_path(Some(format!("./{}", target.as_ref().to_str().unwrap())));
                sl.set_working_dir(Some(working_dir_path));
                sl.header_mut().set_file_size(meta.len() as u32);
                // set link_target_idlist
                sl.linktarget_mut().unwrap().set_linktarget(&target);
            }

            Ok(sl)
        }

        for DesktopEntry {
            app_name,
            name,
            exec,
            icon,
            is_terminal,
        } in apps
        {
            let mut lnk = shell_link_new(if *is_terminal {
                codchi_exe.clone()
            } else {
                codchiw_exe.clone()
            })?;
            if let Some(ico_path) = icon {
                let target = ico_folder.join(format!("{app_name}.ico"));
                fs::copy(ico_path, &target)?;
                lnk.set_icon_location(Some(target.to_string_lossy().to_string()));
            }
            lnk.set_arguments(Some(format!("exec {machine_name} {exec}")));
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
                .processes_by_name("vcxsrv.exe".as_ref())
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
                .join("vcxsrv.exe");
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

    fn post_install(machine_name: &str) -> Result<()> {
        // write json fragments for windows terminal
        {
            // from https://learn.microsoft.com/en-us/windows/terminal/json-fragment-extensions#calculating-a-guid-for-a-built-in-profile
            let guid = {
                use uuid::Uuid;
                // Windows Terminal namespace GUID for auto-generated profiles
                let terminal_namespace_guid =
                    Uuid::parse_str("2bde4a90-d05f-401c-9492-e40884ead1d8")?;
                let profile_name = consts::machine::machine_name(machine_name);

                let utf16le_bytes: Vec<u8> = profile_name
                    .encode_utf16()
                    .flat_map(|unit| unit.to_le_bytes()) // little-endian (BOM-less)
                    .collect();

                Uuid::new_v5(&terminal_namespace_guid, &utf16le_bytes)
            };
            let fragment_dir = get_known_folder_path(KnownFolder::LocalAppData)
                .expect("FOLDERID_LocalAppData missing")
                .join("Microsoft")
                .join("Windows Terminal")
                .join("Fragments")
                .join(APP_NAME);
            fragment_dir.get_or_create()?;

            let fragment_path = fragment_dir.join(format!("{machine_name}.json"));

            // format! with Debug {codchi_icon:?} because Windows Terminal wants double backspaces
            let codchi_icon = env::current_exe()?
                .parent()
                .context("Failed to access codchi.exe install dir.")?
                .join("Assets")
                .join("favicon.ico");

            fs::write(
                fragment_path,
                format!(
                    r#"{{
  "profiles": [
    {{
      "updates": "{{{guid}}}",
      "commandline": "codchi.exe exec {machine_name}",
      "name": "Codchi - {machine_name}",
      "icon": {codchi_icon:?},
    }}
  ]
}}"#
                ),
            )?;
        }

        Ok(())
    }

    fn post_delete(machine_name: &str) -> Result<()> {
        get_known_folder_path(KnownFolder::LocalAppData)
            .expect("FOLDERID_LocalAppData missing")
            .join("Microsoft")
            .join("Windows Terminal")
            .join("Fragments")
            .join(APP_NAME)
            .join(format!("{machine_name}.json"))
            .remove();

        Ok(())
    }

    fn execute(machine_name: &str, desktop_entry: &DesktopEntry) -> Result<()> {
        let codchi_exe = get_known_folder_path(KnownFolder::LocalAppData)
            .expect("FOLDERID_LocalAppData missing")
            .join("Microsoft")
            .join("WindowsApps")
            .join("codchi.exe");
        let exe = if desktop_entry.is_terminal {
            codchi_exe
        } else {
            codchi_exe
                .parent()
                .with_context(|| format!("Missing parent of {codchi_exe:?}"))?
                .join("codchiw.exe")
        };

        let mut cmd = Command::new(&exe);
        cmd.args(["exec", machine_name]);
        for arg in desktop_entry.exec.split(" ") {
            cmd.arg(arg);
        }
        cmd.creation_flags(CREATE_NEW_CONSOLE.0).spawn()?;

        Ok(())
    }
}
