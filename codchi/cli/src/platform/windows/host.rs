use crate::{
    consts::{self, PathExt},
    platform::{DesktopEntry, Host},
};
use anyhow::Result;
use known_folders::{get_known_folder_path, KnownFolder};
use mslnk::ShellLink;
use std::{env, fs};

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
}
