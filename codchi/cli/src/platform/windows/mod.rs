use std::{env, fs, process::exit};

use self::wsl::wsl_command;

use super::{
    private::Private, LinuxCommandTarget, LinuxUser, Machine, MachineDriver, NixDriver, Store,
};
use crate::{
    cli::DEBUG,
    consts::{self, host, machine, ToPath},
    platform::{CommandExt, PlatformStatus},
    util::make_writeable_if_exists,
    ROOT_PROGRESS_BAR,
};
use anyhow::anyhow;
use known_folders::{get_known_folder_path, KnownFolder};

pub const NIX_STORE_PACKAGE: &str = "store-wsl";
pub const NIXOS_DRIVER_NAME: &str = "wsl";

mod status;
mod wsl;

pub struct StoreImpl {}

// https://github.com/rust-lang/cargo/issues/1721

impl Store for StoreImpl {
    fn start_or_init_container(_: Private) -> anyhow::Result<Self> {
        wsl::check_wsl()?;

        let status = wsl::get_platform_status(consts::CONTAINER_STORE_NAME)?;
        log::trace!("WSL store container status: {status:#?}");

        match status {
            PlatformStatus::NotInstalled => (|| {
                let msix_path = get_known_folder_path(KnownFolder::ProgramData)
                    .ok_or(anyhow!("FOLDERID_ProgramData missing"))?
                    .join(consts::APP_NAME)
                    .join(consts::files::STORE_ROOTFS_NAME);
                assert!(
                    fs::metadata(&msix_path).is_ok(),
                    "Store rootfs missing in MSIX. Search path was: {msix_path:?}"
                );

                let tmp_path = host::DIR_RUNTIME
                    .join(consts::APP_NAME)
                    .get_or_create()?
                    .join(consts::files::STORE_ROOTFS_NAME);
                make_writeable_if_exists(&tmp_path)?;
                fs::copy(msix_path, &tmp_path)?;

                wsl::wsl_command()
                    .arg("--import")
                    .arg(consts::CONTAINER_STORE_NAME)
                    .arg(host::DIR_DATA.join_store().get_or_create()?)
                    .arg(&tmp_path)
                    .wait_ok()?;
                host::DIR_DATA.get_or_create()?;
                host::DIR_CONFIG.get_or_create()?;

                make_writeable_if_exists(&tmp_path)?;
                fs::remove_file(&tmp_path)?;

                Self::start_or_init_container(Private)
            })()
            .map_err(|err| {
                log::error!("Removing leftovers of WSL store container...");
                let _ = wsl::wsl_command()
                    .arg("--terminate")
                    .arg(consts::CONTAINER_STORE_NAME)
                    .wait_ok();
                let _ = wsl::wsl_command()
                    .arg("--unregister")
                    .arg(consts::CONTAINER_STORE_NAME)
                    .wait_ok();
                err
            }),
            PlatformStatus::Stopped => {
                let store = StoreImpl {};
                // Start init in background. this will keep the WSL distro running
                store
                    .cmd()
                    .run("/sbin/init", &[])
                    .outout_ok_streaming(|out| log::info!("{out}\r"))?;

                Ok(store)
            }
            PlatformStatus::Running => Ok(StoreImpl {}),
        }
    }

    fn cmd(&self) -> impl LinuxCommandTarget + NixDriver {
        LinuxCommandDriver {
            instance_name: consts::CONTAINER_STORE_NAME.to_string(),
        }
    }
}

impl MachineDriver for Machine {
    fn cmd(&self) -> impl LinuxCommandTarget {
        LinuxCommandDriver {
            instance_name: machine::machine_name(&self.name),
        }
    }

    fn read_platform_status(_name: &str, _: Private) -> anyhow::Result<super::PlatformStatus> {
        todo!()
    }

    fn install(&self, _: Private) -> anyhow::Result<()> {
        todo!()
    }

    fn start(&self, _: Private) -> anyhow::Result<()> {
        todo!()
    }

    fn force_stop(&self, _: Private) -> anyhow::Result<()> {
        todo!()
    }

    fn delete_container(&self, _: Private) -> anyhow::Result<()> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct LinuxCommandDriver {
    pub instance_name: String,
}

impl LinuxCommandTarget for LinuxCommandDriver {
    fn build(&self, user: &Option<LinuxUser>, cwd: &Option<String>) -> std::process::Command {
        let mut cmd = wsl_command();
        cmd.args(&["-d", &self.instance_name]);
        cmd.args(&["--cd", &cwd.clone().unwrap_or("/".to_string())]);

        // https://devblogs.microsoft.com/commandline/share-environment-vars-between-wsl-and-windows/
        cmd.env("CODCHI_DEBUG", if *DEBUG { "1" } else { "" });
        cmd.env("CODCHI_MACHINE_NAME", &self.instance_name); // only neccessary for machines, ignored in store
        cmd.env("CODCHI_IS_STORE", "1"); // only neccessary for store, ignored in machines
        cmd.env("WSL_CODCHI_DIR_CONFIG", host::DIR_CONFIG.as_os_str());
        cmd.env("WSL_CODCHI_DIR_DATA", host::DIR_DATA.as_os_str());
        let mut wslenv = env::var_os("WSLENV").unwrap_or("".into());
        if !wslenv.is_empty() {
            wslenv.push(":");
        }
        wslenv.push(
            "CODCHI_DEBUG:CODCHI_MACHINE_NAME:CODCHI_IS_STORE:WSL_CODCHI_DIR_CONFIG/up:WSL_CODCHI_DIR_DATA/up",
        );
        cmd.env("WSLENV", wslenv);

        match &user {
            Some(LinuxUser::Root) => {
                cmd.args(&["--user", "root"]);
            }
            Some(LinuxUser::Default) => {
                cmd.args(&["--user", consts::user::DEFAULT_NAME]);
            }
            None => {}
        };
        cmd.arg("--");
        cmd
    }

    fn get_driver(&self) -> LinuxCommandDriver {
        self.clone()
    }
}
