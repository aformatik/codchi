mod host;
mod lxd;

pub use host::*;

use super::{private::Private, LinuxCommandTarget, LinuxPath, LinuxUser, NixDriver, Store};
use crate::{
    cli::DEBUG,
    consts::{self, machine::machine_name, PathExt, ToPath},
    platform::{platform::lxd::container::LxdDevice, Machine, MachineDriver, PlatformStatus},
};
use anyhow::{Context, Result};
use log::*;
use std::{env, fs, path::PathBuf};

pub const NIX_STORE_PACKAGE: &str = "store-lxd";
pub const NIXOS_DRIVER_NAME: &str = "lxd";

pub struct StoreImpl {}

impl Store for StoreImpl {
    fn start_or_init_container(_: Private) -> Result<Self> {
        let status = lxd::container::get_platform_status(consts::CONTAINER_STORE_NAME).context(
            "Failed to run LXD. It seems like LXD is not installed or set up correctly! \
Please see <https://codchi.dev/docs/start/installation.html#linux> for setup instructions!",
        )?;
        trace!("LXD store container status: {status:#?}");

        match status {
            PlatformStatus::NotInstalled => {
                let rootfs = env::var("CODCHI_LXD_CONTAINER_STORE")
                        .map(PathBuf::from)
                        .context("Failed reading $CODCHI_LXD_CONTAINER_STORE from environment. This indicates a broken build.")?;
                let mounts = vec![
                    LxdDevice::Disk {
                        source: consts::host::DIR_CONFIG.get_or_create()?.clone(),
                        path: consts::store::DIR_CONFIG.0.clone(),
                    },
                    LxdDevice::Disk {
                        source: consts::host::DIR_DATA.get_or_create()?.clone(),
                        path: consts::store::DIR_DATA.0.clone(),
                    },
                    LxdDevice::Disk {
                        source: consts::host::DIR_NIX.get_or_create()?.clone(),
                        path: consts::store::DIR_NIX.0.clone(),
                    },
                ];
                lxd::container::install(consts::CONTAINER_STORE_NAME, rootfs, mounts.iter())
                    .map_err(|err| {
                        log::error!("Removing leftovers of store files...");
                        let _ = fs::remove_dir_all(consts::host::DIR_CONFIG.join_store());
                        let _ = fs::remove_dir_all(consts::host::DIR_DATA.join_store());
                        err
                    })?;
                Ok(StoreImpl {})
            }
            PlatformStatus::Stopped => {
                lxd::container::start(consts::CONTAINER_STORE_NAME)
                    .context("Failed to start store container")?;

                Ok(StoreImpl {})
            }
            PlatformStatus::Running => Ok(StoreImpl {}),
        }
    }

    fn cmd(&self) -> impl NixDriver {
        LinuxCommandDriver {
            container_name: consts::CONTAINER_STORE_NAME.to_string(),
        }
    }

    fn _store_path_to_host(&self, path: &LinuxPath, _: Private) -> anyhow::Result<PathBuf> {
        Ok(consts::host::DIR_NIX.join(
            path.0
                .strip_prefix("/nix/")
                .ok_or(anyhow::anyhow!("Path '{path}' doesn't start with '/nix/'"))?,
        ))
    }
}

impl MachineDriver for Machine {
    fn cmd(&self) -> impl LinuxCommandTarget {
        LinuxCommandDriver {
            container_name: consts::machine::machine_name(&self.config.name),
        }
    }

    fn read_platform_status(name: &str, _: Private) -> Result<PlatformStatus> {
        Ok(lxd::container::get_platform_status(
            &consts::machine::machine_name(name),
        )?)
    }

    fn install(&self, _: Private) -> Result<()> {
        let lxd_name = machine_name(&self.config.name);
        let rootfs = env::var("CODCHI_LXD_CONTAINER_MACHINE")
                .map(PathBuf::from)
                .context("Failed reading $CODCHI_LXD_CONTAINER_MACHINE from environment. This indicates a broken build.")?;
        let mut mounts = vec![
            LxdDevice::Disk {
                source: consts::host::DIR_NIX.join("store"),
                path: "/nix/store".to_owned(),
            },
            LxdDevice::Disk {
                source: consts::host::DIR_NIX.join("var/nix/daemon-socket"),
                path: "/nix/var/nix/daemon-socket".to_owned(),
            },
            LxdDevice::Disk {
                source: consts::host::DIR_NIX.join("var/nix/db"),
                path: "/nix/var/nix/db".to_owned(),
            },
            LxdDevice::Disk {
                source: consts::host::DIR_CONFIG.join_machine(&self.config.name),
                path: "/nix/var/nix/profiles".to_owned(),
            },
            LxdDevice::Disk {
                source: consts::host::DIR_CONFIG.clone(),
                path: "/nix/var/nix/profiles/codchi".to_owned(),
            },
            LxdDevice::Disk {
                source: consts::host::DIR_DATA.join_machine(&self.config.name),
                path: consts::user::DEFAULT_HOME.0.clone(),
            },
            LxdDevice::InstanceProxy {
                name: "x11".to_owned(),
                listen: "unix:@/tmp/.X11-unix/X0".to_owned(),
                connect: "unix:@/tmp/.X11-unix/X0".to_owned(),
            },
            LxdDevice::Gpu,
        ];
        if let Ok(xauth) = env::var("XAUTHORITY") {
            // log::info!("Xauthority detected. Whitelisting LXD container {lxd_name} via xhost...");
            // if let Err(err) = Command::new("xhost").arg("+local:").wait_ok() {
            //     log::error!("Failed to run `xhost +local:`. X11 programs inside LXD might not work. Reason:\n{err}");
            // }
            mounts.push(LxdDevice::Disk {
                source: xauth.into(),
                path: format!("{}/.Xauthority", consts::user::DEFAULT_HOME.0),
            });
        }
        lxd::container::install(&lxd_name, rootfs, mounts.iter())?;

        Ok(())
    }

    fn start(&self, _: Private) -> Result<()> {
        Ok(lxd::container::start(&machine_name(&self.config.name))?)
    }

    fn force_stop(&self, _: Private) -> Result<()> {
        Ok(lxd::container::stop(
            &machine_name(&self.config.name),
            true,
        )?)
    }

    fn delete_container(&self, _: Private) -> Result<()> {
        Ok(lxd::container::delete(
            &machine_name(&self.config.name),
            true,
        )?)
    }
}

#[derive(Debug, Clone)]
pub struct LinuxCommandDriver {
    pub container_name: String,
}

impl LinuxCommandTarget for LinuxCommandDriver {
    fn build(&self, user: &Option<LinuxUser>, cwd: &Option<LinuxPath>) -> std::process::Command {
        let mut cmd = std::process::Command::new("lxc");
        cmd.arg("-q");
        cmd.args(["exec", &self.container_name]);
        if let Some(cwd) = &cwd {
            cmd.args(["--cwd", &cwd.0]);
        }
        if *DEBUG {
            cmd.args(["--env", "CODCHI_DEBUG=1"]);
        }
        if let Some(user) = &user {
            cmd.args([
                "--user",
                match user {
                    LinuxUser::Root => consts::user::ROOT_UID,
                    LinuxUser::Default => consts::user::DEFAULT_UID,
                },
            ]);
            cmd.args([
                "--group",
                match user {
                    LinuxUser::Root => consts::user::ROOT_GID,
                    LinuxUser::Default => consts::user::DEFAULT_GID,
                },
            ]);
            cmd.args([
                "--env",
                &format!(
                    "HOME={}",
                    match user {
                        LinuxUser::Root => &consts::user::ROOT_HOME.0,
                        LinuxUser::Default => &consts::user::DEFAULT_HOME.0,
                    }
                ),
            ]);
            cmd.args(["--env", "DISPLAY=:0"]);
            cmd.args([
                "--env",
                &format!("XAUTHORITY={}/.Xauthority", consts::user::DEFAULT_HOME.0),
            ]);
        }
        cmd.arg("--");
        cmd
    }

    fn get_driver(&self) -> Self {
        self.clone()
    }

    fn quote_shell_arg(&self, arg: &str) -> String {
        arg.to_string()
    }
}
