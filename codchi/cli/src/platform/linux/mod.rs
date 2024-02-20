use super::{private::Private, CommandDriver, LinuxUser, NixDriver, Store};
use crate::{
    cli::DEBUG,
    consts::{self, machine::machine_name, *},
    platform::{Machine, MachineDriver, PlatformStatus},
};
use anyhow::{Context, Result};
use log::*;
use std::{env, path::PathBuf};

pub mod lxd;

pub const NIX_STORE_PACKAGE: &str = "store-lxd";
pub const NIXOS_DRIVER_NAME: &str = "lxd";

pub struct StoreImpl {}

impl Store for StoreImpl {
    fn start_or_init_container(_: Private) -> Result<Self> {
        // TODO add link to docs
        let status = lxd::container::get_platform_status(consts::CONTAINER_STORE_NAME).context(
            "Failed to run LXD.
    It seems like LXD is not installed or set up correctly!
    Please see the codchi docs for the setup instructions!",
        )?;
        trace!("LXD store container status: {status:#?}");

        match status {
            PlatformStatus::NotInstalled => {
                let rootfs = env::var("CODCHI_LXD_CONTAINER_STORE")
                        .map(|dir| PathBuf::from(dir))
                        .context("Failed reading $CODCHI_LXD_CONTAINER_STORE from environment. This indicates a broken build.")?;
                let mounts = vec![
                    (
                        host::DIR_CONFIG.clone(),
                        store::DIR_CONFIG.to_str().unwrap(),
                    ),
                    (host::DIR_DATA.clone(), store::DIR_DATA.to_str().unwrap()),
                    (host::DIR_NIX.clone(), store::DIR_NIX.to_str().unwrap()),
                ];
                lxd::container::install(consts::CONTAINER_STORE_NAME, &rootfs, mounts)?;
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

    fn cmd(&self) -> impl CommandDriver + NixDriver {
        LxdCommandDriver {
            name: consts::CONTAINER_STORE_NAME.to_string(),
        }
    }
}

impl MachineDriver for Machine {
    fn cmd(&self) -> impl CommandDriver {
        LxdCommandDriver {
            name: machine::machine_name(&self.name),
        }
    }

    fn read_platform_status(name: &str, _: Private) -> Result<PlatformStatus> {
        Ok(lxd::container::get_platform_status(
            &machine::machine_name(name),
        )?)
    }

    fn install(&self, _: Private) -> Result<()> {
        let lxd_name = machine_name(&self.name);
        let rootfs = env::var("CODCHI_LXD_CONTAINER_MACHINE")
                .map(|dir| PathBuf::from(dir))
                .context("Failed reading $CODCHI_LXD_CONTAINER_MACHINE from environment. This indicates a broken build.")?;
        let mounts = vec![
            (host::DIR_NIX.join("store"), "/nix/store"),
            (
                host::DIR_NIX.join("var/nix/daemon-socket"),
                "/nix/var/nix/daemon-socket",
            ),
            (host::DIR_NIX.join("var/nix/db"), "/nix/var/nix/db"),
            (
                host::DIR_CONFIG.join_machine(&self.name),
                "/nix/var/nix/profiles",
            ),
            (host::DIR_CONFIG.clone(), "/nix/var/nix/profiles/codchi"),
            (host::DIR_DATA.join_machine(&self.name), user::DEFAULT_HOME),
        ];
        lxd::container::install(&lxd_name, &rootfs, mounts)?;

        Ok(())
    }

    fn start(&self, _: Private) -> Result<()> {
        Ok(lxd::container::start(&machine_name(&self.name))?)
    }

    fn force_stop(&self, _: Private) -> Result<()> {
        Ok(lxd::container::stop(&machine_name(&self.name), true)?)
    }

    fn delete_container(&self, _: Private) -> Result<()> {
        Ok(lxd::container::delete(&machine_name(&self.name), true)?)
    }
}

pub struct LxdCommandDriver {
    pub name: String,
}
impl CommandDriver for LxdCommandDriver {
    fn build(&self, user: Option<LinuxUser>, cwd: Option<String>) -> std::process::Command {
        let mut cmd = std::process::Command::new("lxc");
        cmd.arg("-q");
        cmd.args(&["exec", &self.name]);
        if let Some(cwd) = &cwd {
            cmd.args(&["--cwd", &cwd]);
        }
        if *DEBUG {
            cmd.args(&["--env", "CODCHI_DEBUG=1"]);
        }
        if let Some(user) = &user {
            cmd.args(&[
                "--user",
                match user {
                    LinuxUser::Root => consts::user::ROOT_UID,
                    LinuxUser::Default => consts::user::DEFAULT_UID,
                },
            ]);
            cmd.args(&[
                "--group",
                match user {
                    LinuxUser::Root => consts::user::ROOT_GID,
                    LinuxUser::Default => consts::user::DEFAULT_GID,
                },
            ]);
            cmd.args(&[
                "--env",
                &format!(
                    "HOME={}",
                    match user {
                        LinuxUser::Root => consts::user::ROOT_HOME,
                        LinuxUser::Default => consts::user::DEFAULT_HOME,
                    }
                ),
            ]);
        }
        cmd.arg("--");
        cmd
    }
}
impl NixDriver for LxdCommandDriver {}
