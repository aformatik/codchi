use super::{private::Private, CommandDriver, NixDriver, Store};
use crate::{
    consts::{self, *},
    platform::{Machine, MachineDriver, PlatformStatus},
};
use anyhow::{Context, Result};
use log::*;
use std::{env, path::PathBuf};

pub const NIX_STORE_PACKAGE: &str = "store-lxd";
pub const NIXOS_DRIVER_NAME: &str = "lxd";

pub struct StoreImpl {}

impl Store for StoreImpl {
    fn start_or_init_container(_: Private) -> Result<Self> {
        // TODO add link to docs
        let info = lxd::container::get_info(consts::CONTAINER_STORE_NAME).context(
            "Failed to run LXD.
    It seems like LXD is not installed or set up correctly!
    Please see the codchi docs for the setup instructions!",
        )?;

        match info {
            Some(info) if info.status == "Running" => {
                trace!("LXD controller container is already running.");
                Ok(StoreImpl {})
            }
            Some(info) => {
                trace!("Got info of LXD controller container: {info:#?}");

                lxd::container::start(consts::CONTAINER_STORE_NAME)
                    .context("Failed to start store container")?;

                Ok(StoreImpl {})
            }
            None => {
                (|| {
                    let rootfs = env::var("CODCHI_LXD_CTRL_ROOTFS")
                        .map(|dir| PathBuf::from(dir))
                        .context("Failed reading $CODCHI_LXD_CTRL_ROOTFS from environment. This indicates a broken build.")?;

                    lxd::image::import(rootfs, consts::CONTAINER_STORE_NAME)
                        .context("Failed to import LXD controller image.")?;

                    lxd::image::init(consts::CONTAINER_STORE_NAME, consts::CONTAINER_STORE_NAME)
                        .context("Failed to create LXD controller container.")?;

                    lxd::image::delete(consts::CONTAINER_STORE_NAME)?;

                    lxd::container::config_set(
                        consts::CONTAINER_STORE_NAME,
                        "security.nesting=true",
                    )?;
                    // Map current host user to root in containers to allow accessing their files.
                    // Although this prevents access to other users' files (like /home in a code
                    // machine) from the host, root inside the container should be able to access
                    // them. TODO: check /etc/sub{u,g}id for correctness
                    let idmap = {
                        #[link(name = "c")]
                        extern "C" {
                            /// Get current uid via libc
                            fn geteuid() -> u32;
                            /// Get current gid via libc
                            fn getegid() -> u32;
                        }
                        unsafe { format!("uid {} 0\ngid {} 0", geteuid(), getegid()) }
                    };
                    lxd::container::config_set(
                        consts::CONTAINER_STORE_NAME,
                        &format!("raw.idmap={idmap}"),
                    )?;

                    fn create_and_mount(source: &PathBuf, dest: &PathBuf) -> Result<()> {
                        let source = source.get_or_create()?;
                        let dest = dest.to_str().expect("Invalid UTF8 in store path.");
                        lxd::container::config_mount(
                            consts::CONTAINER_STORE_NAME,
                            dest.strip_prefix("/").unwrap_or(&dest),
                            &source,
                            &dest,
                        )
                        .with_context(|| {
                            format!(
                                "Failed to mount LXD device '{}' at path {} to controller",
                                source.to_string_lossy(),
                                dest
                            )
                        })
                    }
                    create_and_mount(&host::DIR_CONFIG, &store::DIR_CONFIG)?;
                    create_and_mount(&host::DIR_DATA, &store::DIR_DATA)?;
                    create_and_mount(&host::DIR_NIX, &store::DIR_NIX)?;

                    lxd::container::start(consts::CONTAINER_STORE_NAME)
                        .context("Failed to start the store container")?;

                    Ok(StoreImpl {})
                })().map_err(|err| {
                    warn!("Removing leftovers of LXD store container...");
                    let _ = lxd::image::delete(consts::CONTAINER_STORE_NAME);
                    let _ = lxd::container::delete(consts::CONTAINER_STORE_NAME, true);
                    err
                })
            }
        }
    }

    //fn init_controller(&self) -> Result<()> {

    //fn stop_controller(&self) -> Result<()> {
    //    lxd::container::stop(consts::CONTROLLER_NAME, true)?;
    //    Ok(())
    //}

    fn cmd(&self) -> impl CommandDriver + NixDriver {
        LxdCommandDriver {
            name: consts::CONTAINER_STORE_NAME.to_string(),
        }
    }
}

pub struct LxdCommandDriver {
    pub name: String,
}
impl CommandDriver for LxdCommandDriver {
    fn build(&self, uid: Option<usize>, cwd: Option<String>) -> std::process::Command {
        let mut cmd = std::process::Command::new("lxc");
        cmd.arg("-q");
        cmd.args(&["exec", &self.name]);
        if let Some(cwd) = &cwd {
            cmd.args(&["--cwd", &cwd]);
        }
        if let Some(uid) = &uid {
            cmd.args(&["--user", &uid.to_string()]);
        }
        // if needs_stdin {
        //     cmd.arg("-T");
        // }
        cmd.arg("--");
        cmd
    }
}
impl NixDriver for LxdCommandDriver {}

impl MachineDriver for Machine {
    fn read_platform(name: &str, _: Private) -> Result<PlatformStatus> {
        Ok(
            match lxd::container::get_info(&machine::machine_name(name))? {
                None => PlatformStatus::NotInstalled,
                Some(container) => {
                    if container.status == "Running" {
                        PlatformStatus::Running
                    } else {
                        PlatformStatus::Stopped
                    }
                }
            },
        )
    }

    fn cmd(&self) -> impl CommandDriver {
        LxdCommandDriver {
            name: machine::machine_name(&self.name),
        }
    }
}

/// "inspired" by lxd-rs
mod lxd {
    use itertools::Itertools;
    use serde::{Deserialize, Serialize};
    use std::path::Path;
    use std::process::Output;
    use std::{io, process::Command};

    pub fn lxc(args: &[&str]) -> io::Result<()> {
        let mut cmd = Command::new("lxc");
        cmd.arg("-q");
        for arg in args.iter() {
            cmd.arg(arg);
        }

        log::trace!("Running LXC command: {:?}", &cmd);

        let status = cmd.spawn()?.wait()?;
        if status.success() {
            Ok(())
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!("LXD {:?} failed with {}", args, status),
            ))
        }
    }

    pub fn lxc_output(args: &[&str]) -> io::Result<Output> {
        let mut cmd = Command::new("lxc");
        cmd.arg("-q");
        // see https://github.com/rust-lang/rust/issues/30098#issuecomment-160346319
        // cmd.stdin(Stdio::inherit());
        // cmd.stdout(Stdio::inherit());
        // cmd.stderr(Stdio::inherit());
        for arg in args.iter() {
            cmd.arg(arg);
        }

        log::trace!("Running LXC command: {:?}", &cmd);

        cmd.output()
    }
    pub fn lxc_output_ok(args: &[&str]) -> io::Result<Vec<u8>> {
        let output = lxc_output(&args)?;
        if output.status.success() {
            Ok(output.stdout)
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!("LXD {:?} failed with {}", args, output.status),
            ))
        }
    }

    pub mod image {
        use super::*;

        pub fn import<P: AsRef<Path>>(path: P, alias: &str) -> io::Result<()> {
            lxc(&[
                "image",
                "import",
                &format!("{}", path.as_ref().display()),
                "--alias",
                alias,
            ])
        }

        pub fn delete(name: &str) -> io::Result<()> {
            lxc(&["image", "delete", name])
        }

        pub fn init(image_name: &str, container_name: &str) -> io::Result<()> {
            lxc(&["init", image_name, container_name])
        }
    }

    pub mod container {
        use super::*;

        #[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
        /// LXD container information
        pub struct Info {
            pub status: String,
        }

        pub fn start(name: &str) -> io::Result<()> {
            lxc(&["start", name])
        }

        // pub fn stop(name: &str, force: bool) -> io::Result<()> {
        //     if force {
        //         lxc(&["stop", "--force", name])
        //     } else {
        //         lxc(&["stop", name])
        //     }
        // }

        pub fn list(filter: &str) -> io::Result<Vec<Info>> {
            let json = lxc_output_ok(&["list", filter, "--format", "json"])?;
            serde_json::from_slice::<Vec<Info>>(&json).map_err(|err| {
                io::Error::new(
                    io::ErrorKind::Other,
                    format!("LXD info: failed to parse json: {}", err),
                )
            })
        }

        pub fn get_info(name: &str) -> io::Result<Option<Info>> {
            list(name).map(|mut list| {
                if list.len() == 1 {
                    Some(list.remove(0))
                } else {
                    None
                }
            })
        }

        pub fn delete(name: &str, force: bool) -> io::Result<()> {
            if force {
                lxc(&["delete", name, "--force"])
            } else {
                lxc(&["delete", name])
            }
        }
        pub fn config_set(name: &str, cfg: &str) -> io::Result<()> {
            lxc(&["config", "set", name, cfg])
        }

        pub fn config_mount<P: AsRef<Path>>(
            container_name: &str,
            disk_name: &str,
            source: P,
            dest: &str,
        ) -> io::Result<()> {
            lxc(&[
                "config",
                "device",
                "add",
                container_name,
                disk_name,
                "disk",
                &format!("source={}", source.as_ref().display()),
                &format!("path={}", dest),
            ])
        }

        // pub fn get_logs(name: &str) -> io::Result<String> {
        //     let out = lxc_output_ok(&["console", name, "--show-log"])?;
        //     Ok(String::from_utf8_lossy(&out).lines().dropping(2).join("\n"))
        // }
    }
}
