use std::{env, fs, path::PathBuf};

use crate::consts::{self, Dir};

use super::{CommandDriver, Driver, NixDriver, PathBase, PathDriver};
use anyhow::{Context, Result};
use log::*;

pub static DRIVER: DriverImpl = DriverImpl {};

pub struct DriverImpl {}

impl Driver for DriverImpl {
    fn init_controller(&self) -> Result<()> {
        // TODO add link to docs
        let info = lxd::container::get_info(consts::CONTROLLER_NAME).context(
            "Failed to run LXD.
It seems like LXD is not installed or set up correctly! 
Please see the codchi docs for the setup instructions!",
        )?;

        match info {
            None => {
                let try_import = || -> Result<()> {
                    let rootfs = env::var("CODCHI_LXD_CTRL_ROOTFS")
                        .map(|dir| PathBuf::from(dir).join("controller.tar.gz"))
                        .context("Failed reading $CODCHI_LXD_CTRL_ROOTFS from environment. This indicates a broken build.")?;

                    lxd::image::import(rootfs, consts::CONTROLLER_NAME)
                        .context("Failed to import LXD controller image.")?;

                    // This might be not the best idea because root on host = root in LXD
                    // containers
                    let idmap = r"both 0 0
uid 1000 1000
gid 100 100";
                    lxd::image::init(consts::CONTROLLER_NAME, consts::CONTROLLER_NAME)
                        .context("Failed to create LXD controller container.")?;

                    lxd::container::config_set(consts::CONTROLLER_NAME, "security.nesting=true")?;
                    lxd::container::config_set(
                        consts::CONTROLLER_NAME,
                        &format!("raw.idmap={idmap}"),
                    )?;

                    for base in vec![PathBase::Machines, PathBase::Nix, PathBase::State] {
                        let dir = DRIVER.ctrl_cmd().inner_to_outer(base.clone())?;
                        let base_dir = base.to_string();
                        fs::create_dir_all(&dir)?;
                        lxd::container::mount(
                            consts::CONTROLLER_NAME,
                            &base_dir,
                            &dir,
                            &format!("/{}", base_dir),
                        )
                        .with_context(|| {
                            format!(
                                "Failed to mount LXD device '{}' at path {} to controller",
                                base_dir,
                                dir.display()
                            )
                        })?;
                    }
                    //, "printf \"" <> _IDMAP <> "\" | lxc config set codchi-controller raw.idmap -" -- not sure if this is needed
                    lxd::image::delete(consts::CONTROLLER_NAME)?;
                    Ok(())
                };
                try_import().map_err(|err| {
                    warn!("Failed creating LXD controller container: {err}");
                    trace!("Removing LXD leftovers...");
                    let _ = lxd::image::delete(consts::CONTROLLER_NAME);
                    let _ = lxd::container::delete(consts::CONTROLLER_NAME, true);
                    err
                })?;
                self.init_controller()?;
            }
            Some(info) if info.status == "Running" => {
                trace!("LXD controller container is already running.");
            }
            Some(info) => {
                trace!("Got info of LXD controller container: {info:#?}");
                lxd::container::start(consts::CONTROLLER_NAME)
                    .context("Failed to start codchi controller (LXD container)")?;
            }
        }
        Ok(())
    }

    // fn ctrl_cmd_spawn(&self, program: &str, args: &[&str]) -> io::Result<()> {
    //     let mut lxd_args = vec!["exec", consts::CONTROLLER_NAME, "--", "run", program];
    //     lxd_args.extend_from_slice(&args);
    //     lxd::lxc(&lxd_args)
    // }

    // fn ctrl_cmd_output(&self, program: &str, args: &[&str]) -> io::Result<Output> {
    //     let mut lxd_args = vec!["exec", consts::CONTROLLER_NAME, "--", "run", program];
    //     lxd_args.extend_from_slice(&args);
    //     lxd::lxc_output(&lxd_args)
    // }

    fn internal_nixos_name(&self) -> &'static str {
        "lxd"
    }

    fn ctrl_cmd(&self) -> impl CommandDriver + NixDriver + PathDriver {
        LxdCommandDriver
    }
}

pub struct LxdCommandDriver;
impl CommandDriver for LxdCommandDriver {
    fn build(spec: &super::Command) -> std::process::Command {
        let mut cmd = std::process::Command::new("lxc");
        cmd.arg("-q");
        cmd.args(&["exec", consts::CONTROLLER_NAME]);
        if let Some(cwd) = &spec.cwd {
            cmd.args(&["--cwd", &cwd]);
        }
        if let Some(uid) = &spec.uid {
            cmd.args(&["--user", &uid.to_string()]);
        }
        cmd.args(&["--", "run", &spec.program]);
        for arg in spec.args.iter() {
            cmd.arg(arg);
        }
        cmd
    }
}
impl NixDriver for LxdCommandDriver {}
impl PathDriver for LxdCommandDriver {
    fn resolve_root(&self) -> Result<PathBuf> {
        Dir::Data.get_or_create()
    }
}

/// "inspired" by lxd-rs
mod lxd {
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
        use serde::{Deserialize, Serialize};

        #[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
        /// LXD container information
        pub struct Info {
            pub status: String,
        }

        pub fn start(name: &str) -> io::Result<()> {
            lxc(&["start", name])
        }

        pub fn get_info(name: &str) -> io::Result<Option<Info>> {
            let json = lxc_output_ok(&["list", &format!("{}$", name), "--format", "json"])?;
            match serde_json::from_slice::<Vec<Info>>(&json) {
                Ok(mut list) => {
                    if list.len() == 1 {
                        Ok(Some(list.remove(0)))
                    } else {
                        Ok(None)
                    }
                }
                Err(err) => Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("LXD info: failed to parse json: {}", err),
                )),
            }
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

        pub fn mount<P: AsRef<Path>>(
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
    }
}
