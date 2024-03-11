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
    use std::path::PathBuf;

    use anyhow::Context;
    use itertools::Itertools;

    use crate::{
        consts::{user, ToPath},
        platform::PlatformStatus,
    };

    use super::*;

    #[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
    /// LXD container information
    pub struct Info {
        pub status: String,
    }

    pub fn start(name: &str) -> io::Result<()> {
        lxc(&["start", name])
    }

    pub fn stop(name: &str, force: bool) -> io::Result<()> {
        if force {
            lxc(&["stop", "--force", name])
        } else {
            lxc(&["stop", name])
        }
    }

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

    pub fn get_platform_status(name: &str) -> io::Result<PlatformStatus> {
        Ok(match container::get_info(name)? {
            None => PlatformStatus::NotInstalled,
            Some(container) => {
                if container.status == "Running" {
                    PlatformStatus::Running
                } else {
                    PlatformStatus::Stopped
                }
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

    #[allow(dead_code)]
    pub fn get_logs(name: &str) -> io::Result<String> {
        let out = lxc_output_ok(&["console", name, "--show-log"])?;
        Ok(String::from_utf8_lossy(&out).lines().dropping(2).join("\n"))
    }

    pub fn install<'a, P, I>(name: &str, rootfs: P, mounts: I) -> anyhow::Result<()>
    where
        P: AsRef<Path>,
        I: IntoIterator<Item = (PathBuf, &'a str)>,
    {
        (|| {
            image::import(&rootfs, &name).with_context(|| {
                format!(
                    "Failed to import LXD image {name} from {}.",
                    rootfs.as_ref().to_string_lossy()
                )
            })?;

            image::init(&name, &name).with_context(|| {
                format!(
                    "Failed to create LXD container {name} from {}.",
                    rootfs.as_ref().to_string_lossy()
                )
            })?;

            image::delete(&name)?;

            container::config_set(&name, "security.nesting=true")?;

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
                unsafe {
                    format!(
                        "uid {hostuid} {guestuid}\ngid {hostgid} {guestgid}",
                        hostuid = geteuid(),
                        guestuid = user::ROOT_UID,
                        hostgid = getegid(),
                        guestgid = user::ROOT_GID,
                    )
                }
            };
            container::config_set(&name, &format!("raw.idmap={idmap}"))?;

            for (source, dest) in mounts {
                let source = source.get_or_create()?;
                container::config_mount(
                    &name,
                    dest.strip_prefix("/").unwrap_or(&dest),
                    &source,
                    &dest,
                )
                .with_context(|| {
                    format!(
                        "Failed to mount LXD device '{}' at path {dest} to container {name}.",
                        source.to_string_lossy(),
                    )
                })?;
            }

            container::start(&name).with_context(|| {
                format!(
                    "Failed to start LXD container {name} from {}.",
                    rootfs.as_ref().to_string_lossy()
                )
            })?;

            Ok(())
        })()
        .map_err(|err| {
            log::error!("Removing leftovers of LXD container {name}...");
            let _ = image::delete(&name);
            let _ = container::delete(&name, true);
            err
        })
    }
}
