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
    let output = lxc_output(args)?;
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
    use crate::{
        consts::{user, PathExt},
        platform::{LinuxPath, LinuxUser, PlatformStatus},
    };
    use anyhow::{anyhow, Context};
    use itertools::Itertools;
    use nix::unistd::{getgid, getuid, Group};
    use std::path::PathBuf;

    #[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
    /// LXD container information
    pub struct Info {
        pub name: String,
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

    pub fn get_info(name: &str) -> io::Result<Option<Info>> {
        let json = lxc_output_ok(&["list", "--format", "json"])?;
        Ok(serde_json::from_slice::<Vec<Info>>(&json)
            .map_err(|err| {
                io::Error::new(
                    io::ErrorKind::Other,
                    format!("LXD info: failed to parse json: {}", err),
                )
            })?
            .iter()
            .find(|info| info.name == name)
            .cloned())
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

    #[derive(Debug, Clone)]
    pub enum LxdDevice {
        Disk {
            source: PathBuf,
            path: String,
        },

        InstanceProxy {
            name: String,
            listen: String,
            connect: String,
        },
        #[allow(unused)]
        Gpu,
    }

    pub fn config_mount(container_name: &str, device: &LxdDevice) -> anyhow::Result<()> {
        match device {
            LxdDevice::Disk { source, path } => {
                let source = source.get_or_create()?.display();
                lxc(&[
                    "config",
                    "device",
                    "add",
                    container_name,
                    path.strip_prefix('/').unwrap_or(path),
                    "disk",
                    &format!("source={}", source),
                    &format!("path={}", path),
                ])
                .with_context(|| {
                    format!(
                        "Failed to mount LXD device '{source}' at path '{path}' \
to container {container_name}.",
                    )
                })
            }
            LxdDevice::InstanceProxy {
                name,
                listen,
                connect,
            } => lxc(&[
                "config",
                "device",
                "add",
                container_name,
                name,
                "proxy",
                "bind=instance",
                &format!("connect={}", connect),
                &format!("listen={}", listen),
                &format!("security.uid={}", getuid()),
                &format!("security.gid={}", getgid()),
            ])
            .with_context(|| {
                format!(
                    "Failed to create LXD proxy '{name}' from '{listen}' \
to '{connect}' in container {container_name}.",
                )
            }),
            LxdDevice::Gpu => {
                let video: Group = Group::from_name("video")?.ok_or(anyhow!(
                    "Group 'video' (which is needed for GPU access) not found."
                ))?;
                lxc(&[
                    "config",
                    "device",
                    "add",
                    container_name,
                    "gpu",
                    "gpu",
                    &format!("gid={}", video.gid),
                ])
                .with_context(|| {
                    format!("Failed to create LXD GPU device in container {container_name}.",)
                })
            }
        }
    }

    #[allow(dead_code)]
    pub fn get_logs(name: &str) -> io::Result<String> {
        let out = lxc_output_ok(&["console", name, "--show-log"])?;
        Ok(String::from_utf8_lossy(&out).lines().dropping(2).join("\n"))
    }

    pub fn install<'a, P, I>(name: &str, rootfs: P, mounts: I) -> anyhow::Result<()>
    where
        P: AsRef<Path>,
        I: Iterator<Item = &'a LxdDevice>,
    {
        (|| {
            image::import(&rootfs, name).with_context(|| {
                format!(
                    "Failed to import LXD image {name} from {}.",
                    rootfs.as_ref().to_string_lossy()
                )
            })?;

            image::init(name, name).with_context(|| {
                format!(
                    "Failed to create LXD container {name} from {}.",
                    rootfs.as_ref().to_string_lossy()
                )
            })?;

            image::delete(name)?;

            container::config_set(name, "security.nesting=true")?;

            // Map current host user to root in containers to allow accessing their files.
            // Although this prevents access to other users' files (like /home in a code
            // machine) from the host, root inside the container should be able to access
            // them. TODO: check /etc/sub{u,g}id for correctness
            let idmap = format!(
                "uid {hostuid} {guestuid}\ngid {hostgid} {guestgid}",
                hostuid = getuid(),
                guestuid = user::ROOT_UID,
                hostgid = getgid(),
                guestgid = user::ROOT_GID,
            );
            container::config_set(name, &format!("raw.idmap={idmap}"))?;

            for mount in mounts {
                container::config_mount(name, mount)?;
            }

            container::start(name).with_context(|| {
                format!(
                    "Failed to start LXD container {name} from {}.",
                    rootfs.as_ref().to_string_lossy()
                )
            })?;

            Ok(())
        })()
        .map_err(|err| {
            log::error!("Removing leftovers of LXD container {name}...");
            let _ = image::delete(name);
            let _ = container::delete(name, true);
            err
        })
    }

    /// Pushes file for default user
    pub fn file_push<P: AsRef<Path>>(
        name: &str,
        path: P,
        target: LinuxPath,
        user: Option<LinuxUser>,
    ) -> anyhow::Result<()> {
        let src = path.as_ref().to_string_lossy();
        let target_arg = format!("{name}/{}", target);
        let mut args = vec!["file", "push", &src, &target_arg];
        match user {
            Some(LinuxUser::Default) => {
                args.extend(["--uid", user::DEFAULT_UID, "--gid", user::DEFAULT_GID]);
            }
            Some(LinuxUser::Root) => {
                args.extend(["--uid", user::ROOT_UID, "--gid", user::ROOT_GID]);
            }
            None => {}
        }
        lxc(&args).with_context(|| {
            format!(
                "Failed to copy file '{}' from host into LXD container {name} to {}",
                path.as_ref().display(),
                target.0
            )
        })
    }
    pub fn file_delete(name: &str, target: LinuxPath) -> anyhow::Result<()> {
        let target_arg = format!("{name}/{}", target);
        lxc(&["file", "delete", &target_arg]).with_context(|| {
            format!(
                "Failed to delete file '{}' in LXD container {name}",
                target.0
            )
        })
    }
}
