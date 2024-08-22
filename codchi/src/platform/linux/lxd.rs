use crate::platform::CommandExt;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::process::Command;

pub fn lxc_command(args: &[&str]) -> Command {
    let mut cmd = Command::new("lxc");
    cmd.arg("-q");
    cmd.args(args);
    cmd
}

pub mod image {
    use super::*;

    pub fn import<P: AsRef<Path>>(path: P, alias: &str) -> Result<()> {
        Ok(lxc_command(&[
            "image",
            "import",
            &format!("{}", path.as_ref().display()),
            "--alias",
            alias,
        ])
        .wait_ok()?)
    }

    pub fn delete(name: &str) -> Result<()> {
        Ok(lxc_command(&["image", "delete", name]).wait_ok()?)
    }

    pub fn init(image_name: &str, container_name: &str) -> Result<()> {
        Ok(lxc_command(&["init", image_name, container_name]).wait_ok()?)
    }
}

pub mod container {
    use super::*;
    use crate::platform::{LinuxUser, PlatformStatus};
    use crate::{
        consts::user,
        util::{LinuxPath, PathExt},
    };
    use anyhow::{anyhow, Context};
    use nix::unistd::{getgid, getuid, Group};
    use std::path::PathBuf;

    #[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
    /// LXD container information
    pub struct Info {
        pub name: String,
        pub status: String,
    }

    pub fn start(name: &str) -> Result<()> {
        lxc_command(&["start", name]).wait_ok()?;
        Ok(())
    }

    pub fn stop(name: &str, force: bool) -> Result<()> {
        let mut cmd = lxc_command(&["stop", name]);
        if force {
            cmd.arg("--force");
        }
        cmd.wait_ok()?;
        Ok(())
    }

    pub fn get_info(name: &str) -> Result<Option<Info>> {
        let info = lxc_command(&["list", "--format", "json"])
            .output_json::<Vec<Info>>()?
            .iter()
            .find(|info| info.name == name)
            .cloned();
        Ok(info)
    }

    pub fn get_platform_status(name: &str) -> Result<PlatformStatus> {
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

    pub fn delete(name: &str, force: bool) -> Result<()> {
        let mut cmd = lxc_command(&["delete", name]);
        if force {
            cmd.arg("--force");
        }
        cmd.wait_ok()?;
        Ok(())
    }

    pub fn config_set(name: &str, cfg: &str) -> Result<()> {
        lxc_command(&["config", "set", name, cfg]).wait_ok()?;
        Ok(())
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

    pub fn config_mount(container_name: &str, device: &LxdDevice) -> Result<()> {
        match device {
            LxdDevice::Disk { source, path } => {
                let source = source.get_or_create()?.display();
                lxc_command(&[
                    "config",
                    "device",
                    "add",
                    container_name,
                    path.strip_prefix('/').unwrap_or(path),
                    "disk",
                    &format!("source={}", source),
                    &format!("path={}", path),
                ])
                .wait_ok()
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
            } => lxc_command(&[
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
            .wait_ok()
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
                lxc_command(&[
                    "config",
                    "device",
                    "add",
                    container_name,
                    "gpu",
                    "gpu",
                    &format!("gid={}", video.gid),
                ])
                .wait_ok()
                .with_context(|| {
                    format!("Failed to create LXD GPU device in container {container_name}.",)
                })
            }
        }
    }

    // pub fn get_logs(name: &str) -> String {
    //     lxc_command(&["console", name, "--show-log"])
    //         .output_utf8_ok()
    //         .unwrap_or_default()
    // }

    pub fn install<'a, P, I>(name: &str, rootfs: P, mounts: I) -> Result<()>
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

            // container::start(name).with_context(|| {
            //     format!(
            //         "Failed to start LXD container {name} from {}.",
            //         rootfs.as_ref().to_string_lossy()
            //     )
            // })?;

            Ok(())
        })()
        .inspect_err(|_err| {
            log::error!("Removing leftovers of LXD container {name}...");
            let _ = image::delete(name);
            let _ = container::delete(name, true);
        })
    }

    /// Pushes file for default user
    pub fn file_push<P: AsRef<Path>>(
        name: &str,
        path: P,
        target: LinuxPath,
        user: Option<LinuxUser>,
    ) -> Result<()> {
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
        lxc_command(&args).wait_ok().with_context(|| {
            format!(
                "Failed to copy file '{}' from host into LXD container {name} to {}",
                path.as_ref().display(),
                target.0
            )
        })
    }
    pub fn file_delete(name: &str, target: LinuxPath) -> Result<()> {
        let target_arg = format!("{name}/{}", target);
        lxc_command(&["file", "delete", &target_arg])
            .wait_ok()
            .with_context(|| {
                format!(
                    "Failed to delete file '{}' in LXD container {name}",
                    target.0
                )
            })
    }
}
