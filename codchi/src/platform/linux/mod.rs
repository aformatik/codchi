use std::{fs, path::PathBuf};

use crate::{
    consts::{self, Dir},
    nix,
};

use super::Driver;
use anyhow::{Context, Result};
use lazy_static::lazy_static;

lazy_static! {
    pub static ref DRIVER: DriverImpl = DriverImpl {};
}

pub struct DriverImpl {}

impl Driver for DriverImpl {
    fn init_controller(&self) -> Result<()> {
        if lxd::container::get_info(consts::CONTROLLER_NAME)?.is_none() {
            lxd::image::import(self.get_controller_fs()?, consts::CONTROLLER_NAME)
                .context("Failed to import LXD controller image.")?;

            lxd::image::init(
                consts::CONTROLLER_NAME,
                consts::CONTROLLER_NAME,
                &["security.nesting=true"],
            )
            .context("Failed to create LXD controller container.")?;

            let ctrl_dir = Dir::Data.get_or_create()?.join("controller");

            let mount_local = |rel_path: &str| {
                let dir = ctrl_dir.join(rel_path);
                fs::create_dir_all(&dir)?;
                lxd::container::mount(
                    consts::CONTROLLER_NAME,
                    rel_path,
                    &dir,
                    &format!("/{}", rel_path),
                )
                .with_context(|| {
                    format!(
                        "Failed to mount LXD device '{}' at path {} to controller",
                        rel_path,
                        dir.display()
                    )
                })
            };
            mount_local("nix")?;
            mount_local("instances")?;
            mount_local("instance_state")?;

            lxd::image::delete(consts::CONTROLLER_NAME)?;
        }

        Ok(())
    }

    fn get_controller_fs(&self) -> Result<PathBuf> {
        let dir = nix::cli::build(".#lxd-ctrl-rootfs")
            .context("Failed to build LXD controller rootfs.")?;
        Ok(dir.join("controller.tar.gz"))
    }
}

mod lxd {
    pub use lxd::*;
    use std::path::Path;
    use std::{io, process::Command};

    fn lxc(args: &[&str]) -> io::Result<()> {
        let mut cmd = Command::new("lxc");
        for arg in args.iter() {
            cmd.arg(arg);
        }

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

        pub fn init(image_name: &str, container_name: &str, config: &[&str]) -> io::Result<()> {
            let mut args = vec!["init", image_name, container_name];
            for cfg in config {
                args.push("-c");
                args.push(cfg);
            }
            lxc(&args)
        }
    }

    pub mod container {
        use super::*;

        pub fn get_info(name: &str) -> io::Result<Option<Info>> {
            match Info::new(Location::Local, name) {
                Ok(info) => Ok(Some(info)),
                Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
                Err(e) => Err(e),
            }
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

    // let cmds =
    //         [ "lxc image import $(nix build .#lxd-ctrl-rootfs --no-link --print-out-paths)/controller.tar.gz --alias codchi-controller"
    //         , "lxc init codchi-controller codchi-controller"
    //         , "lxc image delete codchi-controller"
    //         , "mkdir -p $XDG_DATA_HOME/codchi/{nix,instances,instance-state}"
    //         , "lxc config set codchi-controller security.nesting=true"
    //         , "lxc config device add codchi-controller nix disk \"source=$XDG_DATA_HOME/codchi/nix\" path=/nix"
    //         , "lxc config device add codchi-controller instances disk \"source=$XDG_DATA_HOME/codchi/instances\" path=/instances"
    //         , "lxc config device add codchi-controller instance-state disk \"source=$XDG_DATA_HOME/codchi/instance-state\" path=/instance-state"
    //         , "printf \"" <> _IDMAP <> "\" | lxc config set codchi-controller raw.idmap -" -- not sure if this is needed
    //         ]
    // forM_ cmds (runProcess_ . shell)
    //     `catch` ( \case
    //                 ExitFailure _ -> runProcess_ "lxc image delete -q codchi-controller; lxc delete -qf codchi-controller; true"
    //                 _ -> pass
    //             )
}
