mod host;
mod lxd;

use super::{Driver, LinuxCommandTarget, LinuxUser, NixDriver, Store};
use crate::{
    cli::DEBUG,
    consts::{self, machine::machine_name, store, user, ToPath},
    logging::{log_progress, set_progress_status, with_suspended_progress},
    platform::{
        platform::lxd::container::LxdDevice, CommandExt, Machine, MachineDriver, PlatformStatus,
    },
    util::{with_tmp_file, LinuxPath, PathExt, ResultExt, UtilExt},
};
use anyhow::{bail, Context, Result};
pub use host::*;
use inquire::Confirm;
use log::*;
use std::{
    collections::HashMap,
    env,
    fs::{self, File},
    io::Write,
    path::PathBuf,
    process::Command,
    sync::mpsc::channel,
    thread,
};

pub const NIX_STORE_PACKAGE: &str = "store-lxd";
pub const NIXOS_DRIVER_NAME: &str = "lxd";

pub struct StoreImpl {}

impl Store for StoreImpl {
    fn start_or_init_container() -> Result<Self> {
        let status = lxd::container::get_platform_status(consts::CONTAINER_STORE_NAME).context(
            "Failed to run LXD. It seems like LXD is not installed or set up correctly! \
Please see <https://codchi.dev/introduction/installation#linux> for setup instructions!",
        )?;
        trace!("LXD store container status: {status:#?}");

        let start = || {
            lxd::container::config_set(
                consts::CONTAINER_STORE_NAME,
                &format!("environment.CODCHI_DEBUG={}", if *DEBUG { "1" } else { "" }),
            )?;
            if let Ok(PlatformStatus::Stopped) =
                lxd::container::get_platform_status(consts::CONTAINER_STORE_NAME)
            {
                lxd::container::start(consts::CONTAINER_STORE_NAME)
                    .context("Failed to start store container")?;
            }
            let log_file = store::LOGFILE_STORE.0.clone();
            let (cancel_tx, cancel_rx) = channel();

            thread::spawn(move || {
                StoreImpl {}
                    .cmd()
                    .script(format!("touch {log_file}; tail -f {log_file}"))
                    .output_ok_streaming(cancel_rx, |line| {
                        log_progress("store_init", Level::Debug, &line)
                    })
            });
            let this = StoreImpl {};
            this.cmd().wait_pinging_store()?;
            let _ = cancel_tx
                .send(())
                .trace_err("Failed cancelling output stream thread.");

            anyhow::Ok(this)
        };

        match status {
            PlatformStatus::NotInstalled => {
                set_progress_status(
                    "Initializing store container. This can take a while the first time...",
                );
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
                    // Mount all machine data as gcroots to prevent gc-ing auto roots from e.g. direnv
                    LxdDevice::Disk {
                        source: consts::host::DIR_DATA
                            .get_or_create()?
                            .join_str(consts::MACHINE_PREFIX)
                            .clone(),
                        path: "/nix/var/nix/gcroots/machine-data".to_string(),
                    },
                ];
                lxd::container::install(consts::CONTAINER_STORE_NAME, rootfs, mounts.iter())
                    .inspect_err(|_err| {
                        log::error!("Removing leftovers of store files...");
                        let _ = fs::remove_dir_all(consts::host::DIR_CONFIG.join_store());
                        let _ = fs::remove_dir_all(consts::host::DIR_DATA.join_store());
                    })?;
                start()
            }
            PlatformStatus::Stopped => start(),
            PlatformStatus::Running => Ok(StoreImpl {}),
        }
    }

    fn cmd(&self) -> impl NixDriver {
        LinuxCommandDriver {
            container_name: consts::CONTAINER_STORE_NAME.to_string(),
        }
    }

    fn _store_path_to_host(&self, path: &LinuxPath) -> anyhow::Result<PathBuf> {
        Ok(consts::host::DIR_NIX.join(
            path.0
                .strip_prefix("/nix/")
                .ok_or(anyhow::anyhow!("Path '{path}' doesn't start with '/nix/'"))?,
        ))
    }
}

pub fn store_debug_shell() -> anyhow::Result<()> {
    LinuxCommandDriver {
        container_name: consts::CONTAINER_STORE_NAME.to_string(),
    }
    .run("bash", &[])
    .exec()?;
    Ok(())
}

impl MachineDriver for Machine {
    fn cmd(&self) -> impl LinuxCommandTarget {
        LinuxCommandDriver {
            container_name: consts::machine::machine_name(&self.config.name),
        }
    }

    fn read_platform_status(name: &str) -> Result<PlatformStatus> {
        lxd::container::get_platform_status(&consts::machine::machine_name(name))
    }

    fn install(&self) -> Result<()> {
        let lxd_name = machine_name(&self.config.name);
        let rootfs = env::var("CODCHI_LXD_CONTAINER_MACHINE")
                .map(PathBuf::from)
                .context("Failed reading $CODCHI_LXD_CONTAINER_MACHINE from environment. This indicates a broken build.")?;
        let mounts = vec![
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
            LxdDevice::Disk {
                source: consts::host::DIR_DATA.join("log"),
                path: "/var/log/codchi".to_owned(),
            },
            LxdDevice::InstanceProxy {
                name: "x11".to_owned(),
                listen: "unix:@/tmp/.X11-unix/X0".to_owned(),
                connect: "unix:@/tmp/.X11-unix/X0".to_owned(),
            },
            LxdDevice::Gpu,
        ];
        lxd::container::install(&lxd_name, rootfs, mounts.iter())?;

        Ok(())
    }

    fn start(&self) -> Result<()> {
        if let Ok(PlatformStatus::Stopped) = Self::read_platform_status(&self.config.name) {
            let name = &machine_name(&self.config.name);
            lxd::container::config_set(
                name,
                &format!("environment.CODCHI_DEBUG={}", if *DEBUG { "1" } else { "" }),
            )?;
            lxd::container::start(name)?;
        }

        {
            let src = env::var("XAUTHORITY").map(PathBuf::from).ok().unwrap_or(
                PathBuf::from(env::var("HOME").context("Missing $HOME.")?).join(".Xauthority"),
            );
            if src.assert_exists().is_ok() {
                log::debug!("Adding local .Xauthority to {}", self.config.name);
                lxd::container::file_delete(
                    &machine_name(&self.config.name),
                    user::DEFAULT_HOME.join_str(".Xauthority"),
                )
                .trace_err("Failed to delete old .Xauthority in LXD container.")
                .ignore();
                lxd::container::file_push(
                    &machine_name(&self.config.name),
                    &src,
                    user::DEFAULT_HOME.join_str(".Xauthority"),
                    Some(LinuxUser::Default),
                )?;
            }
            with_tmp_file(&format!("codchi-{}-env", self.config.name), |path| {
                let mut env = self.config.secrets.clone();

                env.insert(
                    "DEBUG".to_string(),
                    if *DEBUG { "1" } else { "" }.to_string(),
                );
                env.insert("MACHINE_NAME".to_string(), self.config.name.clone());

                let mut env_file = File::options()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open(path)?;
                for (key, value) in env {
                    writeln!(env_file, r#"export CODCHI_{key}="{value}""#)?;
                }
                env_file.sync_all()?;
                lxd::container::file_push(
                    &machine_name(&self.config.name),
                    path,
                    LinuxPath("/etc/codchi-env".to_string()),
                    Some(LinuxUser::Root),
                )?;
                Ok(())
            })?;
        }

        let log_file = store::machine_log(&self.config.name);
        let (cancel_tx, cancel_rx) = channel();
        thread::spawn(move || {
            // Tail the init log of the machine until the keyword MACHINE_HAS_STARTED
            Driver::store()
                .cmd()
                .script(format!(
                    r#"
touch "{log_file}"
tail -f "{log_file}"
"#
                ))
                .output_ok_streaming(cancel_rx, |line| {
                    log_progress("machine_init", Level::Debug, &line)
                })
                .unwrap();
        });

        // Machine is started by issuing a command
        self.cmd()
            .script(r#"systemctl is-system-running | grep -E "running|degraded""#.to_string())
            .with_user(LinuxUser::Default)
            .retry_until_ok();

        // somehow the above doesn't suffice when LXC container is freshly started
        self.cmd()
            .run("bash", &["-lc", "ls"])
            .with_user(LinuxUser::Default)
            .retry_until_ok();

        cancel_tx
            .send(())
            .trace_err("Failed cancelling output stream thread.")
            .ignore();

        Ok(())
    }

    fn stop(&self, force: bool) -> Result<()> {
        lxd::container::stop(&machine_name(&self.config.name), force)
    }

    fn delete_container(&self) -> Result<()> {
        lxd::container::delete(&machine_name(&self.config.name), true)
    }

    fn create_exec_cmd(&self, cmd: &[&str]) -> super::LinuxCommandBuilder {
        // let args = [&[consts::user::DEFAULT_NAME], cmd].concat();
        let cmd = if cmd.is_empty() {
            self.cmd().raw(
                "machinectl",
                &[
                    &[
                        "shell",
                        "-q",
                        "-E",
                        "DISPLAY",
                        "-E",
                        "XAUTHORITY",
                        &format!("{}@", consts::user::DEFAULT_NAME),
                    ],
                    cmd,
                ]
                .concat(),
            )
        } else {
            self.cmd().raw(
                "machinectl",
                &[
                    "shell",
                    "-q",
                    "-E",
                    "DISPLAY",
                    "-E",
                    "XAUTHORITY",
                    &format!("{}@", consts::user::DEFAULT_NAME),
                    "/bin/bash",
                    "-c",
                    &cmd.join(" "),
                ],
            )
        };

        cmd.with_user(LinuxUser::Root)
    }

    fn tar(&self, target_file: &std::path::Path) -> Result<()> {
        fn command_with_privileges(reason: &str, command: &[&str]) -> Result<Command> {
            let sudo_path = which::which("sudo").ok();
            let doas_path = which::which("doas").ok();

            let (sudo_name, sudo) = if let Some(path) = sudo_path {
                ("sudo", path)
            } else if let Some(path) = doas_path {
                ("doas", path)
            } else {
                bail!(
                    "Neither sudo nor doas was found on this system. \
This is needed in order to {reason}."
                );
            };
            log::debug!("Found {sudo_name} at {sudo:?}");

            let message = format!(
                "Codchi needs to invoke `{}` as root in order to {reason}. Is it okay to use {sudo_name}?",
                command.join(" ")
            );
            let user_confirmed = Confirm::new(&message).with_default(true).prompt()?;

            if user_confirmed {
                let mut cmd = Command::new(sudo);
                cmd.args(command);
                Ok(cmd)
            } else {
                bail!("Operation was canceled by the user");
            }
        }

        with_tmp_file(&format!("codchi-backup-{}", self.config.name), |tmp_dir| {
            fs::create_dir_all(tmp_dir)?;
            let lxc_export = tmp_dir.join("lxc_export.tar").display().to_string();
            let target_file = target_file.display().to_string();
            lxd::container::export(
                &consts::machine::machine_name(&self.config.name),
                &lxc_export,
            )?;
            Command::new("tar")
                .args(["-C", &tmp_dir.display().to_string(), "-xf", &lxc_export])
                .wait_ok()?;
            with_suspended_progress(|| {
                command_with_privileges(
                    "export the code machine root file system",
                    &[
                        "tar",
                        "-C",
                        &tmp_dir
                            .join("backup/container/rootfs")
                            .display()
                            .to_string(),
                        "-cf",
                        &target_file,
                        ".",
                    ],
                )?
                .wait_ok()?;

                // add home dir
                command_with_privileges(
                    "export the code machine home directory",
                    &[
                        "tar",
                        "--append",
                        "-f",
                        &target_file,
                        "-C",
                        &consts::host::DIR_DATA
                            .join_machine(&self.config.name)
                            .display()
                            .to_string(),
                        "--transform",
                        "s|^./|./home/codchi/|",
                        "--owner=1000",
                        "--group=100",
                        "--numeric-owner",
                        ".",
                    ],
                )?
                .wait_ok()?;

                command_with_privileges(
                    "cleanup temporary files",
                    &["rm", "-rf", &tmp_dir.display().to_string()],
                )?
                .wait_ok()?;
                Ok(())
            })
        })?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LinuxCommandDriver {
    pub container_name: String,
}

impl LinuxCommandTarget for LinuxCommandDriver {
    fn build(
        &self,
        user: &Option<LinuxUser>,
        cwd: &Option<LinuxPath>,
        env: &HashMap<String, String>,
    ) -> std::process::Command {
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
        for (name, val) in env {
            // should be already escaped / no escaping needed on linux
            cmd.args(["--env", &format!("{name}={val}")]);
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
