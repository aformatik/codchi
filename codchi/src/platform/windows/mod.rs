use self::wsl::wsl_command;
use super::{
    Driver, LinuxCommandTarget, LinuxUser, Machine, MachineDriver, NixDriver, PlatformStatus, Store,
};
use crate::progress_scope;
use crate::util::{with_tmp_file, LinuxPath, PathExt, ResultExt, UtilExt};
use crate::{
    cli::DEBUG,
    config::CodchiConfig,
    consts::{
        self, files,
        machine::{self, machine_name, CODCHI_ENV, CODCHI_ENV_TMP},
        store, ToPath,
    },
    logging::{log_progress, set_progress_status},
    platform::CommandExt,
};
use anyhow::{Context, Result};
pub use host::*;
use inquire::InquireError;
use itertools::Itertools;
use log::Level;
use std::{
    collections::HashMap,
    env, fs,
    io::{self, IsTerminal},
    path::PathBuf,
    sync::mpsc::channel,
    thread,
    time::Duration,
};

mod host;
mod wsl;

pub const NIX_STORE_PACKAGE: &str = "store-wsl";
pub const NIXOS_DRIVER_NAME: &str = "wsl";

pub struct StoreImpl {}

impl Store for StoreImpl {
    fn start_or_init_container() -> Result<Self> {
        wsl::check_wsl()?;

        let status = wsl::get_platform_status(consts::CONTAINER_STORE_NAME)?;
        log::trace!("WSL store container status: {status:#?}");

        let store = StoreImpl {};
        match status {
            PlatformStatus::NotInstalled => wsl::import(
                files::STORE_ROOTFS_NAME,
                consts::CONTAINER_STORE_NAME,
                &consts::host::DIR_DATA
                    .join_store()
                    .get_or_create()?
                    .to_path_buf(),
                || {
                    wsl::set_sparse(consts::CONTAINER_STORE_NAME)?;
                    set_progress_status(
                        "Initializing store container. This takes a while the first time...",
                    );
                    Store::init()
                },
            )
            .inspect_err(|_| {
                if !log::log_enabled!(Level::Debug) {
                    log::error!("Removing leftovers of store files...");
                    let _ = fs::remove_dir_all(consts::host::DIR_CONFIG.join_store());
                    let _ = fs::remove_dir_all(consts::host::DIR_DATA.join_store());
                }
            }),
            PlatformStatus::Running => {
                while store
                    .cmd()
                    .run("ps", &[])
                    .output_utf8_ok()?
                    .contains("/sbin/init")
                {
                    set_progress_status(
                        "The store is currently initializing. Please wait a moment...",
                    );
                    thread::sleep(Duration::from_millis(500));
                }
                if store.cmd().ping_store() {
                    Ok(store)
                } else {
                    let _ = wsl::wsl_command()
                        .arg("--terminate")
                        .arg(consts::CONTAINER_STORE_NAME)
                        .wait_ok()
                        .trace_err("Failed stopping incorrectly started store container");
                    log::warn!("The store container was started incorrectly. Restarting...");
                    Store::init()
                }
            }
            PlatformStatus::Stopped => {
                store
                    .cmd()
                    .run("/sbin/init", &[])
                    .with_cwd(LinuxPath("/".to_string()))
                    .output_ok_streaming(channel().1, |line| {
                        log_progress("store_init", Level::Debug, &line)
                    })?;

                if CodchiConfig::get().enable_wsl_vpnkit {
                    start_wsl_vpnkit(&store)?;
                }

                Ok(store)
            }
        }
    }

    fn cmd(&self) -> impl NixDriver {
        LinuxCommandDriver {
            instance_name: consts::CONTAINER_STORE_NAME.to_string(),
        }
    }

    fn _store_path_to_host(&self, path: &LinuxPath) -> anyhow::Result<std::path::PathBuf> {
        self.cmd()
            .run("/bin/wslpath", &["-w", &path.0])
            .output_utf8_ok()
            .map(|path| PathBuf::from(path.trim()))
            .with_context(|| format!("Failed to run 'wslpath' with path '{path}'."))
    }
}

pub fn start_wsl_vpnkit(store: &impl Store) -> Result<()> {
    store
        .cmd()
        .script(
            r#"
mkdir -p /var/log
daemonize -e /var/log/wsl-vpnkit -o /var/log/wsl-vpnkit /bin/nix run 'nixpkgs#wsl-vpnkit'
"#
            .to_string(),
        )
        .wait_ok()?;
    Ok(())
}

pub fn stop_wsl_vpnkit(store: &impl Store) -> Result<()> {
    store.cmd().run("pkill", &["wsl-vpnkit"]).wait_ok()?;
    Ok(())
}

pub fn win_path_to_wsl(path: &PathBuf) -> anyhow::Result<LinuxPath> {
    wsl_command()
        .args([
            "-d",
            &consts::CONTAINER_STORE_NAME,
            "--system",
            "--user",
            "root",
        ])
        .args([
            "wslpath",
            "-u",
            &path.display().to_string().replace("\\", "/"),
        ])
        .output_utf8_ok()
        .map(|path| LinuxPath(path.trim().to_owned()))
        .with_context(|| format!("Failed to run 'wslpath' with path {path:?}."))
}

pub fn store_debug_shell() -> anyhow::Result<()> {
    LinuxCommandDriver {
        instance_name: consts::CONTAINER_STORE_NAME.to_string(),
    }
    .run("bash", &[])
    .exec()?;
    Ok(())
}

pub fn store_recover() -> anyhow::Result<()> {
    use wsl::extract_from_msix;

    extract_from_msix(files::STORE_ROOTFS_NAME, |store_tar| {
        let tar_from_wsl = win_path_to_wsl(store_tar)?;
        extract_from_msix("busybox", |busybox| {
            let busybox_from_wsl = win_path_to_wsl(busybox)?;
            wsl_command()
                .args([
                    "-d",
                    &consts::CONTAINER_STORE_NAME,
                    "--system",
                    "--user",
                    "root",
                ])
                .args(["mount", "-o", "remount,rw", "/mnt/wslg/distro"])
                .wait_ok()?;
            wsl_command()
                .args([
                    "-d",
                    &consts::CONTAINER_STORE_NAME,
                    "--system",
                    "--user",
                    "root",
                ])
                .args([
                    &busybox_from_wsl.0,
                    "tar",
                    "-C",
                    "/mnt/wslg/distro",
                    "-xzf",
                    &tar_from_wsl.0,
                ])
                .wait_ok()?;

            let _ = wsl::wsl_command()
                .arg("--terminate")
                .arg(consts::CONTAINER_STORE_NAME)
                .wait_ok()
                .trace_err("Failed stopping store container");

            log::info!("Restored file system of `codchistore`.");
            Ok(())
        })
    })
}

impl MachineDriver for Machine {
    fn cmd(&self) -> impl LinuxCommandTarget {
        LinuxCommandDriver {
            instance_name: machine::machine_name(&self.config.name),
        }
    }

    fn read_platform_status(name: &str) -> Result<PlatformStatus> {
        wsl::get_platform_status(&machine::machine_name(name))
    }

    fn install(&self) -> Result<()> {
        wsl::import(
            files::MACHINE_ROOTFS_NAME,
            &machine::machine_name(&self.config.name),
            &consts::host::DIR_DATA
                .join_machine(&self.config.name)
                .get_or_create()?
                .to_path_buf(),
            || {
                // give windows time to setup WSL filesystem as a network drive
                thread::sleep(Duration::from_millis(200));
                self.start()
            },
        )
    }

    fn start(&self) -> Result<()> {
        {
            let cfg = CodchiConfig::get();
            let mut env = self.config.secrets.clone();

            // TODO consolidate with Machine::write_env_file
            env.insert(
                "DEBUG".to_string(),
                if *DEBUG { "1" } else { "" }.to_string(),
            );
            env.insert("MACHINE_NAME".to_string(), self.config.name.clone());
            env.insert(
                "ENABLE_NETNS".to_string(),
                if cfg.enable_wsl_netns { "1" } else { "" }.to_string(),
            );

            // machine must run to write env file into it...
            let env_path = machine::CODCHI_ENV_TMP.to_host_path(&machine_name(&self.config.name));
            for _try in 0..5 {
                if env_path
                    .parent()
                    .ok_or(anyhow::anyhow!("Missing parent"))
                    .and_then(|par| {
                        par.get_or_create()?;
                        Ok(())
                    })
                    .trace_err("Failed accessing WSL file system via network")
                    .is_ok()
                {
                    break;
                }
                log::warn!("Failed to access WSL file system via network path '{env_path:?}'");
                thread::sleep(Duration::from_millis(200));
            }

            if let Err(err) = (|| {
                self.write_env_file(&env_path)?;

                if self.platform_status == PlatformStatus::Running {
                    self.cmd()
                        .script(format!(
                            r#"
while [ ! -f {tmp_env} ]; do
    sleep .25
done
mv -f {tmp_env} {etc_env}
"#,
                            tmp_env = CODCHI_ENV_TMP.0,
                            etc_env = CODCHI_ENV.0
                        ))
                        .with_user(LinuxUser::Root)
                        .wait_ok()?;
                }
                anyhow::Ok(())
            })() {
                log::warn!(
                    "Failed to write '{}': {err}. Trying backup method...",
                    CODCHI_ENV_TMP.0
                );
            }

            Driver::store()
                .cmd()
                .script(format!(
                    r#"
cat << EOF > /mnt/wsl/codchi/.machine-init-env
{}
EOF
"#,
                    env.iter()
                        .map(|(key, value)| format!(r#"export CODCHI_{key}="{value}""#))
                        .join("\n")
                ))
                .wait_ok()?;
        }

        let log_file = LinuxPath("/mnt/wsl/codchi".to_string())
            .join_str(&store::machine_log(&self.config.name).0);
        let (cancel_tx, cancel_rx) = channel();
        // let machine_log_prefix = machine_name(&self.name);
        thread::spawn(move || {
            // Tail the init log of the machine until the keyword MACHINE_HAS_STARTED
            Driver::store()
                .cmd()
                .script(format!(
                    r#"
mkdir -p "$(dirname {log_file})" || true
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
            .retry_until_ok();

        cancel_tx
            .send(())
            .trace_err("Failed cancelling output stream thread.")
            .ignore();

        Ok(())
    }

    fn stop(&self, _force: bool) -> Result<()> {
        wsl::wsl_command()
            .arg("--terminate")
            .arg(machine_name(&self.config.name))
            .wait_ok()?;
        Ok(())
    }

    fn delete_container(&self) -> Result<()> {
        wsl_command()
            .arg("--unregister")
            .arg(machine_name(&self.config.name))
            .wait_ok()?;
        Ok(())
    }

    fn create_exec_cmd(&self, cmd: &[&str]) -> super::LinuxCommandBuilder {
        // let cmd = match cmd.split_first() {
        //     Some((cmd, args)) => self.cmd().run(cmd, args),
        //     None => self.cmd().run("bash", &["-l"]),
        // };
        let cmd = if cmd.is_empty() {
            self.cmd().raw(
                "/run/current-system/sw/bin/machinectl",
                &[
                    &["shell", "-q", &format!("{}@", consts::user::DEFAULT_NAME)],
                    cmd,
                ]
                .concat(),
            )
        } else {
            self.cmd().raw(
                "/run/current-system/sw/bin/machinectl",
                &[
                    "shell",
                    "-q",
                    &format!("{}@", consts::user::DEFAULT_NAME),
                    "/bin/bash",
                    "-lc",
                    &cmd.join(" "),
                ],
            )
        };

        // cmd.with_cwd(consts::user::DEFAULT_HOME.clone())
        //     .with_user(LinuxUser::Default)
        cmd.with_user(LinuxUser::Root)
    }

    fn tar(&self, target_file: &std::path::Path) -> Result<()> {
        let target_absolute = if target_file.is_absolute() {
            target_file.to_path_buf()
        } else {
            env::current_dir()?.join(target_file)
        };
        let wsl_cmd = || {
            let mut cmd = wsl_command();
            cmd.args([
                "-d",
                &consts::machine::machine_name(&self.config.name),
                "--system",
                "--user",
                "root",
            ]);
            cmd
        };

        let upper = "/mnt/wslg/overlay/upper";
        let work = "/mnt/wslg/overlay/work";
        let merged = "/mnt/wslg/overlay/merged";
        wsl_cmd()
            .args(["mkdir", "-p", upper, work, merged])
            .wait_ok()?;
        wsl_cmd()
            .args([
                "mount",
                "-t",
                "overlay",
                "overlay",
                "-o",
                &format!("lowerdir=/mnt/wslg/distro,upperdir={upper},workdir={work}"),
                merged,
            ])
            .wait_ok()?;
        // cant --exclude and --include with busybox tar, so we manually unwanted files (only in
        // the overlayfs)
        wsl_cmd()
            .args(["rm", "-r", &format!("{merged}/etc")])
            .wait_ok()?;

        with_tmp_file(&format!("codchi-backup-{}", self.config.name), |tmp_dir| {
            fs::create_dir_all(tmp_dir)?;

            let etc_dir = tmp_dir.join("etc");
            let etc_nixos_dir = etc_dir.join("nixos");
            etc_nixos_dir.get_or_create()?;

            self.write_flake_standalone(etc_nixos_dir.join("flake.nix"))?;
            fs::copy(
                consts::host::DIR_CONFIG
                    .join_machine(&self.config.name)
                    .join("flake.lock"),
                etc_nixos_dir.join("flake.lock"),
            )
            .trace_err("Failed copying flake.lock")
            .ignore();

            self.write_env_file(etc_dir.join("codchi-env"))?;

            let tmp_in_wsl = win_path_to_wsl(&tmp_dir)?;

            wsl_cmd()
                .args(["cp", "-r", &format!("{}/*", tmp_in_wsl.0), merged])
                .wait_ok()?;

            Ok(())
        })?;

        let wsl_path = win_path_to_wsl(&target_absolute)?;
        wsl_cmd()
            .args([
                "/mnt/wslg/distro/bin/tar",
                "-C",
                merged,
                "--exclude=./.files",
                "--exclude=./bin*",
                "--exclude=./dev*",
                // "--exclude=etc*",
                "--exclude=./init",
                "--exclude=./lib*",
                "--exclude=./lib64*",
                "--exclude=./nix*",
                "--exclude=./proc*",
                "--exclude=./run*",
                "--exclude=./sbin*",
                "--exclude=./sys*",
                "--exclude=./tmp*",
                "--exclude=./var/.updated",
                "--exclude=./var/cache",
                "--exclude=./var/db*",
                "--exclude=./var/empty",
                "--exclude=./var/lib/nixos*",
                "--exclude=./var/lib/systemd*",
                "--exclude=./var/lock",
                "--exclude=./var/log*",
                "--exclude=./var/spool*",
                "-cf",
                &wsl_path.0,
                ".",
            ])
            .wait_ok()?;

        Ok(())
    }

    fn duplicate_container(&self, target: &Machine) -> Result<()> {
        let target_name = consts::machine::machine_name(&target.config.name);
        if Self::read_platform_status(&self.config.name)? == PlatformStatus::Running {
            if io::stdin().is_terminal()
                && inquire::Confirm::new(&format!(
                    "Machine '{}' needs to be stopped in order to have a consistent file system \
state. Is this OK? [y/n]",
                    self.config.name
                ))
                .prompt()
                .recover_err(|err| match err {
                    InquireError::NotTTY => Ok(false),
                    err => Err(err),
                })?
            {
                self.stop(false)?;
                thread::sleep(Duration::from_millis(500));
            } else {
                anyhow::bail!(
                    "Duplicating a running machine might result in inconsistent file system state"
                );
            }
        }
        progress_scope! {
            set_progress_status("Copying file system...");
            let src_path = consts::host::DIR_DATA
                .join_machine(&self.config.name)
                .join("ext4.vhdx");
            let target_path = consts::host::DIR_DATA
                .join_machine(&target.config.name)
                .get_or_create()?
                .join("ext4.vhdx");
            fs::copy(src_path, &target_path).context("Failed copying ext4.vhdx")?;
            wsl_command()
                .args([
                    "--import-in-place",
                    &target_name,
                    &target_path.display().to_string(),
                ])
                .wait_ok()
                .context("Failed importing ext4.vhdx inplace...")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LinuxCommandDriver {
    pub instance_name: String,
}

impl LinuxCommandTarget for LinuxCommandDriver {
    fn build(
        &self,
        user: &Option<LinuxUser>,
        cwd: &Option<LinuxPath>,
        _env: &HashMap<String, String>,
    ) -> std::process::Command {
        let cfg = CodchiConfig::get();
        let mut cmd = wsl_command();
        cmd.args(["-d", &self.instance_name]);
        cmd.args(["--cd", &cwd.clone().map(|p| p.0).unwrap_or("/".to_string())]);

        // https://devblogs.microsoft.com/commandline/share-environment-vars-between-wsl-and-windows/
        cmd.env("CODCHI_DEBUG", if *DEBUG { "1" } else { "" });
        cmd.env("CODCHI_MACHINE_NAME", &self.instance_name); // only neccessary for machines, ignored in store
        cmd.env("CODCHI_IS_STORE", "1"); // only neccessary for store, ignored in machines
        cmd.env(
            "WSL_CODCHI_DIR_CONFIG",
            consts::host::DIR_CONFIG.as_os_str(),
        );
        cmd.env("WSL_CODCHI_DIR_DATA", consts::host::DIR_DATA.as_os_str());
        let mut wslenv = env::var_os("WSLENV").unwrap_or("".into());
        // log::trace!("WSLENV: {wslenv:?}");
        if !wslenv.is_empty() {
            wslenv.push(":");
        }
        wslenv.push(
            "CODCHI_DEBUG:CODCHI_MACHINE_NAME:CODCHI_IS_STORE:WSL_CODCHI_DIR_CONFIG/up:WSL_CODCHI_DIR_DATA/up",
        );
        if cfg.vcxsrv.enable {
            cmd.env("CODCHI_WSL_USE_VCXSRV", "1");
            wslenv.push(":CODCHI_WSL_USE_VCXSRV");
        }
        cmd.env("WSLENV", wslenv);

        match &user {
            Some(LinuxUser::Root) => {
                cmd.args(["--user", "root"]);
            }
            Some(LinuxUser::Default) => {
                cmd.args(["--user", consts::user::DEFAULT_NAME]);
            }
            None => {}
        };
        cmd.arg("--");
        cmd
    }

    fn get_driver(&self) -> LinuxCommandDriver {
        self.clone()
    }

    fn quote_shell_arg(&self, arg: &str) -> String {
        format!("'{}'", arg)
    }
}
