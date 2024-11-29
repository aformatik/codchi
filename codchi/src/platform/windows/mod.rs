use self::wsl::wsl_command;
use super::{
    Driver, LinuxCommandTarget, LinuxUser, Machine, MachineDriver, NixDriver, PlatformStatus, Store,
};
use crate::util::{LinuxPath, PathExt, ResultExt, UtilExt};
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
use itertools::Itertools;
use log::Level;
use std::{
    collections::HashMap,
    env,
    fs::{self, OpenOptions},
    io::Write,
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
                consts::host::DIR_DATA
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
                    anyhow::bail!("The store container was started incorrectly. Please try again!");
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
            consts::host::DIR_DATA
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
            let mut env = self.config.secrets.clone();

            env.insert(
                "DEBUG".to_string(),
                if *DEBUG { "1" } else { "" }.to_string(),
            );
            env.insert("MACHINE_NAME".to_string(), self.config.name.clone());

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
                let mut env_file = OpenOptions::new()
                    .truncate(true)
                    .write(true)
                    .create(true)
                    .open(env_path)?;

                for (key, value) in &env {
                    writeln!(env_file, r#"export CODCHI_{key}="{value}""#)?;
                }
                env_file.sync_all()?;

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
                log::error!(
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
        let cmd = match cmd.split_first() {
            Some((cmd, args)) => self.cmd().run(cmd, args),
            None => self.cmd().run("bash", &["-l"]),
        };

        cmd.with_cwd(consts::user::DEFAULT_HOME.clone())
            .with_user(LinuxUser::Default)
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
        if CodchiConfig::get().vcxsrv.enable {
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
