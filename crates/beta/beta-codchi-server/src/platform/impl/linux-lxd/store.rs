use super::{
    lxd::{self, container::LxdDevice},
    shell::LxdShellImpl,
};
use crate::{
    cmd::{LinuxCommand, NixDriver},
    log,
    platform::{shell::ShellDriver, store::Store},
    state::{PlatformStatus, DEBUG},
};
use anyhow::Context;
use ipc::{service::LogLevel, RUNTIME_MT};
use shared::{
    cmd::CommandExt,
    consts::{self, ToPath},
    util::{PathExt, ResultExt},
};
use std::{
    env, fs,
    path::PathBuf,
    sync::{mpsc::channel, OnceLock},
    thread,
    time::Duration,
};

pub struct StoreImpl;

impl StoreImpl {
    pub fn new() -> Self {
        Self
    }
}
// pub const NIX_STORE_PACKAGE: &str = ;
pub const NIXOS_DRIVER_NAME: &str = "lxd";

static STORE: OnceLock<LxdShellImpl> = OnceLock::new();

impl Store for StoreImpl {
    fn NIX_FLAKE_ATTRIBUTE(&self) -> &'static str {
        "store-lxd"
    }

    fn shell(&self) -> impl ShellDriver + 'static {
        LxdShellImpl::new_store()
    }

    fn read_platform_status(&self) -> anyhow::Result<crate::state::PlatformStatus> {
        lxd::container::get_platform_status(consts::CONTAINER_STORE_NAME).context(
            "Failed to run LXD. It seems like LXD is not installed or set up correctly! \
Please see <https://codchi.dev/introduction/installation#linux> for setup instructions!",
        )
    }

    fn install(&self) -> anyhow::Result<()> {
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
            // Mount all machine data as gcroots to prevent gc-ing auto roots from (e.g. direnv)
            LxdDevice::Disk {
                source: consts::host::DIR_DATA
                    .get_or_create()?
                    .join_str(consts::MACHINE_PREFIX)
                    .clone(),
                path: "/nix/var/nix/gcroots/machine-data".to_string(),
            },
        ];
        lxd::container::install(consts::CONTAINER_STORE_NAME, rootfs, mounts.iter()).inspect_err(
            |_err| {
                tracing::error!("Removing leftovers of store files...");
                let _ = fs::remove_dir_all(consts::host::DIR_CONFIG.join_store());
                let _ = fs::remove_dir_all(consts::host::DIR_DATA.join_store());
            },
        )?;
        Ok(())
    }

    fn start(&self) -> anyhow::Result<()> {
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
        let (cancel_tx, cancel_rx) = channel();

        let shell = self.shell();
        thread::spawn(move || {
            let log_file = consts::store::LOGFILE_STORE.0.clone();
            // logger.log(ipc::service::LogLevel::Info, None, String::new());
            shell
                .build(LinuxCommand::script(format!(
                    "touch {log_file}; tail -f {log_file}"
                )))
                // .output_ok()
                .output_ok_streaming(cancel_rx, |line| {
                    tracing::debug!(topic = log::TOPIC_STORE, line);
                    //     tracing::info!("store_init: {line}");
                })
        });
        self.shell().wait_pinging_store()?;
        let _ = cancel_tx
            .send(())
            .trace_err("Failed cancelling output stream thread.");

        anyhow::Ok(())
    }

    fn stop(&self) -> anyhow::Result<()> {
        todo!()
    }
}
