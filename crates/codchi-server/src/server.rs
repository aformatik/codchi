use crate::log::codchi_log;
use crate::platform::logging::parse_container_log;
use crate::platform::store::GenFlake;
use crate::state::ServerStateOps;
use crate::{
    codchi_log,
    platform::{store::Store, PlatformRegistry, Virtualization},
    state::{PlatformStatus, ServerState},
};
use anyhow::{bail, Context};
use ipc::health::HealthTopic;
use ipc::logging::{LogLevel, LogLine, LogTopic};
use ipc::service::ServerStatus;
use remoc::rch::broadcast;
use shared::{
    consts::{self, ToPath},
    util::PathExt,
};
use std::sync::OnceLock;
use std::{fs::File, io::Write, sync::Arc};
use tokio::sync::RwLock;

pub static GLOBAL_LOGGER: OnceLock<broadcast::Sender<LogLine>> = OnceLock::new();

/// All code which runs apart from the api runs here.
///
/// It should not raise errors / panics. Instead, they should be written to the server error logs.
///
pub async fn main(state: Arc<RwLock<ServerState>>) {
    let log_tx = state.read().await.log.clone();
    GLOBAL_LOGGER
        .set(log_tx)
        .expect("Failed setting global logger");

    let result: anyhow::Result<()> = try {
        // do some initial health checks
        state
            .add_health_check(
                HealthTopic::OS,
                PlatformRegistry::virtualization().check_os_health(),
            )
            .await?;
        state
            .add_health_check(
                HealthTopic::Virtualization,
                PlatformRegistry::virtualization().check_virtualization_health(),
            )
            .await?;

        // write the store's flake.nix
        let store = PlatformRegistry::get_store();
        let flake_path = consts::host::DIR_CONFIG
            .join_store()
            .get_or_create()?
            .join("flake.nix");
        try {
            let flake_content = store.gen_flake();
            codchi_log!(
                Trace,
                Server,
                "Writing store's flake.nix to {flake_path:?}:\n{flake_content}"
            );
            let mut file = File::create(&flake_path)?;
            file.write_all(flake_content.as_bytes())?;
            file.sync_all()?;
        }
        .with_context(|| format!("Failed to create file {flake_path:?}"))?;

        match store.read_platform_status()? {
            PlatformStatus::NotInstalled => {
                codchi_log!(Info, Server, "Installing store container");
                state.set_status(ServerStatus::StoreInitializing).await;
                store.register()?;

                codchi_log!(Info, Server, "Initializing store container");
                state.set_status(ServerStatus::StoreStarting).await;
                start_with_supervision(state.clone(), store)
                    .await
                    .context("Failed initializing store container")?;
            }
            PlatformStatus::Stopped => {
                codchi_log!(Info, Server, "Starting store container");
                state.set_status(ServerStatus::StoreStarting).await;
                start_with_supervision(state.clone(), store).await
                    .context("Failed starting store container")?;
            }
            PlatformStatus::Running => {
                codchi_log!(Info, Server, "Store container is already running");
            }
        }

        // health checks
        // - check if basic shell command in store succeeds
        // - maybe check if staticBin is installed
        // - check if runtimePackages are installed
        // - check if store ping succeeds (maybe max. 10 sec)
        // - possible remedy: repair store (wsl)

        // let ping_job = tokio::spawn(async {
        //     while !store.shell().ping_store() {
        //         tokio::time::sleep(Duration::from_millis(250)).await;
        //     }
        // });

        state.set_status(ServerStatus::Ready).await;
    };

    if let Err(err) = result {
        codchi_log!(Error, Server, "{err}");
    }
}

async fn start_with_supervision(
    state: Arc<RwLock<ServerState>>,
    store: &impl Store,
) -> anyhow::Result<()> {
    let rx = store.start()?;
    while let Ok(line) = rx.recv() {
        let line = parse_container_log(LogLevel::Info, LogTopic::StoreContainer, line);
        codchi_log(line.clone());
        if line.level == LogLevel::Error {
            bail!("Error while starting nix store container: {line:?}");
        }
    }
    Ok(())
}
