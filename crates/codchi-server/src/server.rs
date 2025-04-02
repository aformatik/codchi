use crate::{
    platform::{store::Store, PlatformRegistry},
    state::{HasLogger, PlatformStatus, ServerState},
};
use ipc::service::LogLevel;
use shared::{
    consts::{self, ToPath},
    util::PathExt,
};
use std::{fs::File, io::Write, sync::Arc};
use tokio::sync::RwLock;

/// All code which runs apart from the api runs here.
///
/// It should not raise errors / panics. Instead they should be written to the server error logs.
///
pub async fn main(state: Arc<RwLock<ServerState>>) {
    let result: anyhow::Result<()> = try {
        let flake_url = consts::CODCHI_FLAKE_URL;
        let system = consts::NIX_SYSTEM;
        let flake_path = consts::host::DIR_CONFIG
            .join_store()
            .get_or_create()?
            .join("flake.nix");
        let store_package = PlatformRegistry::store().NIX_FLAKE_ATTRIBUTE();
        let flake_content = format!(
            r#"{{
  inputs.codchi.url = "{flake_url}";
  outputs = {{ codchi, ... }}: {{
    packages.{system}.default = codchi.packages.{system}.{store_package}.config.build.runtime;
  }};
}}"#
        );
        {
            let mut file = File::create(flake_path)?;
            file.write_all(flake_content.as_bytes())?;
            file.sync_all()?;
        }
        let init_log = state.read().await.store_init_log.clone();
        let init_log = Box::leak(Box::new(init_log));
        match PlatformRegistry::store().read_platform_status()? {
            PlatformStatus::NotInstalled => {
                tracing::info!("Installing store container");
                PlatformRegistry::store().install(init_log)?;
                tracing::info!("Initializing store container");
                PlatformRegistry::store().start(init_log)?;
            }
            PlatformStatus::Stopped => {
                tracing::info!("Starting store container");
                PlatformRegistry::store().start(init_log)?;
            }
            PlatformStatus::Running => {
                tracing::info!("Store container is already running")
            }
        }
        // {
        //     let state = state.read().await;
        //     state
        //         .store_init_log
        //         .send(ipc::service::LogLine {
        //             topic: "status".to_string(),
        //             text: format!("{status:?}"),
        //         })
        //         .ignore();
        // }
    };

    if let Err(err) = result {
        let state = state.read().await;
        state.messages.log(LogLevel::Error, None, format!("{err}"))
    }
}
