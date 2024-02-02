#![deny(unused_crate_dependencies)]

use anyhow::{bail, Result, Context};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::str::from_utf8;
use tarpc::context::Context as TContext;
use tokio::sync::{mpsc, OnceCell};

use crate::platform::{Driver, DRIVER};

pub use self::cmd::*;
use self::tray::TrayMsg;
pub use self::types::*;

mod cmd;
mod tray;

static MACHINES: OnceCell<DashMap<String, MachineState>> = OnceCell::const_new();

type SResult<T> = std::result::Result<T, String>;

#[tarpc::service]
pub trait ControllerService {
    async fn quit() -> SResult<()>;

    async fn get_status() -> SResult<DashMap<String, MachineState>>;
}

impl ControllerService for ControllerServer {
    async fn quit(self, _: TContext) -> SResult<()> {
        DRIVER.stop_controller().map_err(|e| e.to_string())?;
        self.tray_chan
            .send(TrayMsg::Quit)
            .await
            .context("Tray handler disappeared...")
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    async fn get_status(self, _: TContext) -> SResult<DashMap<String, MachineState>> {
        let machines = MACHINES
            .get_or_try_init(|| async {
                // todo!();
                bail!("Foo");
            })
            .await
            .map_err(|e| e.to_string())?;
        Ok(machines.clone())
    }
}

mod types {
    pub use super::*;

    #[derive(Clone)]
    pub struct ControllerServer {
        pub tray_chan: mpsc::Sender<TrayMsg>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
    pub struct MachineState {
        pub status: MachineStatus,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
    pub enum MachineStatus {
        NeedsBuild,
        Dirty,
        Ok,
    }
}

fn main() {

}
