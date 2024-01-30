use anyhow::{bail, Result};
use base64::{prelude::BASE64_STANDARD, Engine};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::str::from_utf8;
use tarpc::context::Context;
use tokio::sync::{mpsc, OnceCell};

pub use self::cmd::*;
use self::tray::TrayMsg;
pub use self::types::*;

mod cmd;
mod tray;

static MACHINES: OnceCell<DashMap<String, MachineState>> = OnceCell::const_new();

type SResult<T> = std::result::Result<T, String>;

#[tarpc::service]
pub trait ControllerService {
    async fn quit();

    async fn get_status() -> SResult<DashMap<String, MachineState>>;
}

impl ControllerService for ControllerServer {
    async fn quit(self, _: Context) {
        self.tray_chan
            .send(TrayMsg::Quit)
            .await
            .expect("Tray handler disappeared...");
    }

    async fn get_status(self, _: Context) -> SResult<DashMap<String, MachineState>> {
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

#[allow(unused)]
pub fn echo_ca_certs() -> Result<()> {
    let crts = rustls_native_certs::load_native_certs()?;
    for crt in crts {
        println!("-----BEGIN CERTIFICATE-----");
        for line in BASE64_STANDARD.encode(crt.as_ref()).as_bytes().chunks(64) {
            println!("{}", from_utf8(line).unwrap());
        }
        println!("-----END CERTIFICATE-----");
        break;
    }
    Ok(())
}
