use crate::service::*;
use crate::{RUNTIME_MT, SERVER_ADDR};
use anyhow::{bail, Result};
use remoc::prelude::*;
use shared::consts;
use shared::exe::{CodchiExe, CommandExt};
use shared::util::PathExt;
use std::fs::File;
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::time::Duration;
use tokio::net::TcpStream;
use tokio::time::timeout;

const MAX_TRIES: u8 = 5;
const TIMEOUT_MS: u64 = 200;
const INITIAL_SLEEP_MS: u64 = 200;

pub static CLIENT_API: OnceLock<ApiClient> = OnceLock::new();

pub fn get_or_connect() -> Result<&'static ApiClient> {
    CLIENT_API.get_or_try_init(|| RUNTIME_MT.block_on(connect()))
}

pub async fn connect() -> Result<ApiClient> {
    let connect = || async {
        let socket = TcpStream::connect(SERVER_ADDR).await?;
        let (socket_rx, socket_tx) = socket.into_split();
        let client: ApiClient = remoc::Connect::io(remoc::Cfg::default(), socket_rx, socket_tx)
            .consume()
            .await?;
        anyhow::Ok(client)
    };

    let mut try_no = MAX_TRIES;
    let mut sleep_duration = Duration::from_millis(INITIAL_SLEEP_MS);
    log::trace!("Connecting to codchi-server");

    loop {
        match timeout(Duration::from_millis(TIMEOUT_MS), connect()).await {
            Ok(Ok(client)) => return Ok(client),
            Ok(Err(err)) => {
                log::trace!("Failed connecting to codchi-server ({err}). Retrying");
            }
            Err(_) => {
                log::trace!("Connection to codchi-server timed out. Increasing timeout")
            }
        }
        if try_no > 0 {
            log::trace!("Daemonizing codchi-server executable");
            let mut cmd = Command::new(CodchiExe::Server.get_path()?);
            let server_log = consts::host::DIR_RUNTIME
                .get_or_create()?
                .join("codchi-server.log");
            let log_file = File::create(server_log)?;
            cmd.stdout(log_file.try_clone()?);
            cmd.stderr(log_file);
            cmd.spawn_daemonized()?;
            tokio::time::sleep(sleep_duration).await;
            sleep_duration = sleep_duration * 2;
            try_no -= 1;
        } else {
            bail!("Failed connecting to codchi-server after {MAX_TRIES} tries");
        }
    }
}
