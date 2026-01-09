#![feature(once_cell_try)]
#![feature(try_blocks)]
// #![deny(unused_crate_dependencies)]

use ipc::service::*;
use ipc::SERVER_ADDR;
use remoc::{codec, prelude::*};
use shared::consts;
use shared::util::UtilExt;
use state::ServerState;
use std::io::stderr;
use std::mem;
use std::process::Stdio;
use std::time::Duration;
use tokio::net::TcpListener;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::{fmt, EnvFilter};

mod api;
mod platform;
mod server;
mod state;
mod log;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let state = ServerState::new();

    let appender = tracing_appender::rolling::never(consts::host::DIR_DATA.as_path(), "server.log");
    let (appender, _guard) = tracing_appender::non_blocking(appender);
    mem::forget(_guard);
    let subscriber = tracing_subscriber::registry()
        // filter log level from env variable
        .with(
            EnvFilter::builder()
                .with_env_var(consts::LOG_ENV_SERVER)
                .with_default_directive(LevelFilter::DEBUG.into())
                .from_env()?,
        )
        // output (with span events) to stdout
        .with(fmt::layer().with_span_events(FmtSpan::CLOSE))
        // append to log file
        .with(fmt::layer().with_writer(appender));
        // send to subscribed clients
        // .with(state.read().await.clone());

    tracing::subscriber::set_global_default(subscriber)?;

    let listener = TcpListener::bind(SERVER_ADDR).await?;
    tracing::info!("Listening on {SERVER_ADDR:?}");

    {
        let state = state.clone();
        tokio::spawn(async move { server::main(state).await });
    }

    loop {
        let (socket, addr) = listener.accept().await?;
        let (socket_rx, socket_tx) = socket.into_split();
        tracing::info!("Accepted connection from {}", addr);

        let state = state.clone();
        tokio::spawn(async move {
            let (server, client) = ApiServerSharedMut::<_, codec::Default>::new(state, 1);

            remoc::Connect::io(remoc::Cfg::default(), socket_rx, socket_tx)
                .provide(client)
                .await
                .unwrap();

            server.serve(true).await.unwrap();
        });
    }
}
