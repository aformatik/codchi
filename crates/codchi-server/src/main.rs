#![deny(unused_crate_dependencies)]

use ipc::service::*;
use ipc::SERVER_ADDR;
use remoc::{codec, prelude::*};
use shared::consts;
use tokio::net::TcpListener;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::EnvFilter;

mod platform;
mod server;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::FmtSubscriber::builder()
        .with_env_filter(
            EnvFilter::builder()
                .with_env_var(consts::LOG_ENV_SERVER)
                .with_default_directive(LevelFilter::DEBUG.into())
                .from_env()?,
        )
        .with_span_events(FmtSpan::CLOSE)
        .init();

    let state = server::State::new();

    let listener = TcpListener::bind(SERVER_ADDR).await.unwrap();
    tracing::info!("Listening on {SERVER_ADDR:?}");

    loop {
        let (socket, addr) = listener.accept().await.unwrap();
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
