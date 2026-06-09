use crate::state::ServerState;
use futures::StreamExt;
use futures_signals::signal::SignalExt;
use ipc::logging::LogLine;
use ipc::service::*;
use remoc::{
    rch::{self, broadcast},
    rtc,
};
use shared::util::{ResultExt, UtilExt};
use tracing::instrument;

#[rtc::async_trait]
impl Api for ServerState {
    #[instrument]
    async fn wait_ready(&mut self) -> RtcResult<rch::watch::Receiver<ServerStatus>> {
        let (tx, rx) = rch::watch::channel(self.status.lock_ref().clone());

        let status_signal = self.status.signal_cloned();

        tokio::spawn(
            status_signal
                .stop_if(|status| matches!(status, ServerStatus::Ready | ServerStatus::Degraded))
                .for_each(move |status| {
                    tx.send(status).trace_err("Failed sending status").ignore();
                    async {}
                }),
        );

        Ok(rx)
    }

    #[instrument]
    async fn stream_log(&mut self) -> RtcResult<broadcast::Receiver<LogLine>> {
        Ok(self.log.subscribe(5))
    }
}
