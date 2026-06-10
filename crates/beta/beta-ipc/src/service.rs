use crate::logging::LogLine;
use remoc::prelude::*;
use serde::{Deserialize, Serialize};

pub type RtcResult<T> = Result<T, rtc::CallError>;

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub enum ServerStatus {
    #[default]
    Stopped,
    Healthcheck,
    StoreStarting,
    StoreInitializing,
    Ready,
    Degraded,
}

impl ServerStatus {
    pub fn next(&self) -> Self {
        match self {
            ServerStatus::Stopped => ServerStatus::Healthcheck,
            ServerStatus::Healthcheck => ServerStatus::StoreStarting,
            ServerStatus::StoreStarting => ServerStatus::StoreInitializing,
            ServerStatus::StoreInitializing => ServerStatus::Ready,
            ServerStatus::Ready => ServerStatus::Degraded,
            ServerStatus::Degraded => ServerStatus::Stopped,
        }
    }
}

/// Remote counting service.
#[rtc::remote]
pub trait Api {
    /// Clients should call this and wait for `ServerStatus::Ready` or `ServerStatus::Degraded`
    /// before making further Api calls.
    ///
    /// Returns a watch (channel) which will get notified when the server status changes. Even when
    /// there are no further changes, the current status will always be sent.
    async fn wait_ready(&mut self) -> RtcResult<rch::watch::Receiver<ServerStatus>>;

    async fn stream_log(&mut self) -> RtcResult<rch::broadcast::Receiver<LogLine>>;

    // async fn stream_log(
    //     &mut self,
    //     machine_name: String,
    // ) -> RtcResult<rch::broadcast::Receiver<LogLine>>;
    //
    // async fn value(&self) -> RtcResult<u32>;
    // async fn watch(&mut self) -> RtcResult<rch::watch::Receiver<u32>>;
    // async fn increase(&mut self, by: u32) -> RtcResult<()>;
    // async fn count_to_value( &self, step: u32, delay: Duration,) -> RtcResult<rch::mpsc::Receiver<u32>>;
}
