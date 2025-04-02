use crate::platform::{Platform, Virtualisation};
use ipc::service::*;
use remoc::rtc;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::instrument;

#[derive(Debug, Clone, Default)]
pub struct State {
    pub health: Option<Healthcheck>,
}

impl State {
    pub fn new() -> Arc<RwLock<Self>> {
        Arc::new(RwLock::new(Self::default()))
    }
}

#[rtc::async_trait]
impl Api for State {
    #[instrument]
    async fn ping(&mut self) -> RtcResult<Healthcheck> {
        let health = if let Some(health) = &self.health {
            health.clone()
        } else {
            let health = Platform::virtualisation().healthcheck();
            self.health = Some(health.clone());
            health
        };
        Ok(health)
    }
}
