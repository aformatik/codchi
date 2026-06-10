use futures_signals::signal::Mutable;
use ipc::health::{Health, HealthCheck, HealthTopic};
use ipc::service::*;
use remoc::rch::broadcast;
use std::sync::Arc;
use tokio::sync::{RwLock, RwLockWriteGuard};
use ipc::logging::LogLine;

#[derive(Debug, Clone)]
pub struct ServerState {
    pub health: Health,
    pub log: broadcast::Sender<LogLine>,
    pub status: Mutable<ServerStatus>,
    pub machines: scc::HashMap<String, MachineState>,
}

impl ServerState {
    pub fn new() -> Arc<RwLock<Self>> {
        let (log_tx, _) = broadcast::channel::<_, _, 5>(5);
        Arc::new(RwLock::new(Self {
            health: Default::default(),
            status: Default::default(),
            log: log_tx,
            machines: Default::default(),
        }))
    }
}

#[derive(Debug, Clone)]
pub struct MachineState {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PlatformStatus {
    NotInstalled,
    Stopped,
    Running,
}

pub trait ServerStateOps {
    async fn write_lock(&self) -> RwLockWriteGuard<'_, ServerState>;

    async fn add_health_check(&self, topic: HealthTopic, check: HealthCheck) -> anyhow::Result<()> {
        let mut lock = self.write_lock().await;
        // Join error logs if both previous and current healt check on this topic are errors.
        // Else just overwrite the old one / insert the new one
        if let Some(previous_check) = lock.health.statuses.get_mut(&topic) {
            match (previous_check, check) {
                (
                    HealthCheck::Err {
                        last_errors: prev_errors,
                        ..
                    },
                    HealthCheck::Err { last_errors, .. },
                ) => {
                    for err in last_errors {
                        prev_errors.push(err)
                    }
                }
                (prev, cur) => *prev = cur,
            }
        } else {
            lock.health.statuses.insert(topic.clone(), check.clone());
        }
        lock.health.statuses.get(&topic).unwrap().clone().into()
    }

    async fn set_status(&self, status: ServerStatus) {
        self.write_lock().await.status.set(status)
    }
}

impl ServerStateOps for Arc<RwLock<ServerState>> {
    async fn write_lock(&self) -> RwLockWriteGuard<'_, ServerState> {
        self.write().await
    }
}
