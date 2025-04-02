use futures_signals::{signal::Mutable, signal_vec::MutableVec};
use ipc::service::*;
use remoc::rch::broadcast;
use shared::util::UtilExt;
use std::sync::{Arc, LazyLock};
use tokio::sync::RwLock;

pub static DEBUG: LazyLock<bool> = LazyLock::new(|| false);

#[derive(Debug, Clone)]
pub struct ServerState {
    pub health: Option<Healthcheck>,
    pub messages: MutableVec<LogLine>,

    pub status: Mutable<ServerStatus>,

    // pub store: StoreState,
    pub store_init_log: broadcast::Sender<LogLine>,

    pub machines: scc::HashMap<String, MachineState>,
}

impl ServerState {
    pub fn new() -> Arc<RwLock<Self>> {
        let (store_log_tx, _) = broadcast::channel::<_, _, 5>(5);
        Arc::new(RwLock::new(Self {
            health: Default::default(),
            messages: Default::default(),
            status: Default::default(),
            store_init_log: store_log_tx,
            machines: Default::default(),
        }))
    }
}

#[derive(Debug, Clone)]
pub struct MachineState {
    pub init_log: broadcast::Sender<LogLine>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PlatformStatus {
    NotInstalled,
    Stopped,
    Running,
}

pub trait HasLogger: Send + Sync + 'static {
    fn log(&self, level: LogLevel, topic: Option<String>, text: String);
}

impl HasLogger for broadcast::Sender<LogLine> {
    fn log(&self, level: LogLevel, topic: Option<String>, text: String) {
        self.send(LogLine { level, topic, text }).ignore();
    }
}

impl HasLogger for MutableVec<LogLine> {
    fn log(&self, level: LogLevel, topic: Option<String>, text: String) {
        self.lock_mut().push_cloned(LogLine { level, topic, text });
    }
}
