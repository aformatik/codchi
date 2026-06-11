//! Narrow platform-independent store driver boundary.

use codchi_shared::CommandError;

/// Store state as observed from the host platform.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum StorePlatformStatus {
    #[default]
    NotInstalled,
    Stopped,
    Running,
}

/// A store-driver failure suitable for lifecycle and health reporting.
///
/// Two shapes only: a `Message` for protocol/logic problems the driver detects
/// itself (an impossible state string, a missing image name), and `Command` for
/// any subprocess failure, which the shared [`CommandExt`](codchi_shared::CommandExt)
/// already models in full (spawn, non-zero exit with captured output, decode).
#[derive(Debug, thiserror::Error)]
pub enum StoreError {
    #[error("{0}")]
    Message(String),
    #[error(transparent)]
    Command(#[from] CommandError),
}

impl StoreError {
    pub fn new(message: impl Into<String>) -> Self {
        Self::Message(message.into())
    }
}

/// Platform operations needed to own the store lifecycle.
///
/// Methods are synchronous because they wrap host subprocesses. The
/// [`StoreManager`](crate::StoreManager) runs them on Tokio's blocking pool.
pub trait Store: Send + Sync + 'static {
    fn status(&self) -> Result<StorePlatformStatus, StoreError>;
    fn register(&self) -> Result<(), StoreError>;
    fn start(&self) -> Result<(), StoreError>;
    fn probe_health(&self) -> Result<(), StoreError>;
    fn stop(&self) -> Result<(), StoreError>;
}
