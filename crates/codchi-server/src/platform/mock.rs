//! Non-Unix (Windows, dev-only) store driver.
//!
//! There is no rootless Podman on the Windows host, and the v1 Windows host
//! transport + store driver are unported (Phase 12/13). Until then the Windows
//! build still needs *a* [`Store`] so `codchi-server` compiles and its lifecycle
//! can be exercised: [`MockStore`] reports an always-installed, always-running,
//! always-healthy store and produces no log stream. The supervisor brings it
//! "up" without touching the host, so the daemon comes up `Ready` against
//! mock-backed domains rather than `Degraded`.

use super::{Store, StoreError, StoreLogStream, StorePlatformStatus};

/// A no-op store that always reports a healthy, running store.
#[derive(Debug, Default)]
pub struct MockStore;

impl MockStore {
    pub fn new() -> Self {
        Self
    }
}

impl Store for MockStore {
    fn status(&self) -> Result<StorePlatformStatus, StoreError> {
        Ok(StorePlatformStatus::Running)
    }

    fn register(&self) -> Result<(), StoreError> {
        Ok(())
    }

    fn start(&self) -> Result<(), StoreError> {
        Ok(())
    }

    fn probe_health(&self) -> Result<(), StoreError> {
        Ok(())
    }

    fn stop(&self) -> Result<(), StoreError> {
        Ok(())
    }

    fn attach(&self) -> Result<StoreLogStream, StoreError> {
        Ok(StoreLogStream::Empty)
    }
}
