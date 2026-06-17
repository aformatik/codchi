//! Compile-time selected platform drivers.

mod store;

#[cfg(unix)]
mod podman;

#[cfg(not(unix))]
mod mock;

pub use store::{Store, StoreError, StoreLogStream, StorePlatformStatus};

#[cfg(unix)]
pub use podman::PodmanStore;

#[cfg(not(unix))]
pub use mock::MockStore;
