//! Compile-time selected platform drivers.

mod store;

#[cfg(unix)]
mod podman;

pub use store::{Store, StoreError, StoreLogStream, StorePlatformStatus};

#[cfg(unix)]
pub use podman::PodmanStore;
