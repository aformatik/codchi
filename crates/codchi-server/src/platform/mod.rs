//! Compile-time selected platform drivers.

mod store;

#[cfg(unix)]
mod podman;

pub use store::{Store, StoreError, StorePlatformStatus};

#[cfg(unix)]
pub use podman::PodmanStore;
