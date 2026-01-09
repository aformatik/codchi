// use host::Host;
use ipc::health::HealthCheck;
use std::sync::LazyLock;
use store::Store;

#[cfg_attr(target_os = "linux", path = "impl/podman/mod.rs")]
#[cfg_attr(target_os = "windows", path = "impl/windows/mod.rs")]
mod implementation;

pub mod shell;
// pub mod host;
pub mod store;
pub mod cmd;
pub mod logging;

static PLATFORM: LazyLock<PlatformRegistry> = LazyLock::new(|| PlatformRegistry {
    virtualization: implementation::VirtualizationImpl::new(),
    // host: implementation::HostImpl::new(),
    store: implementation::StoreImpl::new(),
});

pub struct PlatformRegistry {
    virtualization: implementation::VirtualizationImpl,
    store: implementation::StoreImpl,
    // host: implementation::HostImpl,
}

/// The entry point into platform specific APIs
impl PlatformRegistry {
    pub fn virtualization() -> &'static impl Virtualization {
        &PLATFORM.virtualization
    }

    pub fn get_store() -> &'static impl Store {
        &PLATFORM.store
    }

    // pub fn host() -> &'static impl Host {
    //     &PLATFORM.host
    // }
}

pub trait Virtualization {
    fn check_os_health(&self) -> HealthCheck;
    fn check_virtualization_health(&self) -> HealthCheck;
}
