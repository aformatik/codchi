// use host::Host;
use ipc::service::Healthcheck;
use std::sync::LazyLock;
use store::Store;

#[cfg_attr(target_os = "linux", path = "impl/linux-lxd/mod.rs")]
#[cfg_attr(target_os = "windows", path = "impl/windows/mod.rs")]
mod implementation;

pub mod shell;
// pub mod host;
pub mod store;

static PLATFORM: LazyLock<PlatformRegistry> = LazyLock::new(|| PlatformRegistry {
    virtualisation: implementation::VirtualisationImpl::new(),
    // host: implementation::HostImpl::new(),
    store: implementation::StoreImpl::new(),
});

pub struct PlatformRegistry {
    virtualisation: implementation::VirtualisationImpl,
    store: implementation::StoreImpl,
    // host: implementation::HostImpl,
}

/// The entry point into platform specific APIs
impl PlatformRegistry {
    pub fn virtualisation() -> &'static impl Virtualisation {
        &PLATFORM.virtualisation
    }

    pub fn store() -> &'static impl Store {
        &PLATFORM.store
    }

    // pub fn host() -> &'static impl Host {
    //     &PLATFORM.host
    // }
}

pub trait Virtualisation {
    fn healthcheck(&self) -> Healthcheck;
}
