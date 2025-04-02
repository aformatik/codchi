use std::sync::LazyLock;

use ipc::service::Healthcheck;

#[allow(clippy::module_inception)]
#[cfg_attr(target_os = "linux", path = "linux-lxd/mod.rs")]
#[cfg_attr(target_os = "windows", path = "windows/mod.rs")]
pub mod platform;

use platform::*;

static PLATFORM: LazyLock<Platform> = LazyLock::new(|| Platform {
    virtualisation: VirtualisationImpl {},
});

pub struct Platform {
    virtualisation: VirtualisationImpl,
}
/// The entry point into platform specific APIs
impl Platform {
    pub fn virtualisation() -> &'static impl Virtualisation {
        &PLATFORM.virtualisation
    }
}

pub trait Virtualisation {
    fn healthcheck(&self) -> Healthcheck;
}
