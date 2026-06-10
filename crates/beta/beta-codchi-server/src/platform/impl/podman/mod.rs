// mod lxd;
mod podman;
mod shell;
mod store;

use ipc::health::HealthCheck;
use super::Virtualization;

pub use store::StoreImpl;

pub struct VirtualizationImpl;

impl VirtualizationImpl {
    pub fn new() -> VirtualizationImpl {
        VirtualizationImpl
    }
}

impl Virtualization for VirtualizationImpl {

    fn check_os_health(&self) -> HealthCheck {
        HealthCheck::Ok
    }

    fn check_virtualization_health(&self) -> HealthCheck {
        // let info = podman::info()
        //     .map_err(|err| format!("Failed to execute the podman command. Reason: {err}"))?;
        // tracing::debug!("Got podman info: {info:?}");
        HealthCheck::Ok
    }
}

// pub struct HostImpl;
//
// impl HostImpl {
//     pub fn new() -> Self {
//         Self
//     }
// }
//
// impl Host for HostImpl {}
