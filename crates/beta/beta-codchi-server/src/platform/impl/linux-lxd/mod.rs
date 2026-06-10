mod lxd;
mod shell;
mod store;

use super::Virtualisation;
use std::process::Command;

pub use store::StoreImpl;

pub struct VirtualisationImpl;

impl VirtualisationImpl {
    pub fn new() -> Self {
        Self
    }
}

impl Virtualisation for VirtualisationImpl {
    fn healthcheck(&self) -> ipc::service::Healthcheck {
        Command::new("lxc")
            .arg("info")
            .spawn()
            .map_err(|err| err.to_string())?
            .wait()
            .map_err(|err| err.to_string())?;

        Ok(())
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
