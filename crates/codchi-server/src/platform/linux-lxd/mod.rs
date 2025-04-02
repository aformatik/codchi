use std::process::Command;

use super::Virtualisation;

pub struct VirtualisationImpl;

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
