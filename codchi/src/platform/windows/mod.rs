use std::ffi::OsString;

use lazy_static::lazy_static;

use super::Driver;
// use crate::data::{CodchiHealth, CodchiStatus};

lazy_static! {
    pub static ref DRIVER: DriverImpl = DriverImpl {};
}

pub struct DriverImpl {}

impl Driver for DriverImpl {
    fn init_controller(&self) -> anyhow::Result<()> {
        let wsl = wslapi::Library::new()?;
        let cfg = wsl.get_distribution_configuration("codchi-controller")?;
        println!(
            "version: {}, uid: {}, flags: {:?}",
            cfg.version, cfg.default_uid, cfg.flags
        );

        let names = wslapi::registry::distribution_names();
        println!("{:#?}", names.collect::<OsString>());
        Ok(())
    }

    fn get_controller_fs(&self) -> anyhow::Result<std::path::PathBuf> {
        todo!()
    }

    fn ctrl_cmd_spawn(&self, program: &str, args: &[&str]) -> std::io::Result<()> {
        todo!()
    }

    fn ctrl_cmd_output(&self, program: &str, args: &[&str]) -> std::io::Result<std::process::Output> {
        todo!()
    }
    // fn controller_healthcheck(&self) -> ControllerHealth {
    //     let wsl = wslapi::Library::new()?;
    //     let cfg = wsl.get_distribution_configuration("codchi-controller")?;
    //     println!(
    //         "version: {}, uid: {}, flags: {:?}",
    //         cfg.version, cfg.default_uid, cfg.flags
    //     );

    //     let names = wslapi::registry::distribution_names();
    //     println!("{:#?}", names.collect::<OsString>());

    //     todo!()
    // }
}
