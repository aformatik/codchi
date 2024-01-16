#[allow(clippy::module_inception)]
#[cfg_attr(target_os = "linux", path = "linux/mod.rs")]
#[cfg_attr(target_os = "windows", path = "windows/mod.rs")]
mod platform;

pub use self::platform::*;

use anyhow::Result;
use std::{io, path::PathBuf, process::Output};

/// The interface to a platform specific driver. This driver holds the nix store, runs the nix
/// daemon and executes nix commands
pub trait Driver {
    /// Checks prerequisits to run the controller, emits warnings and installs the controller (if not
    /// already installed). Implementations must be idempotent.
    fn init_controller(&self) -> Result<()>;

    /// Get local filepath to archive with controller rootfs. This might fetch from the internet or
    /// locate it in the packaged codchi (MSIX / ...)
    fn get_controller_fs(&self) -> Result<PathBuf>;

    fn ctrl_cmd_spawn(&self, program: &str, args: &[&str]) -> io::Result<()>;

    fn ctrl_cmd_output(&self, program: &str, args: &[&str]) -> io::Result<Output>;
}

impl<T: Driver> NixDriver for T {}
pub trait NixDriver: Driver {
    fn list_nixos_modules(&self, url: &str) -> io::Result<Vec<String>> {
        let list_attr_names = |attr_path: &str| {
            let args = [
                "eval",
                "--json",
                "--quiet",
                "--quiet",
                &format!("{}#{}", url, attr_path),
                "--apply",
                "builtins.attrNames",
            ];
            let output = self.ctrl_cmd_output("nix", &args)?;

            let json = if output.status.success() {
                Ok(output.stdout)
            } else {
                let stderr = String::from_utf8_lossy(&output.stderr);
                if stderr.contains("does not provide attribute") {
                    Ok("[]".as_bytes().to_vec())
                } else {
                    Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!(
                            "Nix {:?} failed with {:?}. Original error:\n{}",
                            args, output.status, stderr
                        ),
                    ))
                }
            }?;

            serde_json::from_slice::<Vec<String>>(&json).map_err(|err| {
                io::Error::new(
                    io::ErrorKind::Other,
                    format!("LXD info: failed to parse json: {}", err),
                )
            })
        };
        let mut modules = list_attr_names("codchiModules")?;
        modules.extend(list_attr_names("nixosModules")?);
        Ok(modules)
    }
}
