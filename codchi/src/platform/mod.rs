#[allow(clippy::module_inception)]
#[cfg_attr(target_os = "linux", path = "linux/mod.rs")]
#[cfg_attr(target_os = "windows", path = "windows/mod.rs")]
mod platform;

pub use self::platform::*;

use anyhow::Result;
use std::path::PathBuf;

/// The interface to a platform specific driver. This driver holds the nix store, runs the nix
/// daemon and executes nix commands
pub trait Driver {
    /// Checks prerequisits to run the controller, emits warnings and installs the controller (if not
    /// already installed). Implementations must be idempotent.
    fn init_controller(&self) -> Result<()>;

    fn get_controller_fs(&self) -> Result<PathBuf>;
}
