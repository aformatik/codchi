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

pub mod nix {
    use crate::cli::ModuleAttrPath;

    use super::*;
    use serde_json::Value;
    use std::{io, process::ExitStatus};
    use thiserror::Error;

    #[derive(Error, Debug)]
    pub enum Error {
        #[error("Failed to call Nix.")]
        IO(#[from] io::Error),

        #[error("Nix eval didn't find attribute.")]
        EvalMissingAttr,

        // TODO: link to docs
        #[error("SSL peer certificate or SSH remote key was not OK.")]
        InvalidRemoteSSLOrSSH,

        // TODO: link to docs
        #[error("Couldn't access repository at '{url}'. If the repository is private you need to provide the correct credentials.")]
        InvalidURLOrCredentials { url: String },

        #[error("Failed parsing JSON output from Nix")]
        JSON(#[from] serde_json::Error),

        #[error("Nix {args:?} failed with exit status {exit_status:?}. Stderr was:\n{stderr}")]
        Other {
            args: String,
            exit_status: ExitStatus,
            stderr: String,
        },
    }
    pub type Result<T> = std::result::Result<T, Error>;

    fn output_to_result(args: &[&str], url: Option<&str>, output: Output) -> Result<Vec<u8>> {
        if output.status.success() {
            Ok(output.stdout)
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            if stderr.contains("SSL peer certificate or SSH remote key was not OK") {
                Err(Error::InvalidRemoteSSLOrSSH)
            } else if url.is_some()
                && /* http / ssh      */ (stderr.contains("program 'git' failed with exit code 128")
                || /* gitlab / github */ stderr.contains("HTTP error 404")
                || /* srht            */ stderr.contains("HTTP error 403"))
            {
                Err(Error::InvalidURLOrCredentials {
                    url: url.unwrap().to_string(),
                })
            } else if stderr.contains("does not provide attribute") {
                Err(Error::EvalMissingAttr)
            } else {
                Err(Error::Other {
                    args: format!("{args:?}"),
                    exit_status: output.status,
                    stderr: stderr.to_string(),
                })
            }
            // codchi init flow
            //  -> fetch module flow
            //  -> add another module?
            //  -> build now? (as controller job? => progress?)
            // codchi add module flow
            //  -> fetch module flow
        }
    }

    impl<T: Driver> NixDriver for T {}
    pub trait NixDriver: Driver {
        fn list_nixos_modules(&self, url: &str) -> Result<Vec<ModuleAttrPath>> {
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
                let json = match output_to_result(
                    &args,
                    Some(&url),
                    self.ctrl_cmd_output("nix", &args)?,
                ) {
                    Err(Error::EvalMissingAttr) => Ok("[]".as_bytes().to_vec()),
                    res => res,
                }?;

                serde_json::from_slice::<Vec<String>>(&json).map_err(Error::JSON)
            };
            let modules = list_attr_names("codchiModules")?
                .iter()
                .map(|module| ModuleAttrPath {
                    base: "codchiModules".to_string(),
                    module: module.to_string(),
                })
                .chain(
                    list_attr_names("nixosModules")?
                        .iter()
                        .map(|module| ModuleAttrPath {
                            base: "nixosModules".to_string(),
                            module: module.to_string(),
                        }),
                )
                .collect();
            Ok(modules)
        }

        fn has_nixpkgs_input(&self, url: &str) -> Result<bool> {
            let args = ["flake", "metadata", "--json", "--no-write-lock-file", url];
            let output = self.ctrl_cmd_output("nix", &args)?;

            let metadata: Value =
                serde_json::from_slice(&output_to_result(&args, Some(&url), output)?)?;

            Ok(metadata
                .get("locks")
                .and_then(|value| value.get("nodes"))
                .and_then(|value| value.get("nixpkgs"))
                .is_some())
        }
    }
}
