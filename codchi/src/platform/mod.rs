#[allow(clippy::module_inception)]
#[cfg_attr(target_os = "linux", path = "linux/mod.rs")]
#[cfg_attr(target_os = "windows", path = "windows/mod.rs")]
mod platform;

pub use self::cmd::*;
pub use self::nix::NixDriver;
pub use self::path::*;
pub use self::platform::*;

use anyhow::Result;
use std::{io, process::ExitStatus, process::Output};
use thiserror::Error;

/// The interface to a platform specific driver. This driver holds the nix store, runs the nix
/// daemon and executes nix commands
pub trait Driver {
    /// Checks prerequisits to run the controller, emits warnings and installs the controller (if not
    /// already installed). Implementations must be idempotent.
    fn init_controller(&self) -> Result<()>;

    fn ctrl_cmd(&self) -> impl CommandDriver + NixDriver + PathDriver;

    /// Name of NixOS driver
    fn internal_nixos_name(&self) -> &'static str;
}

pub mod path {
    use super::*;
    use std::path::PathBuf;

    #[derive(strum::Display, Clone)]
    #[strum(serialize_all = "snake_case")]
    pub enum PathBase {
        Machines,
        State,
        Nix,
    }

    pub trait PathDriver {
        fn resolve_root(&self) -> Result<PathBuf>;

        fn inner_to_outer(&self, base: PathBase) -> Result<PathBuf> {
            Ok(self.resolve_root()?.join(base.to_string()))
        }
    }
}

pub mod cmd {
    use super::*;
    use serde::Deserialize;
    use std::process::Stdio;

    #[derive(Error, Debug)]
    pub enum Error {
        #[error("Failed to call command.")]
        IO(#[from] io::Error),

        #[error("Failed parsing JSON output.")]
        JSON(#[from] serde_json::Error),

        #[error("{cmd:?} failed with exit status {exit_status:?}. Stderr was:\n{stderr}")]
        Other {
            cmd: Command,
            exit_status: ExitStatus,
            stderr: String,
        },
    }
    pub type Result<T> = std::result::Result<T, Error>;

    fn to_result(cmd: Command, output: Output) -> Result<Vec<u8>> {
        if output.status.success() {
            Ok(output.stdout)
        } else {
            Err(Error::Other {
                cmd,
                exit_status: output.status,
                stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            })
        }
    }

    #[derive(Debug, Clone)]
    pub struct Command {
        pub program: String,
        pub args: Vec<String>,
        pub uid: Option<usize>,
        pub cwd: Option<String>,
        pub silence_out: bool,
        pub silence_err: bool,
    }

    impl Command {
        pub fn new(program: &str, args: &[&str]) -> Self {
            Self {
                program: program.to_string(),
                args: args.iter().map(|arg| arg.to_string()).collect(),
                uid: None,
                cwd: None,
                silence_out: true,
                silence_err: true,
            }
        }

        // pub fn uid(mut self, uid: usize) -> Self {
        //     self.uid = Some(uid);
        //     self
        // }

        pub fn cwd(mut self, cwd: String) -> Self {
            self.cwd = Some(cwd);
            self
        }

        pub fn verbose(mut self) -> Self {
            self.silence_out = false;
            self.silence_err = false;
            self
        }
    }

    pub trait CommandDriver {
        fn build(spec: &Command) -> std::process::Command;

        fn spawn(&self, spec: Command) -> Result<()> {
            let mut cmd = Self::build(&spec);

            if spec.silence_out {
                cmd.stdout(Stdio::null());
            }
            if spec.silence_err {
                cmd.stderr(Stdio::null());
            }

            to_result(spec, cmd.spawn()?.wait_with_output()?)?;
            Ok(())
        }

        fn output_json<T>(&self, spec: Command) -> Result<T>
        where
            T: for<'de> Deserialize<'de>,
        {
            let mut cmd = Self::build(&spec);
            let output = to_result(spec, cmd.output()?)?;
            Ok(serde_json::from_slice(&output)?)
        }
    }
}

pub mod nix {
    use crate::cli::ModuleAttrPath;

    use super::*;
    use serde_json::Value;

    #[derive(Error, Debug)]
    pub enum Error {
        #[error("Nix eval didn't find attribute.")]
        EvalMissingAttr,

        // TODO: link to docs
        #[error("SSL peer certificate or SSH remote key was not OK.")]
        InvalidRemoteSSLOrSSH,

        // TODO: link to docs
        #[error("Couldn't access repository. If the repository is private you need to provide the correct credentials.")]
        InvalidURLOrCredentials,

        #[error("Nix command failed: {0}")]
        Command(super::cmd::Error),
    }
    pub type Result<T> = std::result::Result<T, Error>;

    impl From<cmd::Error> for Error {
        fn from(err: cmd::Error) -> Self {
            if let cmd::Error::Other { stderr, .. } = &err {
                if stderr.contains("SSL peer certificate or SSH remote key was not OK") {
                    Error::InvalidRemoteSSLOrSSH
                } else if
                // http / ssh
                stderr.contains("program 'git' failed with exit code 128")
                // gitlab / github
                || stderr.contains("HTTP error 404")
                // srht
                || stderr.contains("HTTP error 403")
                {
                    Error::InvalidURLOrCredentials
                } else if stderr.contains("does not provide attribute") {
                    Error::EvalMissingAttr
                } else {
                    Error::Command(err)
                }
            } else {
                Error::Command(err)
            }
        }
    }

    pub trait NixDriver: CommandDriver {
        fn list_nixos_modules(&self, url: &str) -> Result<Vec<ModuleAttrPath>> {
            let list_attr_names = |attr_path: &str| -> Result<Vec<String>> {
                let args = [
                    "eval",
                    "--json",
                    "--quiet",
                    "--quiet",
                    &format!("{}#{}", url, attr_path),
                    "--apply",
                    "builtins.attrNames",
                ];
                match self
                    .output_json::<Vec<String>>(Command::new("nix", &args))
                    .map_err(|err| err.into())
                {
                    Ok(attrs) => Ok(attrs),
                    Err(Error::EvalMissingAttr) => Ok(Vec::new()),
                    Err(err) => Err(err),
                }
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
            let metadata = self.output_json::<Value>(Command::new("nix", &args))?;

            Ok(metadata
                .get("locks")
                .and_then(|value| value.get("nodes"))
                .and_then(|value| value.get("nixpkgs"))
                .is_some())
        }
    }
}
