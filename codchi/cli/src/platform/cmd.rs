use std::{
    io,
    process::{exit, ExitStatus},
    str::FromStr,
};
use thiserror::Error;

use crate::util::UtilExt;

use super::*;
use anyhow::{anyhow, Context};
use serde::Deserialize;
use std::{
    io::Write,
    path::Path,
    process::{Child, Stdio},
};

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to call command.")]
    IO(#[from] io::Error),

    #[error("Failed parsing JSON output.")]
    JSON(#[from] serde_json::Error),

    #[error("Failed parsing output string.")]
    Parse(#[from] anyhow::Error),

    #[error("{cmd:?} failed with exit status {exit_status:?}. Stderr was:\n{stderr}")]
    Other {
        cmd: Command,
        exit_status: ExitStatus,
        stderr: String,
    },
}
type Result<T> = std::result::Result<T, Error>;

fn to_result(cmd: Command, output: std::process::Output) -> Result<Vec<u8>> {
    if output.status.success() {
        Ok(output.stdout)
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        log::trace!("Got error when running {cmd:?}:\n{stderr}");
        Err(Error::Other {
            cmd,
            exit_status: output.status,
            stderr,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Command {
    pub program: Program,
    pub user: Option<LinuxUser>,
    pub cwd: Option<String>,
    pub output: Output,
}

#[derive(Debug, Clone)]
pub enum Program {
    // Raw { program: String, args: Vec<String> },
    Run { program: String, args: Vec<String> },
    Script(String),
}

#[derive(Debug, Clone)]
pub enum LinuxUser {
    Root,
    Default,
}

#[derive(Debug, Clone)]
pub enum Output {
    Inherit,
    Collect,
    Discard,
}

impl Command {
    pub fn new(program: &str, args: &[&str]) -> Self {
        Self {
            program: Program::Run {
                program: program.to_string(),
                args: args.iter().map(|arg| arg.to_string()).collect(),
            },
            user: None,
            cwd: None,
            output: Output::Collect,
        }
    }

    pub fn script(script: String) -> Self {
        Self {
            program: Program::Script(script),
            user: None,
            cwd: None,
            output: Output::Collect,
        }
    }

    pub fn user(mut self, user: LinuxUser) -> Self {
        self.user = Some(user);
        self
    }

    pub fn cwd<P: AsRef<Path>>(mut self, cwd: P) -> Self {
        self.cwd = Some(
            cwd.as_ref()
                .to_str()
                .with_context(|| format!("Invalid UTF in cwd: {self:?}."))
                .unwrap()
                .to_string(),
        );
        self
    }

    pub fn output(mut self, output: Output) -> Self {
        self.output = output;
        self
    }
}

pub trait CommandDriver {
    fn build(&self, uid: Option<LinuxUser>, cwd: Option<String>) -> std::process::Command;

    fn spawn(&self, spec: Command) -> Result<Child> {
        let mut cmd = self.build(spec.user, spec.cwd);
        // let mut cmd = std::process::Command::new("cat");

        let stdin = match spec.program {
            // Program::Raw { program, args } => {
            //     cmd.arg(program);
            //     for arg in args.iter() {
            //         cmd.arg(arg);
            //     }
            //     None
            // }
            Program::Run { program, args } => {
                cmd.args(&["run", &program]);
                for arg in args.iter() {
                    cmd.arg(arg);
                }
                None
            }
            Program::Script(script) => {
                cmd.arg("runin");
                cmd.stdin(Stdio::piped());
                Some(script)
            }
        };

        match spec.output {
            Output::Inherit => {
                cmd.stdout(Stdio::inherit());
                cmd.stderr(Stdio::inherit());
            }
            Output::Collect => {
                cmd.stdout(Stdio::piped());
                cmd.stderr(Stdio::piped());
            }
            Output::Discard => {
                cmd.stdout(Stdio::null());
                cmd.stderr(Stdio::null());
            }
        }

        log::trace!("Running CommandDriver command: {:?}", &cmd);

        let mut child = cmd.spawn()?;

        if let Some(input) = stdin {
            let mut stdin = child.stdin.take().unwrap();
            stdin.write_all(input.as_bytes())?;
        }

        Ok(child)
    }

    fn output(&self, spec: Command) -> Result<Vec<u8>> {
        let out = self.spawn(spec.clone())?.wait_with_output()?;
        to_result(spec, out).map(|out| {
            out.peek(|out| log::trace!("Got output:\n{}", String::from_utf8_lossy(&out)))
        })
    }

    fn run(&self, spec: Command) -> Result<()> {
        self.output(spec)?;
        Ok(())
    }

    fn exec(&self, spec: Command) -> Result<()> {
        exit(self.spawn(spec)?.wait()?.code().unwrap_or(1))
    }

    fn output_json<T>(&self, spec: Command) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let output = self.output(spec)?;
        Ok(serde_json::from_slice(&output)?)
    }

    fn output_from_str<T, Err>(&self, spec: Command) -> Result<T>
    where
        T: FromStr<Err = Err>,
        Err: std::fmt::Display,
    {
        let output = self.output(spec)?;
        T::from_str(std::str::from_utf8(&output).expect("Invalid UTF8"))
            .map_err(|err| Error::Parse(anyhow!("{err}")))
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
