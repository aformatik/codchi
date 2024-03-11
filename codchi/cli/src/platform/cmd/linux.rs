use self::platform::LinuxCommandDriver;

use super::*;
use anyhow::Context;
use std::fmt::Debug;
use std::io::Write;
use std::{
    path::Path,
    process::{Child, Stdio},
};

pub trait LinuxCommandTarget {
    fn build(&self, uid: &Option<LinuxUser>, cwd: &Option<String>) -> std::process::Command;

    fn get_driver(&self) -> LinuxCommandDriver;

    fn run(&self, program: &str, args: &[&str]) -> LinuxCommandBuilder {
        LinuxCommandBuilder {
            driver: self.get_driver(),
            program: Program::Run {
                program: program.to_string(),
                args: args.iter().map(|arg| arg.to_string()).collect(),
            },
            user: None,
            cwd: None,
            // output: Output::Collect,
        }
    }

    fn script(&self, script: String) -> LinuxCommandBuilder {
        LinuxCommandBuilder {
            driver: self.get_driver(),
            program: Program::Script(script),
            user: None,
            cwd: None,
            // output: Output::Collect,
        }
    }
}

#[derive(Clone)]
pub struct LinuxCommandBuilder {
    driver: LinuxCommandDriver,
    program: Program,
    user: Option<LinuxUser>,
    cwd: Option<String>,
    // output: Output,
}

#[derive(Debug, Clone)]
pub enum Program {
    Run { program: String, args: Vec<String> },
    Script(String),
}

#[derive(Debug, Clone)]
pub enum LinuxUser {
    Root,
    Default,
}

impl LinuxCommandBuilder {
    pub fn with_user(mut self, user: LinuxUser) -> Self {
        self.user = Some(user);
        self
    }

    pub fn with_cwd<P: AsRef<Path>>(mut self, cwd: P) -> Self {
        self.cwd = Some(
            cwd.as_ref()
                .to_str()
                .with_context(|| format!("Invalid UTF in cwd: {self:?}."))
                .unwrap()
                .to_string(),
        );
        self
    }
}

impl Into<Command> for LinuxCommandBuilder {
    fn into(self) -> Command {
        let mut cmd = self.driver.build(&self.user, &self.cwd);

        match &self.program {
            Program::Run { program, args } => {
                cmd.args(&["run", &program]);
                for arg in args.iter() {
                    cmd.arg(arg);
                }
            }
            Program::Script(_) => {
                cmd.arg("runin");
                cmd.stdin(Stdio::piped());
            }
        };
        cmd
    }
}

impl Debug for LinuxCommandBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cmd: Command = self.clone().into();
        f.debug_struct("LinuxCommandBuilder")
            .field("driver", &self.driver)
            .field("program", &self.program)
            .field("user", &self.user)
            .field("cwd", &self.cwd)
            .field("cmd", &cmd)
            .finish()
    }
}

impl CommandExt for LinuxCommandBuilder {
    fn spawn(&mut self, out_ty: OutputType) -> Result<Child> {
        let mut cmd: Command = self.clone().into();

        cmd.stdout(out_ty.clone());
        cmd.stderr(out_ty);

        let mut child = cmd.spawn()?;

        if let Program::Script(input) = &self.program {
            let mut stdin = child.stdin.take().unwrap();
            stdin.write_all(input.as_bytes())?;
        }

        Ok(child)
    }
}
