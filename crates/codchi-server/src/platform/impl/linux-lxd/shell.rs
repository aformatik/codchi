use std::process::Stdio;

use shared::consts;

use crate::{
    cmd::{LinuxUser, Program},
    platform::shell::ShellDriver,
};

use super::lxd::lxc_command;

#[derive(Clone, Debug)]
pub struct LxdShellImpl {
    container_name: String,
}

impl LxdShellImpl {
    pub fn new_store() -> Self {
        Self {
            container_name: consts::CONTAINER_STORE_NAME.to_string(),
        }
    }
    pub fn new_machine(machine_name: &str) -> Self {
        Self {
            container_name: consts::machine::machine_name(machine_name),
        }
    }
}

impl ShellDriver for LxdShellImpl {
    fn build(&self, command: crate::cmd::LinuxCommand) -> std::process::Command {
        let mut cmd = lxc_command(&["exec", &self.container_name]);
        if let Some(cwd) = &command.cwd {
            cmd.args(["--cwd", &cwd.0]);
        }
        // if *DEBUG { TODO
        cmd.args(["--env", "CODCHI_DEBUG=1"]);
        // }
        if let Some(user) = &command.user {
            cmd.args([
                "--user",
                match user {
                    LinuxUser::Root => consts::user::ROOT_UID,
                    LinuxUser::Default => consts::user::DEFAULT_UID,
                },
            ]);
            cmd.args([
                "--group",
                match user {
                    LinuxUser::Root => consts::user::ROOT_GID,
                    LinuxUser::Default => consts::user::DEFAULT_GID,
                },
            ]);
            cmd.args([
                "--env",
                &format!(
                    "HOME={}",
                    match user {
                        LinuxUser::Root => &consts::user::ROOT_HOME.0,
                        LinuxUser::Default => &consts::user::DEFAULT_HOME.0,
                    }
                ),
            ]);
            cmd.args(["--env", "DISPLAY=:0"]);
            cmd.args([
                "--env",
                &format!("XAUTHORITY={}/.Xauthority", consts::user::DEFAULT_HOME.0),
            ]);
        }
        for (name, val) in command.env {
            // should be already escaped / no escaping needed on linux
            cmd.args(["--env", &format!("{name}={val}")]);
        }
        cmd.arg("--");

        match &command.program {
            Program::Run { program, args } => {
                cmd.args(["run", program]);
                for arg in args.iter() {
                    cmd.arg(arg);
                }
            }
            Program::Script(_) => {
                cmd.arg("runin");
                cmd.stdin(Stdio::piped());
            }
            Program::Raw { program, args } => {
                cmd.arg(program);
                for arg in args.iter() {
                    cmd.arg(arg);
                }
            }
        };
        cmd
    }

    fn quote_shell_arg(&self, arg: &str) -> String {
        arg.to_string()
    }
}
