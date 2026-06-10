use crate::platform::cmd::{LinuxUser, Program};
use crate::platform::shell::ShellDriver;
use shared::consts;
use std::process::{Command, Stdio};

#[derive(Clone, Debug)]
pub struct PodmanShellImpl {
    container_name: String,
}

impl PodmanShellImpl {
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

impl ShellDriver for PodmanShellImpl {
    fn build(&self, command: crate::platform::cmd::LinuxCommand) -> std::process::Command {
        let mut cmd = Command::new("podman");
        cmd.arg("exec");

        if let Some(cwd) = &command.cwd {
            cmd.args(["-w", &cwd.0]);
        }
        // if *DEBUG { TODO
        cmd.args(["-e", "CODCHI_DEBUG=1"]);
        // }
        if let Some(user) = &command.user {
            // Podman uses --user for combined user:group format
            let uid = match user {
                LinuxUser::Root => consts::user::ROOT_UID,
                LinuxUser::Default => consts::user::DEFAULT_UID,
            };
            let gid = match user {
                LinuxUser::Root => consts::user::ROOT_GID,
                LinuxUser::Default => consts::user::DEFAULT_GID,
            };
            cmd.args(["--user", &format!("{}:{}", uid, gid)]);
            // cmd.args([
            //     "--env",
            //     &format!(
            //         "HOME={}",
            //         match user {
            //             LinuxUser::Root => &consts::user::ROOT_HOME.0,
            //             LinuxUser::Default => &consts::user::DEFAULT_HOME.0,
            //         }
            //     ),
            // ]);
            // cmd.args(["-e", "DISPLAY=:0"]);
            // cmd.args([
            //     "-e",
            //     &format!("XAUTHORITY={}/.Xauthority", consts::user::DEFAULT_HOME.0),
            // ]);
        }
        for (name, val) in command.env {
            // should be already escaped / no escaping needed on linux
            cmd.args(["-e", &format!("{name}={val}")]);
        }

        cmd.arg(&self.container_name);

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
