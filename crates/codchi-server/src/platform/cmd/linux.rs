// use shared::cmd::{CommandExt, OutputType};
use shared::util::LinuxPath;
use std::collections::HashMap;
use std::fmt::Debug;
// use std::io::Write;
// use std::process::{Child, Command, Stdio};

#[derive(Clone)]
pub struct LinuxCommand {
    pub program: Program,
    pub user: Option<LinuxUser>,
    pub cwd: Option<LinuxPath>,
    pub env: HashMap<String, String>, // output: Output,
}

#[derive(Debug, Clone)]
pub enum Program {
    Run { program: String, args: Vec<String> },
    Raw { program: String, args: Vec<String> },
    Script(String),
}

#[derive(Debug, Clone)]
pub enum LinuxUser {
    Root,
    Default,
}

impl LinuxCommand {
    pub fn run(program: &str, args: &[&str]) -> LinuxCommand {
        LinuxCommand {
            program: Program::Run {
                program: program.to_string(),
                args: args.iter().map(|arg| arg.to_string()).collect(),
            },
            user: None,
            cwd: None,
            env: HashMap::new(), // output: Output::Collect,
        }
    }

    pub fn raw(program: &str, args: &[&str]) -> LinuxCommand {
        LinuxCommand {
            program: Program::Raw {
                program: program.to_string(),
                args: args.iter().map(|arg| arg.to_string()).collect(),
            },
            user: None,
            cwd: None,
            env: HashMap::new(), // output: Output::Collect,
        }
    }

    pub fn script(script: String) -> LinuxCommand {
        LinuxCommand {
            program: Program::Script(script),
            user: None,
            cwd: None,
            env: HashMap::new(), // output: Output::Collect,
        }
    }

    pub fn with_user(mut self, user: LinuxUser) -> Self {
        self.user = Some(user);
        self
    }

    pub fn with_cwd(mut self, cwd: LinuxPath) -> Self {
        self.cwd = Some(cwd);
        self
    }

    pub fn with_env(mut self, env: HashMap<String, String>) -> Self {
        self.env = env;
        self
    }
}

// impl From<LinuxCommand> for Command {
//     fn from(val: LinuxCommand) -> Self {
//         let mut cmd = val.driver.build(&val.user, &val.cwd, &val.env);
//
//         match &val.program {
//             Program::Run { program, args } => {
//                 cmd.args(["run", program]);
//                 for arg in args.iter() {
//                     cmd.arg(arg);
//                 }
//             }
//             Program::Script(_) => {
//                 cmd.arg("runin");
//                 cmd.stdin(Stdio::piped());
//             }
//             Program::Raw { program, args } => {
//                 cmd.arg(program);
//                 for arg in args.iter() {
//                     cmd.arg(arg);
//                 }
//             }
//         };
//         cmd
//     }
// }

// impl Debug for LinuxCommand {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let cmd: Command = self.clone().into();
//         f.debug_struct("LinuxCommandBuilder")
//             // .field("driver", &self.driver)
//             .field("program", &self.program)
//             .field("user", &self.user)
//             .field("cwd", &self.cwd)
//             .field("cmd", &cmd)
//             .finish()
//     }
// }

// impl CommandExt for LinuxCommand {
//     fn spawn(&mut self, out_ty: OutputType) -> shared::cmd::Result<Child> {
//         let mut cmd: Command = self.clone().into();
//
//         cmd.stdout(out_ty.clone());
//         cmd.stderr(out_ty);
//
//         let mut child = cmd.spawn()?;
//
//         if let Program::Script(input) = &self.program {
//             let mut stdin = child.stdin.take().unwrap();
//             stdin.write_all(input.as_bytes())?;
//         }
//
//         Ok(child)
//     }
// }
