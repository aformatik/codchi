use crate::platform::cmd::LinuxCommand;

/// Driver for issueing linux commands (either in the store or machine containers)
pub trait ShellDriver: Send {
    fn build(&self, command: LinuxCommand) -> std::process::Command;

    fn quote_shell_arg(&self, arg: &str) -> String;
}
