use clap::*;

/// codchi
#[derive(Debug, Parser)]
#[command(
    version, author, about, long_about = None, 
    infer_subcommands = true
)]
pub struct Cli {
    #[command(flatten)]
    pub verbose: clap_verbosity_flag::Verbosity,

    #[command(subcommand)]
    pub command: Cmd,
}

#[derive(Debug, Subcommand)]
pub enum Cmd {
    #[command(subcommand)]
    Controller (ControllerCmd),
    Status {},
    Rebuild {},
}

#[derive(Debug, Subcommand)]
pub enum ControllerCmd {
    /// Start the codchi controller
    Start {
        /// Run in foreground, dont daemonize
        #[arg(long="foreground", short='f')] 
        run_in_foreground: bool,
    },

    /// Stop the codchi controller
    Stop {},
}
