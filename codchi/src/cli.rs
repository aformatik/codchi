use clap::*;
use clap_verbosity_flag::{LogLevel, Verbosity, WarnLevel};

type DefaultLogLevel = WarnLevel;

/// codchi
#[derive(Debug, Parser)]
#[command(
    version, author, about, long_about = None,
    infer_subcommands = true
)]
pub struct Cli {
    #[command(flatten)]
    pub verbose: Verbosity<DefaultLogLevel>,

    #[command(subcommand)]
    pub command: Cmd,
}

#[derive(Debug, Subcommand)]
pub enum Cmd {
    #[command(subcommand)]
    Controller(ControllerCmd),
    Status {},
    Rebuild {},
}

#[derive(Debug, Subcommand)]
pub enum ControllerCmd {
    /// Start the codchi controller
    Start {
        /// Run in foreground, dont daemonize
        #[arg(long = "foreground", short = 'f')]
        run_in_foreground: bool,
    },

    /// Stop the codchi controller
    Stop {},
}

impl Cli {
    #[allow(dead_code)]
    pub fn to_args(&self) -> Vec<&'static str> {
        let mut args = Vec::new();

        let default_lvl = <DefaultLogLevel as LogLevel>::default().unwrap() as i8;
        let log_level = self.verbose.log_level().map(|it| it as i8).unwrap_or(0);

        let rel_level = log_level - default_lvl;
        // println!("{default_lvl} {log_level} {rel_level} {:?}", self.verbose);

        #[allow(clippy::comparison_chain)]
        if rel_level > 0 {
            args.resize((args.len() as i8 + rel_level) as usize, "--verbose");
        } else if rel_level < 0 {
            args.resize((args.len() as i8 - rel_level) as usize, "--quiet");
        }

        match &self.command {
            Cmd::Controller(cmd) => {
                args.push("controller");
                match cmd {
                    ControllerCmd::Stop {} => args.push("stop"),
                    ControllerCmd::Start { run_in_foreground } => {
                        args.push("start");
                        if *run_in_foreground {
                            args.push("--foreground");
                        }
                    }
                }
            }
            Cmd::Status {} => args.push("status"),
            Cmd::Rebuild {} => args.push("rebuild"),
        }

        args
    }
}
