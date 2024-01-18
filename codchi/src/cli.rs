use std::{fmt::Display, str::FromStr};

use clap::*;
use clap_verbosity_flag::{LogLevel, Verbosity, WarnLevel};
use git_url_parse::GitUrl;
use lazy_regex::regex_captures;
use once_cell::sync::OnceCell;

pub static CLI_ARGS: OnceCell<Cli> = OnceCell::new();

type DefaultLogLevel = WarnLevel;

/// codchi
#[derive(Debug, Parser, Clone)]
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

#[derive(Debug, Subcommand, Clone)]
pub enum Cmd {
    #[command(subcommand)]
    Controller(ControllerCmd),
    Status {},
    Rebuild {},

    /// Create a new code machine
    Init {
        /// Initialize this code machine without any modules
        #[arg(long, short)]
        empty: bool,

        #[command(flatten)]
        options: AddModuleOptions,
    },
}

#[derive(clap::Args, Debug, Clone, Default)]
#[clap(group(ArgGroup::new("ref").args(&["branch", "tag"])))] // branch an tag are mutually
                                                              // exclusive
pub struct AddModuleOptions {
    /// Don't prompt for confirmation and accept all of codchi's defaults
    #[arg(long, short = 'y')]
    pub accept_defaults: bool,

    // TODO examples gitlab / github (docs)
    /// Authorisation token for private repositories.
    #[arg(long, short)]
    pub token: Option<String>,

    /// Git branch
    #[arg(long, short)]
    pub branch: Option<String>,

    /// Git tag
    #[arg(long, short = 'r')]
    pub tag: Option<String>,

    /// Git commit
    #[arg(long, short)]
    pub commit: Option<String>,

    /// Name of the code machine
    pub name: String,

    // TODO link to docs
    /// http(s) url to the codchi module
    pub url: GitUrl,

    /// Path of the NixOS module you whish to add.
    /// Currently supported: 'codchiModules.<module>' or 'nixosModules.<module>'
    pub module_path: Option<ModuleAttrPath>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModuleAttrPath {
    /// codchiModules or nixosModules
    pub base: String,
    pub module: String,
}

impl Display for ModuleAttrPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.base, self.module)
    }
}

impl FromStr for ModuleAttrPath {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        match regex_captures!(
            r"^(nixosModules|codchiModules)\.([a-zA-Z\_][a-zA-Z0-9\_\'\-]*)$",
            string
        ) {
            Some((_, base, module)) => Ok(ModuleAttrPath {
                base: base.to_string(),
                module: module.to_string(),
            }),
            None => Err(
                "Please refer to a NixOS module as 'codchiModules.<name>' or 'nixosModules.<name>'"
                    .to_string(),
            ),
        }
    }
}

#[derive(Debug, Subcommand, Clone)]
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

impl Cmd {
    #[allow(dead_code)]
    pub fn to_args(&self) -> Vec<&'static str> {
        let global_args = CLI_ARGS.get().expect("Global CLI_ARGS not set.");
        let mut args = Vec::new();

        let default_lvl = <DefaultLogLevel as LogLevel>::default().unwrap() as i8;
        let log_level = global_args
            .verbose
            .log_level()
            .map(|it| it as i8)
            .unwrap_or(0);

        let rel_level = log_level - default_lvl;
        // println!("{default_lvl} {log_level} {rel_level} {:?}", self.verbose);

        #[allow(clippy::comparison_chain)]
        if rel_level > 0 {
            args.resize((args.len() as i8 + rel_level) as usize, "--verbose");
        } else if rel_level < 0 {
            args.resize((args.len() as i8 - rel_level) as usize, "--quiet");
        }

        match &self {
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
            Cmd::Init { .. } => args.push("init"),
        }

        args
    }
}
