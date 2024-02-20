pub use self::module::*;

use clap::builder::*;
use clap::*;
use clap_verbosity_flag::{InfoLevel, LogLevel, Verbosity};
use git_url_parse::GitUrl;
use lazy_regex::regex_captures;
use log::Level;
use once_cell::sync::Lazy;
use std::{fmt::Display, str::FromStr, sync::OnceLock};

#[allow(dead_code)]
pub static CLI_ARGS: OnceLock<Cli> = OnceLock::new();
#[allow(dead_code)]
pub static DEBUG: Lazy<bool> = Lazy::new(|| {
    CLI_ARGS
        .get()
        .and_then(|cli| cli.verbose.log_level())
        .or(<DefaultLogLevel as LogLevel>::default())
        .unwrap()
        > Level::Debug
});

type DefaultLogLevel = InfoLevel;

/// codchi
#[derive(Debug, Parser, Clone)]
#[command(
    version, author, about, long_about = None,
    // infer_subcommands = true
    long_version = format!("v{}\n{}",
        option_env!("CARGO_PKG_VERSION").unwrap_or(""),
        option_env!("CODCHI_GIT_COMMIT").unwrap_or(""),
    ),
)]
pub struct Cli {
    #[command(flatten)]
    pub verbose: Verbosity<DefaultLogLevel>,

    /// Dont keep the controller running in background
    // #[arg(long, short = 's')]
    // pub stop_ctrl: bool,

    #[command(subcommand)]
    pub command: Option<Cmd>,
}

#[derive(Debug, Subcommand, Clone)]
pub enum Cmd {
    // #[command(subcommand)]
    // #[clap(aliases = &["ctrl"])]
    // Controller(ControllerCmd),
    Status {},

    /// Create a new code machine
    Init {
        /// Initialize this code machine without any modules
        #[arg(long, short)]
        empty: bool,

        #[command(flatten)]
        options: AddModuleOptions,
    },

    /// Execute (interactive) command inside machine.
    #[clap(aliases = &["run"])]
    Exec {
        /// Name of the code machine
        name: String,

        /// Command with arguments to run
        #[arg(trailing_var_arg = true)]
        cmd: Vec<String>,
    },

    /// Apply changes to a code machine
    Rebuild {
        /// Name of the code machine
        name: String,
    },

    /// Check for updates
    Update {
        /// Name of the code machine
        name: String,
    },

    /// Delete code machine with all associated files
    Delete {
        /// Don't prompt for confirmation and delete immediately
        #[arg(long)]
        im_really_sure: bool,

        /// Name of the code machine
        name: String,
    },

    /// Manage modules of code machines
    #[command(subcommand)]
    #[clap(aliases = &["mod"])]
    Module(ModuleCmd),
}

// mod ctrl {
//     use super::*;

//     #[derive(Debug, Subcommand, Clone)]
//     pub enum ControllerCmd {
//         /// Start the codchi controller
//         Start {
//             /// Run in foreground, dont daemonize
//             #[arg(long = "foreground", short = 'f')]
//             run_in_foreground: bool,
//         },

//         /// Stop the codchi controller
//         Stop {},
//     }

//     impl ControllerCmd {
//         #[allow(dead_code)]
//         pub fn to_args(&self) -> Vec<&'static str> {
//             let global_args = CLI_ARGS.get().expect("Global CLI_ARGS not set.");
//             let mut args = Vec::new();

//             let default_lvl = <DefaultLogLevel as LogLevel>::default().unwrap() as i8;
//             let log_level = global_args
//                 .verbose
//                 .log_level()
//                 .map(|it| it as i8)
//                 .unwrap_or(0);

//             let rel_level = log_level - default_lvl;
//             // println!("{default_lvl} {log_level} {rel_level} {:?}", self.verbose);

//             #[allow(clippy::comparison_chain)]
//             if rel_level > 0 {
//                 args.resize((args.len() as i8 + rel_level) as usize, "--verbose");
//             } else if rel_level < 0 {
//                 args.resize((args.len() as i8 - rel_level) as usize, "--quiet");
//             }

//             args.push("controller");
//             match self {
//                 ControllerCmd::Stop {} => args.push("stop"),
//                 ControllerCmd::Start { run_in_foreground } => {
//                     args.push("start");
//                     if *run_in_foreground {
//                         args.push("--foreground");
//                     }
//                 }
//             }

//             args
//         }
//     }
// }

mod module {
    pub use super::*;

    #[derive(Debug, Subcommand, Clone)]
    pub enum ModuleCmd {
        #[clap(aliases = &["ls"])]
        List {
            /// Name of the code machine
            name: String,
        },

        Add(AddModuleOptions),

        #[clap(aliases = &["rm", "remove"])]
        Delete {
            /// Name of the code machine
            name: String,

            /// Id of the module (You can list them with `codchi module ls NAME`)
            id: usize,
        },
        // /// Fetch module updates
        // Update {
        // /// Name of the code machine
        // name: String,
        // },
    }

    #[derive(clap::Args, Debug, Clone)]
    // branch an tag are mutually exclusive
    #[clap(group(ArgGroup::new("ref").args(&["branch", "tag"])))]
    pub struct AddModuleOptions {
        /// Don't prompt for confirmation and accept all of codchi's defaults
        #[arg(long, short = 'y')]
        pub dont_prompt: bool,

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

    // TODO this is only neccessary when we have named modules. Currently they are refered to by
    // their index
    // #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
    // pub struct CodchiName(pub String);

    // impl Display for CodchiName {
    //     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    //         f.write_str(&self.0)
    //     }
    // }

    // impl FromStr for CodchiName {
    //     type Err = String;

    //     fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
    //         if s != "__codchi" {
    //             Ok(CodchiName(s.to_string()))
    //         } else {
    //             Err("The name '__codchi' is a reserved keyword.".to_string())
    //         }
    //     }
    // }

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
}
