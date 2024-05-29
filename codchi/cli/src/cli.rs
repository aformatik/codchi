pub use self::module::*;
use self::name::{ModuleName, CODCHI_DRIVER_MODULE};

use clap::builder::*;
use clap::*;
use clap_verbosity_flag::{InfoLevel, LogLevel, Verbosity};
use git_url_parse::{GitUrl, Scheme};
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
        >= Level::Debug
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

    /// Hide / show console window on windows (for GUI apps)
    #[cfg(target_os = "windows")]
    #[arg(long, hide = true, default_value = "true")]
    pub terminal: Option<bool>,

    #[command(subcommand)]
    pub command: Option<Cmd>,
}

#[derive(Debug, Subcommand, Clone)]
#[allow(clippy::large_enum_variant)] // can't use Box with flattened Option
pub enum Cmd {
    // #[command(subcommand)]
    // #[clap(aliases = &["ctrl"])]
    // Controller(ControllerCmd),
    ///

    /// List machines with their status
    Status {},

    /// Create a new code machine. <https://codchi.dev/docs/start/usage.html#creating-a-machine>
    Init {
        /// Name of the code machine
        machine_name: String,

        /// http(s) url to the codchi module
        url: Option<CodchiUrl>,

        #[command(flatten)]
        // #[group(requires = "url")]
        options: Box<ModuleOptions>,
    },

    /// Execute (interactive) command inside a machine.
    /// <https://codchi.dev/docs/start/usage.html#running-programs>
    #[clap(aliases = &["run"])]
    Exec {
        /// Name of the code machine
        name: String,

        /// Command with arguments to run
        #[arg(trailing_var_arg = true)]
        cmd: Vec<String>,
    },

    /// Apply changes to a code machine. By default this fetches updates for each module.
    /// <https://codchi.dev/docs/start/usage.html#applying-changes>
    Rebuild {
        /// Don't fetch module updates
        #[arg(long, short = 'n')]
        no_update: bool,

        /// Name of the code machine
        name: String,
    },

    /// Check for updates
    Update {
        /// Name of the code machine
        name: String,
    },

    /// Delete code machine with all associated files.
    /// <https://codchi.dev/docs/start/usage.html#uninstalling-a-machine>
    Delete {
        /// Don't prompt for confirmation and delete immediately
        #[arg(long)]
        i_am_really_sure: bool,

        /// Name of the code machine
        name: String,
    },

    /// Manage modules of code machines.
    /// <https://codchi.dev/docs/start/usage.html#managing-a-code-machine>
    #[command(subcommand)]
    #[clap(aliases = &["mod"])]
    Module(ModuleCmd),

    /// Perform garbage collection of old nix store paths.
    /// <https://codchi.dev/docs/start/usage.html#garbage-collection>
    #[clap(
        group(ArgGroup::new("exclusive").args(&["all", "machines"])),
    )]
    GC {
        /// Delete old machine generations. AGE is the minimum age in days to be deleted [default: 0]
        #[arg(long, short = 'd', value_name = "AGE")]
        older_than: Option<Option<u16>>,

        /// Process all machines (only works with `--older-than`)
        #[arg(long, short = 'a', requires = "older_than")]
        all: bool,

        /// Machines to be processed (only works with `--older-than`)
        #[arg(requires = "older_than")]
        machines: Vec<String>,
    },
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
    use self::name::ModuleName;

    pub use super::*;

    #[derive(Debug, Subcommand, Clone)]
    pub enum ModuleCmd {
        /// Lists modules of a machine <https://codchi.dev/docs/start/usage.html#modules>
        #[clap(aliases = &["ls"])]
        List {
            /// Name of the code machine
            name: String,
        },

        /// Adds a module to a machine <https://codchi.dev/docs/start/usage.html#adding-modules>
        Add {
            /// Name of the code machine
            machine_name: String,

            /// HTTP(S) URL or file path to the codchi module
            url: CodchiUrl,

            #[command(flatten)]
            options: Box<ModuleOptions>,
        },

        /// Modifies a module of a machine <https://codchi.dev/docs/start/usage.html#modifying-a-module>
        Set {
            /// Name of the code machine
            machine_name: String,

            /// Name of the module to modify
            module_name: ModuleName,

            /// HTTP(S) URL or file path to the codchi module
            url: Option<CodchiUrl>,

            #[command(flatten)]
            options: Box<ModuleOptions>,
        },

        /// Deletes a module of a machine <https://codchi.dev/docs/start/usage.html#delete-a-module>
        /// Note: This will NOT delete user data.
        #[clap(aliases = &["rm", "remove"])]
        Delete {
            /// Name of the code machine
            name: String,

            /// Name of the module (You can list them with `codchi module ls NAME`)
            module_name: ModuleName,
        },
        // /// Fetch module updates
        // Update {
        // /// Name of the code machine
        // name: String,
        // },
    }

    #[derive(clap::Args, Debug, Clone, Default, Eq, PartialEq)]
    // branch an tag are mutually exclusive
    #[clap(
        group(ArgGroup::new("exclusive").args(&["branch", "tag"])),
    )]
    pub struct ModuleOptions {
        /// Don't prompt for confirmation and accept all of codchi's defaults
        #[arg(long, short = 'y')]
        pub dont_prompt: bool,

        #[arg(long, short = 'p')]
        pub use_nixpkgs: Option<NixpkgsLocation>,

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

        /// Name of the module
        #[arg(long, short = 'n')]
        pub name: Option<ModuleName>,

        /// Path of the NixOS module you whish to add.
        /// Currently supported: 'codchiModules.<module>' or 'nixosModules.<module>'
        pub module_path: Option<ModuleAttrPath>,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
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

pub mod name {

    use super::*;
    use core::fmt;

    pub static CODCHI_DRIVER_MODULE: &str = "codchi_driver";

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
    pub struct ModuleName(pub String);

    impl Display for ModuleName {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str(&self.0)
        }
    }

    impl FromStr for ModuleName {
        type Err = String;

        fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
            if s != CODCHI_DRIVER_MODULE {
                Ok(ModuleName(s.to_string()))
            } else {
                Err("The name 'codchi_driver' is reserved.".to_string())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, ValueEnum)]
pub enum NixpkgsLocation {
    Local,
    Remote,
}
#[allow(dead_code)]
impl NixpkgsLocation {
    pub fn named(loc: &Option<NixpkgsLocation>, module_name: &ModuleName) -> Option<String> {
        match loc {
            Some(Self::Remote) => Some(module_name.to_string()),
            Some(Self::Local) => Some(CODCHI_DRIVER_MODULE.to_owned()),
            None => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CodchiUrl {
    pub git_url: GitUrl,
    pub original: String,
}

impl FromStr for CodchiUrl {
    type Err = <GitUrl as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let git_url = GitUrl::from_str(s)?;

        Ok(Self {
            git_url,
            original: s.to_owned(),
        })
    }
}

impl From<&CodchiUrl> for GitUrl {
    fn from(val: &CodchiUrl) -> Self {
        match val.git_url.scheme {
            Scheme::File => {
                let mut url = val.git_url.clone();
                url.path = val.original.clone();
                url
            },
            _ => val.git_url.clone(),
        }
    }
}
