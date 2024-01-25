#![deny(unused_crate_dependencies)]

use anyhow::Context;
use clap::*;
use colored::Colorize;
use log::*;
use std::env;
use tarpc::context;

use crate::cli::{Cli, Cmd, ControllerCmd, CLI_ARGS};

mod cli;
mod config;
mod consts;
mod ctrl;
mod module;
mod nix;
mod platform;
mod util;

fn real_main() -> anyhow::Result<()> {
    human_panic::setup_panic!();

    let cli = Cli::parse();

    env_logger::Builder::new()
        .filter_level(cli.verbose.log_level_filter())
        .init();

    trace!("Started codchi with args: {:?}", cli);

    CLI_ARGS
        .set(cli.clone())
        .expect("Only main is allowed to set CLI_ARGS.");

    match &cli.command.unwrap_or(Cmd::Status {}) {
        Cmd::Controller(ctrl_cmd) => match ctrl_cmd {
            ControllerCmd::Start { run_in_foreground } => {
                ctrl::start(*run_in_foreground)?;
                if !run_in_foreground {
                    std::process::exit(0);
                }
            }
            ControllerCmd::Stop {} => ctrl::stop()?,
        },
        Cmd::Status {} => {
            let status = ctrl::force_client(|client| async move {
                client
                    .get_status(context::current())
                    .await
                    .context("Failed to get status via RPC.")
            })?;
            println!("{status:#?}");
        }
        Cmd::Init { empty, options } => module::init(*empty, &options)?,
        Cmd::Rebuild { name } => todo!(),
        Cmd::Module(cmd) => match cmd {
            cli::ModuleCmd::List { name } => module::list(name)?,
            cli::ModuleCmd::Add(opts) => module::add(opts)?,
            cli::ModuleCmd::Delete { name, id } => module::delete(name, *id)?,
            cli::ModuleCmd::Update { name } => todo!(),
        },
    }

    // TODO check for dirty machines and warn user

    Ok(())
}

fn main() {
    match real_main() {
        Ok(()) => {}
        Err(err) => {
            eprintln!("{}: {}", "ERROR".red(), err);
            std::process::exit(1);
        }
    }
}
