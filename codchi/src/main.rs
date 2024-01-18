#![deny(unused_crate_dependencies)]

use anyhow::{Context, Result};
use clap::*;
use log::*;
use std::env;
use tarpc::context;

mod cli;
mod module;
mod consts;
mod ctrl;
mod nix;
mod util;
mod config;
mod platform;

fn command() -> Command {
    cli::Cli::command().long_version(format!(
        r"{}
commit={}
branch={}
dirty={}",
        env!("CARGO_PKG_VERSION"),
        env!("GIT_BRANCH"),
        env!("GIT_COMMIT"),
        env!("GIT_DIRTY")
    ))
}

fn main() -> Result<()> {
    human_panic::setup_panic!();

    let cli = {
        let res = cli::Cli::from_arg_matches(&command().get_matches()) //
            .map_err(|err| err.format(&mut command()));
        match res {
            Ok(r) => r,
            Err(e) => e.exit(),
        }
    };

    env_logger::Builder::new()
        .filter_level(cli.verbose.log_level_filter())
        .init();

    trace!("Started codchi with args: {:?}", cli);

    cli::CLI_ARGS
        .set(cli.clone())
        .expect("Only main is allowed to set CLI_ARGS.");

    match &cli.command {
        cli::Cmd::Controller(ctrl_cmd) => match ctrl_cmd {
            cli::ControllerCmd::Start { run_in_foreground } => {
                ctrl::start(*run_in_foreground)?;
                if !run_in_foreground {
                    std::process::exit(0);
                }
                Ok(())
            }
            cli::ControllerCmd::Stop {} => ctrl::stop(),
        },
        cli::Cmd::Rebuild {} => todo!(),
        cli::Cmd::Status {} => {
            let status = ctrl::force_client(|client| async move {
                client
                    .get_status(context::current())
                    .await
                    .context("Failed to get status via RPC.")
            })?;
            println!("{status:#?}");
            Ok(())
        }
        cli::Cmd::Init { empty, options } => {
            // let url = "github:aformatik/codchi";
            // let url = "gitlab:jhr/nixos-devenv?host=gitlab.aformatik.de";
            // let url = "github:htngr/broba-website";
            // let url = "git+https://code.huettinger.me/johannes/passwords";
            // let url = "sourcehut:~htngs/nix-colors";
            println!("{:?}", module::init_flow(*empty, &options)?);
            Ok(())
        }
    }
}
