use anyhow::{Context, Result};
use clap::*;
use log::*;
use std::env;
use tarpc::context;

mod cli;
mod config;
mod consts;
mod controller;
mod data;
mod nix;
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
                controller::start(*run_in_foreground)?;
                if !run_in_foreground {
                    std::process::exit(0);
                }
                Ok(())
            }
            cli::ControllerCmd::Stop {} => controller::stop(),
        },
        cli::Cmd::Rebuild {} => todo!(),
        cli::Cmd::Status {} => {
            let status = controller::force_client(|client| async move {
                client
                    .get_status(context::current())
                    .await
                    .context("Failed to get status via RPC.")
            })?;
            println!("{status:#?}");
            // println!("{}", status);
            Ok(())
        }
    }
}
