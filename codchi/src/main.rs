use anyhow::Result;
use clap::*;
use log::*;
use shadow_rs::shadow;
use std::env;

mod cli;
mod config;
mod consts;
mod controller;
mod data;
mod driver;
shadow!(build);

fn command() -> Command {
    cli::Cli::command() //
        .long_version(build::CLAP_LONG_VERSION)
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
            cli::ControllerCmd::Start { run_in_foreground } => controller::start(run_in_foreground),
            cli::ControllerCmd::Stop {} => controller::stop(),
        },
        cli::Cmd::Rebuild {} => todo!(),
        cli::Cmd::Status {} => todo!(),
    }
}
