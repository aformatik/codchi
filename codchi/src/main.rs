use std::env;

use anyhow::Result;
use clap::*;
use log::*;
use shadow_rs::shadow;

mod cli;
mod config;
mod consts;
mod controller;
mod data;
mod driver;
shadow!(build);

use crate::cli::Cli;

fn command() -> Command {
    Cli::command() //
        .long_version(build::CLAP_LONG_VERSION)
}

fn main() -> Result<()> {
    human_panic::setup_panic!();

    let cli = {
        let res = Cli::from_arg_matches(&command().get_matches()) //
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

    match &cli.command {
        cli::Cmd::Controller(ctrl_cmd) => cli.controller(ctrl_cmd),
        cli::Cmd::Rebuild {} => todo!(),
        cli::Cmd::Status {} => todo!(),
    }
}
