#![feature(once_cell_try)]
#![deny(unused_crate_dependencies)]

use crate::{
    cli::{Cli, Cmd},
    platform::{ConfigStatus, Driver, Machine, MachineDriver, PlatformStatus},
};
use base64::{prelude::BASE64_STANDARD, Engine};
use clap::*;
use colored as _;
use log::*;
use std::env;

pub mod cli;
pub mod config;
pub mod consts;
pub mod module;
pub mod platform;
pub mod util;

fn main() -> anyhow::Result<()> {
    human_panic::setup_panic!();

    let cli = Cli::parse();

    env_logger::Builder::new()
        .filter_level(cli.verbose.log_level_filter())
        .parse_env("CODCHI_LOG")
        .init();

    trace!("Started codchi with args: {:?}", cli);

    // CLI_ARGS
    //     .set(cli.clone())
    //     .expect("Only main is allowed to set CLI_ARGS.");

    let _ = Driver::store();

    match &cli.command.unwrap_or(Cmd::Status {}) {
        Cmd::Status {} => print_status(Machine::list()?),
        Cmd::Init { empty, options } => module::init(*empty, &options)?,
        Cmd::Rebuild { name } => Machine::build(name)?,
        Cmd::Update { name } => Machine::update(name)?,
        Cmd::Module(cmd) => match cmd {
            cli::ModuleCmd::List { name } => module::list(name)?,
            cli::ModuleCmd::Add(opts) => module::add(opts)?,
            cli::ModuleCmd::Delete { name, id } => module::delete(name, *id)?,
            cli::ModuleCmd::Update { name: _ } => todo!(),
        },
    }

    // TODO check for dirty machines and warn user

    Ok(())
}

fn print_status(machines: Vec<Machine>) {
    use comfy_table::*;
    let mut table = Table::new();
    table.load_preset(presets::UTF8_FULL).set_header(vec![
        Cell::new("Machine"),
        Cell::new("Status"),
        Cell::new("Running?"),
    ]);

    for machine in machines.iter() {
        table.add_row(vec![
            Cell::new(&machine.name),
            match machine.config_status {
                ConfigStatus::NotInstalled => Cell::new("Never built").fg(Color::DarkYellow),
                ConfigStatus::Modified => Cell::new("Modified").fg(Color::Yellow),
                ConfigStatus::UpdatesAvailable => Cell::new("Updates available").fg(Color::Yellow),
                ConfigStatus::UpToDate => Cell::new("Up to date").fg(Color::Green),
            },
            Cell::new(if machine.platform_status == PlatformStatus::Running {
                "✅"
            } else {
                "❌"
            }),
        ]);
    }

    println!("{table}");
}

#[allow(unused)]
pub fn echo_ca_certs() -> anyhow::Result<()> {
    let crts = rustls_native_certs::load_native_certs()?;
    for crt in crts {
        println!("-----BEGIN CERTIFICATE-----");
        for line in BASE64_STANDARD.encode(crt.as_ref()).as_bytes().chunks(64) {
            println!("{}", std::str::from_utf8(line).unwrap());
        }
        println!("-----END CERTIFICATE-----");
        break;
    }
    Ok(())
}
