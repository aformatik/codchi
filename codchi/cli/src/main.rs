#![feature(once_cell_try)]
#![deny(unused_crate_dependencies)]

use crate::{
    cli::{Cli, Cmd, CLI_ARGS},
    platform::{ConfigStatus, Driver, Machine, PlatformStatus},
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

    CLI_ARGS
        .set(cli.clone())
        .expect("Only main is allowed to set CLI_ARGS.");

    let _ = Driver::store();

    match &cli.command.unwrap_or(Cmd::Status {}) {
        Cmd::Status {} => print_status(Machine::list()?),
        Cmd::Init { empty, options } => alert_dirty(module::init(*empty, &options)?),
        Cmd::Rebuild { name } => Machine::by_name(name)?.build()?,
        Cmd::Update { name } => alert_dirty(Machine::by_name(name)?.update()?),
        Cmd::Exec { name, cmd } => Machine::by_name(name)?.exec(cmd)?,
        Cmd::Delete {
            name,
            im_really_sure,
        } => Machine::by_name(name)?.delete(*im_really_sure)?,
        Cmd::Module(cmd) => match cmd {
            cli::ModuleCmd::List { name } => module::list(name)?,
            cli::ModuleCmd::Add(opts) => alert_dirty(module::add(opts)?),
            cli::ModuleCmd::Delete { name, id } => alert_dirty(module::delete(name, *id)?),
        },
    }

    // TODO check for dirty machines and warn user

    Ok(())
}

fn alert_dirty(machine: Machine) {
    match machine.config_status {
        ConfigStatus::NotInstalled => {
            info!(
                "{} is not installed yet. Install with `codchi rebuild {}`",
                machine.name, machine.name
            );
        }
        ConfigStatus::Modified => {
            info!(
                "{} was modified. Apply changes with `codchi rebuild {}`",
                machine.name, machine.name
            );
        }
        ConfigStatus::UpdatesAvailable => {
            info!(
                "{} has been updated upstream. Update with `codchi rebuild {}`",
                machine.name, machine.name
            );
        }
        ConfigStatus::UpToDate => {
            info!("Everything up to date!");
        }
    }
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
                ConfigStatus::NotInstalled => Cell::new("Not installed yet").fg(Color::Red),
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
