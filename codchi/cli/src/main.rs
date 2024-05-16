#![feature(once_cell_try)]
#![deny(unused_crate_dependencies)]
// #![windows_subsystem = "windows"]

use crate::{
    cli::{Cli, Cmd, CLI_ARGS},
    platform::{ConfigStatus, Driver, Machine, PlatformStatus, Store},
};
use anyhow::Context;
use base64::{prelude::BASE64_STANDARD, Engine};
use clap::*;
use colored as _;
use indicatif::MultiProgress;
use indicatif_log_bridge::LogWrapper;
use log::*;
use std::{env, sync::OnceLock};

pub mod cli;
pub mod config;
pub mod consts;
pub mod module;
pub mod platform;
pub mod util;

pub static ROOT_PROGRESS_BAR: OnceLock<MultiProgress> = OnceLock::new();

fn main() -> anyhow::Result<()> {
    not_so_human_panic::setup_panic!();

    let cli = Cli::parse();

    #[cfg(target_os = "windows")]
    {
        use windows::Win32::{
            System::Console::GetConsoleWindow,
            UI::WindowsAndMessaging::{self, ShowWindow},
        };
        if cli.terminal == Some(false) {
            let window = unsafe { GetConsoleWindow() };
            if window.0 != 0 {
                if let Err(err) = unsafe { ShowWindow(window, WindowsAndMessaging::SW_HIDE).ok() } {
                    log::error!("Failed to hide console window. Reason: {err}");
                }
                println!("");
            }
        }
    }

    let logger = env_logger::Builder::new()
        .filter_level(cli.verbose.log_level_filter())
        .parse_env("CODCHI_LOG")
        .build();
    let root_pb = ROOT_PROGRESS_BAR.get_or_init(MultiProgress::new);
    LogWrapper::new(root_pb.clone(), logger)
        .try_init()
        .context("Failed initializing logger")?;

    trace!("Started codchi with args: {:?}", cli);

    CLI_ARGS
        .set(cli.clone())
        .expect("Only main is allowed to set CLI_ARGS.");

    let _ = Driver::store();

    match &cli.command.unwrap_or(Cmd::Status {}) {
        Cmd::Status {} => print_status(Machine::list()?),
        Cmd::Init {
            machine_name,
            url,
            options,
        } => alert_dirty(module::init(machine_name, url, options)?),
        Cmd::Rebuild { no_update, name } => Machine::by_name(name)?.build(*no_update)?,
        Cmd::Update { name } => alert_dirty(Machine::by_name(name)?.update()?),
        Cmd::Exec { name, cmd } => Machine::by_name(name)?.exec(cmd)?,
        Cmd::Delete {
            name,
            i_am_really_sure,
        } => Machine::by_name(name)?.delete(*i_am_really_sure)?,
        Cmd::Module(cmd) => match cmd {
            cli::ModuleCmd::List { name } => module::list(name)?,
            cli::ModuleCmd::Add {
                machine_name,
                options,
                url,
            } => alert_dirty(module::add(machine_name, url, options)?),
            cli::ModuleCmd::Set {
                machine_name,
                module_name,
                url,
                options,
            } => alert_dirty(module::set(machine_name, module_name, url, options)?),
            cli::ModuleCmd::Delete { name, module_name } => {
                alert_dirty(module::delete(name, module_name.clone())?)
            }
        },
        Cmd::GC {
            older_than,
            all,
            machines,
        } => Driver::store().gc(older_than.map(|x| x.unwrap_or_default()), *all, machines)?,
    }

    Ok(())
}

fn alert_dirty(machine: Machine) {
    match machine.config_status {
        ConfigStatus::NotInstalled => {
            println!(
                "{} is not installed yet. Install with `codchi rebuild {}`",
                machine.config.name, machine.config.name
            );
        }
        ConfigStatus::Modified => {
            println!(
                "{} was modified. Apply changes with `codchi rebuild {}`",
                machine.config.name, machine.config.name
            );
        }
        ConfigStatus::UpdatesAvailable => {
            println!(
                "{} has been updated upstream. Update with `codchi rebuild {}`",
                machine.config.name, machine.config.name
            );
        }
        ConfigStatus::UpToDate => {
            println!("Everything up to date!");
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
            Cell::new(&machine.config.name),
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
        // break;
    }
    Ok(())
}
