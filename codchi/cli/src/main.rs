#![feature(once_cell_try)]
#![feature(let_chains)]
#![deny(unused_crate_dependencies)]
// #![windows_subsystem = "windows"]

use crate::{
    cli::{Cli, Cmd, CLI_ARGS},
    platform::{ConfigStatus, Driver, Machine, PlatformStatus, Store},
};
use base64::{prelude::BASE64_STANDARD, Engine};
use clap::*;
use colored as _;
use config::CodchiConfig;
use git_url_parse::GitUrl;
use std::env;

pub mod cli;
pub mod config;
pub mod consts;
pub mod logging;
pub mod module;
pub mod platform;
pub mod util;

fn main() -> anyhow::Result<()> {
    not_so_human_panic::setup_panic!();

    let cli = Cli::parse();

    logging::init(cli.verbose.log_level_filter())?;

    #[cfg(target_os = "windows")]
    {
        // https://github.com/rust-lang/cargo/issues/1721
        use crate::util::dbg_duration;
        use windows::Win32::{
            System::Console::GetConsoleWindow,
            UI::WindowsAndMessaging::{self, ShowWindow},
        };
        if cli.terminal == Some(false) {
            dbg_duration("hide console", || {
                let window = unsafe { GetConsoleWindow() };
                if window.0 != 0 {
                    if let Err(err) =
                        unsafe { ShowWindow(window, WindowsAndMessaging::SW_HIDE).ok() }
                    {
                        log::error!("Failed to hide console window. Reason: {err}");
                    }
                    println!();
                }
            });
        }
    }

    log::trace!("Started codchi with args: {:?}", cli);
    // preload config
    CodchiConfig::get();

    CLI_ARGS
        .set(cli.clone())
        .expect("Only main is allowed to set CLI_ARGS.");

    let _ = Driver::store();

    match &cli.command.unwrap_or(Cmd::Status {}) {
        Cmd::Status {} => print_status(Machine::list()?),
        Cmd::Init {
            machine_name,
            url,
            input_options: options,
            module_paths,
        } => {
            let machine = module::init(
                machine_name,
                url.as_ref().map(GitUrl::from),
                options,
                module_paths,
            )?;
            if !options.no_build {
                machine.build(true)?;
                log::info!("Machine '{machine_name}' is ready! Use `codchi exec {machine_name}` to start it.")
            } else {
                alert_dirty(machine);
            }
        }
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
                url,
                options,
                module_paths,
            } => {
                let machine = module::add(machine_name, GitUrl::from(url), options, module_paths)?;
                if !options.no_build {
                    machine.build(true)?;
                } else {
                    alert_dirty(machine);
                }
            }
            cli::ModuleCmd::Set {
                machine_name,
                url,
                options,
                name,
                new_name,
                module_path,
            } => {
                let machine = module::set(
                    machine_name,
                    name,
                    options,
                    new_name,
                    module_path,
                    url.as_ref().map(GitUrl::from),
                )?;
                if !options.no_build {
                    machine.build(false)?;
                } else {
                    alert_dirty(machine);
                }
            }
            cli::ModuleCmd::Delete { name, module_name } => {
                alert_dirty(module::delete(name, module_name)?)
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
