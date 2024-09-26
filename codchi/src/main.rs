#![feature(let_chains)]
#![feature(once_cell_try)]
#![deny(unused_crate_dependencies)]
// #![windows_subsystem = "windows"]

use crate::{
    cli::{Cli, Cmd, CLI_ARGS},
    platform::{Driver, Machine, Store},
};
use base64::{prelude::BASE64_STANDARD, Engine};
use clap::Parser;
use colored as _;
use config::{git_url::GitUrl, CodchiConfig, MachineConfig};
use logging::CodchiOutput;
use platform::{ConfigStatus, Host};
use std::env;
use util::{ResultExt, UtilExt};

pub mod cli;
pub mod config;
pub mod consts;
pub mod logging;
pub mod module;
pub mod platform;
pub mod tray;
pub mod util;

fn main() -> anyhow::Result<()> {
    not_so_human_panic::setup_panic!();

    let cli = Cli::parse();

    logging::init(cli.verbose.log_level_filter())?;

    #[cfg(target_os = "windows")]
    {
        // https://github.com/rust-lang/cargo/issues/1721
        use util::dbg_duration;
        use windows::Win32::{
            System::Console::GetConsoleWindow,
            UI::WindowsAndMessaging::{self, ShowWindow},
        };
        if cli.terminal == Some(false) {
            dbg_duration("hide console", || {
                let window = unsafe { GetConsoleWindow() };
                if !window.0.is_null() {
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
    let cfg = CodchiConfig::get();

    if !matches!(cli.command, Some(Cmd::Tray {})) && cfg.tray.autostart {
        Driver::host()
            .start_tray(false)
            .trace_err("Failed starting codchi's tray")
            .ignore();
    }

    CLI_ARGS
        .set(cli.clone())
        .expect("Only main is allowed to set CLI_ARGS.");

    let _ = Driver::store();

    match &cli.command.unwrap_or(Cmd::Status {}) {
        Cmd::Status {} => Machine::list()?.print(cli.json),
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
        Cmd::Exec { name, cmd } => Machine::by_name(name)?.exec(cmd)?,
        Cmd::Delete {
            name,
            i_am_really_sure,
        } => Machine::by_name(name)?.delete(*i_am_really_sure)?,
        Cmd::Module(cmd) => match cmd {
            cli::ModuleCmd::List { name } => {
                let json = cli.json;
                let (_, cfg) = MachineConfig::open_existing(name, false)?;
                cfg.modules.print(json);
            }
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
        Cmd::Tray {} => tray::run()?,
    }
    if CodchiConfig::get().tray.autostart {
        Driver::host()
            .start_tray(true)
            .trace_err("Failed restarting tray")
            .ignore();
    }

    Ok(())
}

// Alerts the user if there was a change to the machine. Also restarts / updates tray
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
