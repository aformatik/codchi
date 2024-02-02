use std::fs;

use anyhow::Result;
use spinoff::*;

use super::private::Private;
use crate::{
    config::{Config, MachineConfig},
    consts::{host, machine, store, ToPath},
    platform::{Command, CommandDriver, Driver, Store},
    util::UtilExt,
};

pub trait MachineDriver: Sized {
    fn read_platform(name: &str, _: Private) -> Result<PlatformStatus>;
    fn cmd(&self) -> impl CommandDriver;

    fn read(name: String, config: MachineConfig, _: Private) -> Result<Machine> {
        let platform_status = Self::read_platform(&name, Private)?;
        let config_status = {
            use ConfigStatus::*;
            let machine_dir = host::DIR_CONFIG.join_machine(&name);
            if platform_status == PlatformStatus::NotInstalled
                || fs::metadata(machine_dir.join("profile")).is_err()
            {
                NotInstalled
            } else {
                Driver::store().cmd().output_from_str(
                    Command::script(format!(
                        /* bash */
                        r#"
set -x
if [ -n "$(git diff flake.nix)" ]; then
  printf "{Modified}"
elif [ -n "$(git diff flake.lock)" ]; then
  printf "{UpdatesAvailable}"
else 
  printf "{UpToDate}"
fi
"#,
                    ))
                    .cwd(store::DIR_CONFIG.join_machine(&name)),
                )?
            }
        };
        Ok(Machine {
            name,
            config,
            config_status,
            platform_status,
        })
    }

    fn by_name(name: String) -> Result<Option<Machine>> {
        match Config::read()?.machines.get(&name) {
            Some(config) => Ok(Some(Machine::read(name, config.clone(), Private)?)),
            None => Ok(None),
        }
    }

    fn list() -> Result<Vec<Machine>> {
        Config::read()?
            .machines
            .into_iter()
            .map(|(name, config)| Machine::read(name, config, Private))
            .collect()
    }

    fn write(name: &str, machine: &MachineConfig) -> Result<()> {
        let machine_dir = host::DIR_CONFIG.join_machine(name);
        machine_dir.get_or_create()?;
        fs::write(&machine_dir.join("flake.nix"), machine.gen_flake())?;

        let mut spinner = Spinner::new_with_stream(
            spinners::Dots,
            "Initializing machine...",
            Color::Blue,
            spinoff::Streams::Stderr,
        );
        Driver::store()
            .cmd()
            .run(
                Command::script(format!(
                    /* bash */
                    r#"
if [ ! -d .git ]; then
  git init -q
  git add flake.nix
fi
"#
                ))
                .cwd(store::DIR_CONFIG.join_machine(name)),
            )
            .finally(|| spinner.clear())?;

        Ok(())
    }

    fn build(name: &str) -> Result<()> {
        let mut spinner = Spinner::new_with_stream(
            spinners::Dots,
            format!("Building {}...", machine::machine_name(name)),
            Color::Blue,
            spinoff::Streams::Stderr,
        );
        Driver::store()
            .cmd()
            .run(
                Command::script(format!(
                    /* bash */
                    r#"
if [ ! -e profile ]; then
  nix profile install --profile profile '.#nixosConfigurations.default.config.system.build.toplevel'
else
  nix profile upgrade --profile profile '.*'
fi
git add flake.*
"#
                ))
                .cwd(store::DIR_CONFIG.join_machine(name)),
            )
            .finally(|| spinner.clear())?;

        Ok(())
    }

    fn update(name: &str) -> Result<()> {
        let mut spinner = Spinner::new_with_stream(
            spinners::Dots,
            format!("Checking for updates for {}...", machine::machine_name(name)),
            Color::Blue,
            spinoff::Streams::Stderr,
        );
        Driver::store()
            .cmd()
            .run(
                Command::script(format!(
                    /* bash */
                    r#"
nix flake update
"#
                ))
                .cwd(store::DIR_CONFIG.join_machine(name)),
            )
            .finally(|| spinner.clear())?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Machine {
    pub name: String,
    pub config: MachineConfig,
    pub config_status: ConfigStatus,
    pub platform_status: PlatformStatus,
}

/// The (NixOS) status of the machine configuration
#[derive(Debug, PartialEq, Eq, Clone, strum::EnumString, strum::Display)]
pub enum ConfigStatus {
    /// Machine was added / configured, but not built and installed
    NotInstalled,

    /// Machine was already built and installed but config has changed (flake.nix has changed)
    Modified,

    /// Machine was already built and installed but updates are available (flake.lock has changed)
    UpdatesAvailable,

    /// Machine is built, installed and up to date
    UpToDate,
    // Machine is currently building / installing
    // Installing,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PlatformStatus {
    NotInstalled,
    Stopped,
    Running,
}
