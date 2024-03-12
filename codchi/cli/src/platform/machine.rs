use super::{private::Private, LinuxCommandTarget, LinuxUser};
use crate::{
    config::{Config, MachineConfig, MutableConfig},
    consts::{self, host, store, user, PathExt, ToPath},
    platform::{self, CommandExt, Driver, Store},
    util::with_spinner,
};
use anyhow::{bail, Context, Result};
use itertools::Itertools;
use log::info;
use std::{fs, thread, time::Duration};

pub trait MachineDriver: Sized {
    fn cmd(&self) -> impl LinuxCommandTarget;

    /// Read if container is running / stopped / not installed
    fn read_platform_status(name: &str, _: Private) -> Result<PlatformStatus>;

    /// Import and configure machine container
    fn install(&self, _: Private) -> Result<()>;

    /// Start container
    fn start(&self, _: Private) -> Result<()>;

    /// Kill container
    fn force_stop(&self, _: Private) -> Result<()>;

    /// Delete container
    fn delete_container(&self, _: Private) -> Result<()>;
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
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PlatformStatus {
    NotInstalled,
    Stopped,
    Running,
}

impl Machine {
    pub fn update_status(mut self) -> Result<Self> {
        self.platform_status = Self::read_platform_status(&self.name, Private)?;
        self.config_status = {
            use ConfigStatus::*;
            let machine_dir = host::DIR_CONFIG.join_machine(&self.name);
            if self.platform_status == PlatformStatus::NotInstalled
                || fs::symlink_metadata(machine_dir.join("system")).is_err()
            {
                NotInstalled
            } else {
                Driver::store()
                    .cmd()
                    .script(format!(
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
                    .with_cwd(store::DIR_CONFIG.join_machine(&self.name))
                    .output_from_str()?
            }
        };
        Ok(self)
    }
    pub fn read(name: &str, config: MachineConfig, _: Private) -> Result<Self> {
        Self {
            name: name.to_string(),
            config,
            config_status: ConfigStatus::NotInstalled,
            platform_status: PlatformStatus::NotInstalled,
        }
        .update_status()
    }

    /// Returns Err if machine doesn't exist
    pub fn by_name(name: &str) -> Result<Self> {
        match Config::read()?.machines.get(name) {
            Some(config) => Ok(Self::read(name, config.clone(), Private)?),
            None => bail!("There is no machine with name '{name}'. List available machines with `codchi status`."),
        }
    }

    pub fn list() -> Result<Vec<Self>> {
        Config::read()?
            .machines
            .into_iter()
            .map(|(name, config)| Self::read(&name, config, Private))
            .collect()
    }

    pub fn write_flake(&self) -> Result<()> {
        let machine_dir = host::DIR_CONFIG.join_machine(&self.name);
        machine_dir.get_or_create()?;

        let flake = {
            let codchi_url = consts::CODCHI_FLAKE_URL;
            let module_inputs = self
                .config
                .modules
                .iter()
                .enumerate()
                .map(|(idx, url)| format!(r#"    "{idx}".url = "{}";"#, url.to_nix_url()))
                .join("\n");
            let driver = platform::NIXOS_DRIVER_NAME;
            let nix_system = consts::NIX_SYSTEM;
            let nixpkgs = if let Some(idx) = self.config.nixpkgs_from {
                format!(r#"inputs."{idx}".inputs.nixpkgs"#)
            } else {
                "inputs.codchi.inputs.nixpkgs".to_string()
            };
            let modules = self
                .config
                .modules
                .iter()
                .enumerate()
                .map(|(idx, url)| {
                    format!(
                        r#"        inputs."{idx}".{module_name}"#,
                        module_name = url.flake_attr.0
                    )
                })
                .join("\n");
            format!(
                r#"{{
  inputs = {{
    codchi.url = "{codchi_url}";
{module_inputs}
  }};
  outputs = inputs: {{
    nixosConfigurations.default = inputs.codchi.lib.codeMachine {{
      driver = "{driver}";
      system = "{nix_system}";
      nixpkgs = {nixpkgs};
      modules = [
{modules}
      ];
    }};
  }};
}}"#
            )
        };
        fs::write(&machine_dir.join("flake.nix"), flake)?;

        with_spinner("Initializing machine...", |_| {
            Driver::store()
                .cmd()
                .script(format!(
                    /* bash */
                    r#"
if [ ! -d .git ]; then
  git init -q
  git add flake.nix
fi
"#
                ))
                .with_cwd(store::DIR_CONFIG.join_machine(&self.name))
                .wait_ok()
        })?;

        Ok(())
    }

    pub fn build(&self) -> Result<()> {
        self.write_flake()?;
        with_spinner(format!("Building {}...", self.name), |spinner| {
            Driver::store()
                .cmd()
                .script(format!(
                    /* bash */
                    r#"
if [ ! -e system ]; then
  nix profile install --profile system '.#nixosConfigurations.default.config.system.build.toplevel'
else
  nix profile upgrade --profile system '.*'
fi
pwd
git add flake.*
"#
                ))
                .with_cwd(store::DIR_CONFIG.join_machine(&self.name))
                .output_ok_streaming(|line| info!("{line}\r"))?;

            spinner.set_message(format!("Building {}...", self.name));

            let status = Self::read_platform_status(&self.name, Private)?;
            if status == PlatformStatus::NotInstalled {
                spinner.set_message(format!("Installing {}...", self.name));
                self.install(Private).map_err(|err| {
                    log::error!("Removing leftovers of machine files for {}...", self.name);
                    let _ = fs::remove_dir_all(host::DIR_CONFIG.join_machine(&self.name));
                    let _ = fs::remove_dir_all(host::DIR_DATA.join_machine(&self.name));
                    err
                })?;

                spinner.set_message(format!("Initializing {}...", self.name));
                self.wait_online()?;
                // self.cmd().run("sudo", &["poweroff"]).wait_ok()?;
            } else {
                if status == PlatformStatus::Stopped {
                    spinner.set_message(format!("Starting {}...", self.name));
                    self.start(Private)?;
                    self.wait_online()?;
                }
                self.cmd()
                    .run(
                        "/nix/var/nix/profiles/system/bin/switch-to-configuration",
                        &["switch"],
                    )
                    .with_user(LinuxUser::Root)
                    .wait_ok()?;
            }
            Ok(())
        })
    }

    pub fn wait_online(&self) -> Result<()> {
        while self
            .cmd()
            .run("nix", &["store", "ping", "--store", "daemon"])
            .wait_ok()
            .is_err()
        {
            thread::sleep(Duration::from_millis(250));
        }
        Ok(())
    }

    pub fn update(self) -> Result<Self> {
        self.write_flake()?;
        with_spinner(format!("Checking for updates for {}...", self.name), |_| {
            Driver::store()
                .cmd()
                .run("nix", &["flake", "update"])
                .with_cwd(store::DIR_CONFIG.join_machine(&self.name))
                .wait_ok()
        })?;

        self.update_status()
    }

    pub fn delete(self, im_really_sure: bool) -> Result<()> {
        let name = &self.name;
        if !im_really_sure
            && !inquire::Confirm::new(&format!("Delete '{name}'?",))
                .with_help_message(&format!(
                    "This will remove all files associated with '{name}'"
                ))
                .prompt()?
        {
            bail!("Canceled deletion.");
        }

        with_spinner("", |spinner| {
            spinner.set_message(format!("Stopping {name}"));
            if self.platform_status == PlatformStatus::Running {
                self.force_stop(Private)?;
            }
            spinner.set_message(format!("Deleting container of {name}"));
            if self.platform_status != PlatformStatus::NotInstalled {
                MachineDriver::delete_container(&self, Private)?;
            }

            spinner.set_message(format!("Deleting files from {name}"));
            Driver::store()
                .cmd()
                .run(
                    "rm",
                    &[
                        "-rf",
                        &store::DIR_DATA.join_machine(&self.name).0,
                        &store::DIR_CONFIG.join_machine(&self.name).0,
                    ],
                )
                .wait_ok()
                .context("Failed deleting data.")?;

            let mut cfg = MutableConfig::open()?;
            cfg.get_machines().remove_entry(&name);
            cfg.write()?;

            Ok(())
        })
    }

    pub fn exec(&self, cmd: &[String]) -> Result<()> {
        if self.config_status == ConfigStatus::NotInstalled
            || self.platform_status == PlatformStatus::NotInstalled
        {
            bail!(
                "Machine {} wasn't installed yet. Install with `codchi rebuild {}`.",
                self.name,
                self.name
            );
        }

        if self.platform_status == PlatformStatus::Stopped {
            self.start(Private)?;
            self.wait_online()?;
        }

        let cmd = match cmd.split_first() {
            Some((cmd, args)) => self
                .cmd()
                .run(cmd, &args.iter().map(|str| str.as_str()).collect_vec())
                .with_user(LinuxUser::Default),
            None => self.cmd().run("bash", &["-l"]),
        };
        cmd.with_cwd(user::DEFAULT_HOME.clone())
            .with_user(LinuxUser::Default).exec()?;
        Ok(())
    }
}
