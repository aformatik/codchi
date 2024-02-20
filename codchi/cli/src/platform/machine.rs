use super::{private::Private, LinuxUser};
use crate::{
    config::{Config, MachineConfig, MutableConfig},
    consts::{self, host, store, user, ToPath},
    platform::{self, Command, CommandDriver, Driver, Store},
    util::with_spinner,
};
use anyhow::{bail, Context, Result};
use itertools::Itertools;
use platform::Output;
use std::{fs, thread, time::Duration};

pub trait MachineDriver: Sized {
    fn cmd(&self) -> impl CommandDriver;

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
                    .cwd(store::DIR_CONFIG.join_machine(&self.name)),
                )?
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
            Driver::store().cmd().run(
                Command::script(format!(
                    /* bash */
                    r#"
if [ ! -d .git ]; then
  git init -q
  git add flake.nix
fi
"#
                ))
                .cwd(store::DIR_CONFIG.join_machine(&self.name)),
            )
        })?;

        Ok(())
    }

    pub fn build(&self) -> Result<()> {
        self.write_flake()?;
        with_spinner(format!("Building {}...", self.name), |spinner| {
            Driver::store().cmd().run(
                Command::script(format!(
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
                .cwd(store::DIR_CONFIG.join_machine(&self.name)),
            )?;

            spinner.update_text(format!("Building {}...", self.name));

            let status = Self::read_platform_status(&self.name, Private)?;
            if status == PlatformStatus::NotInstalled {
                spinner.update_text(format!("Installing {}...", self.name));
                self.install(Private)?;

                spinner.update_text(format!("Initializing {}...", self.name));
                self.wait_online()?;
                self.cmd().run(Command::new("poweroff", &[]))?;
            } else {
                if status == PlatformStatus::Stopped {
                    spinner.update_text(format!("Starting {}...", self.name));
                    self.start(Private)?;
                    self.wait_online()?;
                }
                self.cmd().run(
                    Command::new(
                        "/nix/var/nix/profiles/system/bin/switch-to-configuration",
                        &["switch"],
                    )
                    .user(LinuxUser::Root),
                )?;
            }
            Ok(())
        })
    }

    pub fn wait_online(&self) -> Result<()> {
        while self
            .cmd()
            .run(Command::new("nix", &["store", "ping", "--store", "daemon"]))
            .is_err()
        {
            thread::sleep(Duration::from_millis(250));
        }
        Ok(())
    }

    pub fn update(self) -> Result<Self> {
        self.write_flake()?;
        with_spinner(format!("Checking for updates for {}...", self.name), |_| {
            Driver::store().cmd().run(
                Command::new("nix", &["flake", "update"])
                    .cwd(store::DIR_CONFIG.join_machine(&self.name)),
            )
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
            spinner.update_text(format!("Stopping {name}"));
            if self.platform_status == PlatformStatus::Running {
                self.force_stop(Private)?;
            }
            spinner.update_text(format!("Deleting container of {name}"));
            if self.platform_status != PlatformStatus::NotInstalled {
                MachineDriver::delete_container(&self, Private)?;
            }

            spinner.update_text(format!("Deleting files from {name}"));
            Driver::store()
                .cmd()
                .run(Command::new(
                    "rm",
                    &[
                        "-rf",
                        store::DIR_DATA.join_machine(&self.name).to_str().unwrap(),
                        store::DIR_CONFIG.join_machine(&self.name).to_str().unwrap(),
                    ],
                ))
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
            Some((cmd, args)) => {
                Command::new(cmd, &args.iter().map(|str| str.as_str()).collect_vec())
                    .user(LinuxUser::Default)
            }
            None => Command::new("su", &["-l", user::DEFAULT_NAME]),
        };
        self.cmd()
            .exec(cmd.output(Output::Inherit).cwd(user::DEFAULT_HOME))?;
        Ok(())
    }
}
