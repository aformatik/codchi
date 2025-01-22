use super::{platform, CommandExt, LinuxCommandTarget, NixDriver};
use crate::{
    config::MachineConfig,
    consts::{self, store, ToPath},
    util::{LinuxPath, PathExt},
};
use crate::{
    logging::{log_progress, set_progress_status},
    progress_scope,
};
use anyhow::{bail, Context, Result};
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    path::PathBuf,
    sync::mpsc::channel,
};

/// Internal name of driver module in codchi's NixOS modules
pub const NIXOS_DRIVER_NAME: &str = platform::NIXOS_DRIVER_NAME;

/// Attribute path to store rootfs in codchi's flake
pub const NIX_STORE_PACKAGE: &str = platform::NIX_STORE_PACKAGE;

pub type HasStarted = bool;

/// The interface to a platform specific store driver (LXD / WSL) which provides access to nix.
pub trait Store: Sized {
    /// Import (if not existant) and start the store container (if not running). Must wait for it
    /// to start properly
    fn start_or_init_container() -> Result<Self>;

    fn init() -> Result<Self> {
        let flake_url = consts::CODCHI_FLAKE_URL;
        let system = consts::NIX_SYSTEM;
        let flake_path = consts::host::DIR_CONFIG
            .join_store()
            .get_or_create()?
            .join("flake.nix");
        let flake_content = format!(
            r#"{{
  inputs.codchi.url = "{flake_url}";
  outputs = {{ codchi, ... }}: {{
    packages.{system}.default = codchi.packages.{system}.{NIX_STORE_PACKAGE}.config.build.runtime;
  }};
}}"#
        );
        {
            let mut file = File::create(flake_path)?;
            file.write_all(flake_content.as_bytes())?;
            file.sync_all()?;
        }

        progress_scope! {
            set_progress_status("Starting store container...");
            Self::start_or_init_container()
        }
    }

    /// Get driver for running commands inside store
    fn cmd(&self) -> impl NixDriver;

    fn gc(&self, min_age: Option<u16>, all: bool, machine_names: &Vec<String>) -> Result<()> {
        #[cfg(target_family = "windows")]
        {
            if !inquire::Confirm::new(
                "Currently, garbage collection will delete user-created roots for example when \
using 'nix build' or direnv. Still procceed? [y/n]",
            )
            .prompt()?
            {
                bail!("Operation was canceled by the user");
            }
        }
        set_progress_status("Deleting dead store paths...");
        if let Some(min_age) = min_age {
            let mut args = vec!["profile", "wipe-history", "--profile", "system"];
            let min_age_str = format!("{}d", min_age);
            if min_age > 0 {
                args.extend(["--older-than", &min_age_str]);
            }

            let machines = if all {
                MachineConfig::list()?
            } else {
                let all_machines: HashMap<String, MachineConfig> = MachineConfig::list()?
                    .into_iter()
                    .map(|cfg| (cfg.name.clone(), cfg))
                    .collect();
                let mut machines = Vec::with_capacity(all_machines.len());
                for name in machine_names {
                    match all_machines.get(name) {
                        Some(cfg) => machines.push(cfg.clone()),
                        None => bail!("Machine {name} doesn't exist."),
                    }
                }
                machines
            };
            for machine in machines {
                self.cmd()
                    .run("nix", &args)
                    .with_cwd(store::DIR_CONFIG.join_machine(&machine.name))
                    .wait_ok()?;
            }
        }
        self.cmd()
            .script("nix $NIX_VERBOSITY store gc".to_string())
            .output_ok_streaming(channel().1, |line| {
                log_progress("gc", log::Level::Debug, &line)
            })?;
        Ok(())
    }

    fn _store_path_to_host(&self, path: &LinuxPath) -> anyhow::Result<PathBuf>;

    /// Resolve an absolute path with all symlinks resolved on the host. This only works reliable
    /// for nix store paths
    /// * `path` - Store path starting with '/nix/'
    fn store_path_to_host(&self, path: &LinuxPath) -> anyhow::Result<PathBuf> {
        let host_path = self._store_path_to_host(path)?;

        fs::metadata(&host_path)
            .with_context(|| format!("Store path '{path}', resolved to '{host_path:?}', is not accessible from your host."))?;

        Ok(host_path)
    }
}
