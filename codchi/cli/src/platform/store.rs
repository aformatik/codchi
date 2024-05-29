use super::{
    platform,
    private::{self, Private},
    CommandExt, LinuxCommandTarget, LinuxPath, NixDriver,
};
use crate::{config::MachineConfig, consts::*, util::with_spinner};
use anyhow::{bail, Context, Result};
use std::{collections::HashMap, fs, path::PathBuf, thread, time::Duration};

/// Internal name of driver module in codchi's NixOS modules
pub const NIXOS_DRIVER_NAME: &str = platform::NIXOS_DRIVER_NAME;

/// Attribute path to store rootfs in codchi's flake
pub const NIX_STORE_PACKAGE: &str = platform::NIX_STORE_PACKAGE;

pub type HasStarted = bool;

/// The interface to a platform specific store driver (LXD / WSL) which provides access to nix.
pub trait Store: Sized {
    /// Import (if not existant) and start the store container (if not running)
    fn start_or_init_container(_: private::Private) -> Result<Self>;

    fn init(_: private::Private) -> Result<Self> {
        let flake_path = host::DIR_CONFIG
            .join_store()
            .get_or_create()?
            .join("flake.nix");
        let flake_content = format!(
            r#"{{
  inputs.codchi.url = "{CODCHI_FLAKE_URL}";
  outputs = {{ codchi, ... }}: {{
    packages.{NIX_SYSTEM}.default = codchi.packages.{NIX_SYSTEM}.{NIX_STORE_PACKAGE}.config.build.runtime;
  }};
}}"#
        );
        fs::write(flake_path, flake_content)?;

        with_spinner(
            "Starting store container. This may take a while the first time...",
            |_| {
                let store = Self::start_or_init_container(private::Private)?;

                while store
                    .cmd()
                    .run("nix", &["store", "ping", "--store", "daemon"])
                    .wait_ok()
                    .is_err()
                {
                    thread::sleep(Duration::from_millis(250));
                }
                Ok(store)
            },
        )
    }

    /// Get driver for running commands inside store
    fn cmd(&self) -> impl NixDriver;

    fn gc(&self, min_age: Option<u16>, all: bool, machine_names: &Vec<String>) -> Result<()> {
        if !inquire::Confirm::new(
            "Currently, garbage collection will delete user-created roots for example when \
            using 'nix build' or direnv. Still procceed?",
        )
        .prompt()?
        {
            bail!("Operation was canceled by the user");
        }
        with_spinner("Deleting dead store paths...", |_| {
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
                .run("nix", &["store", "gc"])
                .output_ok_streaming(|line| log::info!("{line}\r"))?;
            Ok(())
        })
    }

    fn _store_path_to_host(&self, path: &LinuxPath, _: Private) -> anyhow::Result<PathBuf>;

    /// Resolve an absolute path with all symlinks resolved on the host. This only works reliable
    /// for nix store paths
    /// * `path` - Store path starting with '/nix/'
    fn store_path_to_host(&self, path: &LinuxPath) -> anyhow::Result<PathBuf> {
        let host_path = self._store_path_to_host(path, Private)?;

        fs::metadata(&host_path)
            .with_context(|| format!("Store path '{path}', resolved to '{host_path:?}', is not accessible from your host."))?;

        Ok(host_path)
    }

    // Collect substituters from all machines and add to stores' nix.conf
    // fn write_nix_conf(&self) -> Result<()> {
    //     let mut all = NixSettings {
    //         substituters: vec![
    //             "https://cache.nixos.org/".to_owned(),
    //             "https://nixos-devenv.cachix.org".to_owned(),
    //         ]
    //         .into_iter()
    //         .collect(),

    //         trusted_public_keys: vec![
    //             "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=".to_owned(),
    //             "nixos-devenv.cachix.org-1:TfcIbSCGLCufAt9UCxzBTi3ekrzgI3HAHX73VWpByoE=".to_owned(),
    //         ]
    //         .into_iter()
    //         .collect(),
    //     };
    //     for machine in MachineConfig::list()? {
    //         let settings: NixSettings = self.cmd().eval(
    //             store::DIR_CONFIG.join_machine(&machine.name),
    //             "nixosConfigurations.default.config.nix.settings",
    //         )?;

    //         all.substituters.extend(settings.substituters);
    //         all.trusted_public_keys.extend(settings.trusted_public_keys);
    //     }

    //     log::trace!("All substituters: {all:?}");

    //     self.cmd()
    //         .script(format!(
    //             r#"

    // "#
    //         ))
    //         .with_cwd("/etc/nix")
    //         .wait_ok()?;

    //     Ok(())
    // }
}

// #[derive(Deserialize, Debug, Clone, Default)]
// #[serde(rename_all = "kebab-case")]
// struct NixSettings {
//     #[serde(default)]
//     substituters: HashSet<String>,
//     #[serde(default)]
//     trusted_public_keys: HashSet<String>,
// }
