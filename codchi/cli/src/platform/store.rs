use super::{platform, private, Command, CommandDriver, NixDriver};
use crate::{consts::*, util::with_spinner};
use anyhow::Result;
use std::{fs, thread, time::Duration};

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
    packages.{NIX_SYSTEM}.default = codchi.packages.{NIX_SYSTEM}.{NIX_STORE_PACKAGE}.config.system.build.runtime;
  }};
}}"#
        );
        fs::write(flake_path, flake_content)?;

        with_spinner("Starting store container...", |spinner| {
            spinner.update_after_time(
                "Starting store container. This may take a while the first time...",
                Duration::from_secs(10),
            );
            let store = Self::start_or_init_container(private::Private)?;

            while store
                .cmd()
                .run(Command::new("nix", &["store", "ping", "--store", "daemon"]))
                .is_err()
            {
                thread::sleep(Duration::from_millis(250));
            }
            Ok(store)
        })
    }

    /// Get driver for running commands inside store
    fn cmd(&self) -> impl CommandDriver + NixDriver;
}
