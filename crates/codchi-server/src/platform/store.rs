use super::shell::ShellDriver;
use crate::platform::cmd::{LinuxCommand, NixDriver};
use crate::state::PlatformStatus;
use anyhow::Context;
use ipc::health::HealthCheck;
use shared::cmd::CommandExt;
use shared::consts;
use std::sync::mpsc::Receiver;
use std::thread;
use std::time::{Duration, SystemTime};

// use super::cmd::nix::NixDriver;
//
// /// Internal name of driver module in codchi's NixOS modules
// pub const NIXOS_DRIVER_NAME: &str = platform::NIXOS_DRIVER_NAME;
//
// /// Attribute path to store rootfs in codchi's flake
// pub const NIX_STORE_PACKAGE: &str = platform::NIX_STORE_PACKAGE;
//
// pub type HasStarted = bool;
//
// #[derive(Debug, Clone, Default)]
// pub struct StoreState {
//     pub status: Status,
// }
//
// #[derive(Debug, Clone, Default, Serialize, Deserialize)]
// pub enum Status {
//     #[default]
//     Stopped,
//     Starting, Running, }
//
// /// The interface to a platform specific store driver (LXD / WSL) which provides access to nix.
pub trait Store: Sized {
    /// Attribute of the store tar.gz in codchi's flake.nix
    fn get_nix_flake_attribute(&self) -> &'static str;

    /// Get driver for running shell commands inside store
    fn shell(&self) -> impl ShellDriver + 'static;

    /// Get driver for running shell commands inside store
    fn read_platform_status(&self) -> anyhow::Result<PlatformStatus>;

    /// Register the store container with the platform driver. Don't start it as starting the first
    /// time / subsequent times should be isomorphic
    fn register(&self) -> anyhow::Result<()>;

    /// Start store container and return stream to its logs. Each platform implementation should
    /// healthcheck the platform side. Health checks inside the container are done centrally.
    fn start(&self) -> anyhow::Result<Receiver<String>>;

    /// Stop store container
    fn stop(&self) -> anyhow::Result<()>;

    fn check_health(&self) -> HealthCheck {
        let shell = self.shell();

        try {
            // check if basic shell command in store succeeds
            shell
                .build(LinuxCommand::run("echo", &[]))
                .wait_ok()
                .context("Failed to run basic shell command in store container")?;
            // check if staticBin is installed
            shell
                .build(LinuxCommand::run("nix", &["--version"]))
                .wait_ok()
                .context("Failed to run package from static binaries")?;
            // check if runtimePackages are installed
            shell
                .build(LinuxCommand::run("git", &["--version"]))
                .wait_ok()
                .context("Failed to run package from runtime packages")?;

            let start = SystemTime::now();
            let mut ping_success = shell.ping_store();
            let max_ping_duration = Duration::from_secs(10);
            while !ping_success
                && SystemTime::now()
                    .duration_since(start)
                    .unwrap_or(Duration::from_millis(0))
                    < max_ping_duration
            {
                thread::sleep(Duration::from_millis(250));
                ping_success = shell.ping_store();
            }

            if !ping_success {
                Err(anyhow::anyhow!(
                    "Failed to ping store after {max_ping_duration:?}"
                ))?;
            }
        }
        .into()
    }

    //     /// Import (if not existant) and start the store container (if not running). Must wait for it
    //     /// to start properly
    //     fn start_or_init_container() -> Result<Self>;
    //
    //     fn init() -> Result<Self> {
    //         let flake_url = consts::CODCHI_FLAKE_URL;
    //         let system = consts::NIX_SYSTEM;
    //         let flake_path = consts::host::DIR_CONFIG
    //             .join_store()
    //             .get_or_create()?
    //             .join("flake.nix");
    //         let flake_content = format!(
    //             r#"{{
    //   inputs.codchi.url = "{flake_url}";
    //   outputs = {{ codchi, ... }}: {{
    //     packages.{system}.default = codchi.packages.{system}.{NIX_STORE_PACKAGE}.config.build.runtime;
    //   }};
    // }}"#
    //         );
    //         {
    //             let mut file = File::create(flake_path)?;
    //             file.write_all(flake_content.as_bytes())?;
    //             file.sync_all()?;
    //         }
    //
    //         progress_scope! {
    //             set_progress_status("Starting store container...");
    //             Self::start_or_init_container()
    //         }
    //     }
    //
    //
    //     fn gc(&self, min_age: Option<u16>, all: bool, machine_names: &Vec<String>) -> Result<()> {
    //         #[cfg(target_family = "windows")]
    //         {
    //             if !inquire::Confirm::new(
    //                 "Currently, garbage collection will delete user-created roots for example when \
    // using 'nix build' or direnv. Still procceed? [y/n]",
    //             )
    //             .prompt()?
    //             {
    //                 bail!("Operation was canceled by the user");
    //             }
    //         }
    //         set_progress_status("Deleting dead store paths...");
    //         if let Some(min_age) = min_age {
    //             let mut args = vec!["profile", "wipe-history", "--profile", "system"];
    //             let min_age_str = format!("{}d", min_age);
    //             if min_age > 0 {
    //                 args.extend(["--older-than", &min_age_str]);
    //             }
    //
    //             let machines = if all {
    //                 MachineConfig::list()?
    //             } else {
    //                 let all_machines: HashMap<String, MachineConfig> = MachineConfig::list()?
    //                     .into_iter()
    //                     .map(|cfg| (cfg.name.clone(), cfg))
    //                     .collect();
    //                 let mut machines = Vec::with_capacity(all_machines.len());
    //                 for name in machine_names {
    //                     match all_machines.get(name) {
    //                         Some(cfg) => machines.push(cfg.clone()),
    //                         None => bail!("Machine {name} doesn't exist."),
    //                     }
    //                 }
    //                 machines
    //             };
    //             for machine in machines {
    //                 self.cmd()
    //                     .run("nix", &args)
    //                     .with_cwd(store::DIR_CONFIG.join_machine(&machine.name))
    //                     .wait_ok()?;
    //             }
    //         }
    //         self.cmd()
    //             .script("nix $NIX_VERBOSITY store gc".to_string())
    //             .output_ok_streaming(channel().1, |line| {
    //                 log_progress("gc", log::Level::Debug, &line)
    //             })?;
    //         Ok(())
    //     }
    //
    //     fn _store_path_to_host(&self, path: &LinuxPath) -> anyhow::Result<PathBuf>;
    //
    //     /// Resolve an absolute path with all symlinks resolved on the host. This only works reliable
    //     /// for nix store paths
    //     /// * `path` - Store path starting with '/nix/'
    //     fn store_path_to_host(&self, path: &LinuxPath) -> anyhow::Result<PathBuf> {
    //         let host_path = self._store_path_to_host(path)?;
    //
    //         fs::metadata(&host_path)
    //             .with_context(|| format!("Store path '{path}', resolved to '{host_path:?}', is not accessible from your host."))?;
    //
    //         Ok(host_path)
    //     }
}

pub trait GenFlake {
    fn gen_flake(&self) -> String;
}

impl<T: Store> GenFlake for T {
    fn gen_flake(&self) -> String {
        let flake_url = consts::CODCHI_FLAKE_URL;
        let system = consts::NIX_SYSTEM;
        let store_package = self.get_nix_flake_attribute();
        format!(
            r#"{{
  inputs.codchi.url = "{flake_url}";
  outputs = {{ codchi, ... }}: {{
    packages.{system}.default = codchi.packages.{system}.{store_package}.config.build.runtime;
  }};
}}"#
        )
    }
}
