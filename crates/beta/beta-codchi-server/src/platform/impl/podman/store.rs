use crate::platform::implementation::podman;
use crate::platform::implementation::shell::PodmanShellImpl;
use crate::platform::shell::ShellDriver;
use crate::platform::store::Store;
use crate::state::PlatformStatus;
use anyhow::Context;
use ipc::health::HealthCheck;
use shared::cmd::CommandExt;
use shared::consts;
use shared::util::PathExt;
use shared::util::ResultExt;
use shared::util::UtilExt;
use std::env;
use std::path::PathBuf;
use std::process::Command;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::channel;
use std::thread;

pub struct StoreImpl;

impl StoreImpl {
    pub fn new() -> Self {
        Self
    }
}

impl Store for StoreImpl {
    fn get_nix_flake_attribute(&self) -> &'static str {
        "store-podman"
    }

    fn shell(&self) -> impl ShellDriver + 'static {
        PodmanShellImpl::new_store()
    }

    fn read_platform_status(&self) -> anyhow::Result<PlatformStatus> {
        Ok(podman::get_platform_status(consts::CONTAINER_STORE_NAME)?)
    }

    fn register(&self) -> anyhow::Result<()> {
        let rootfs = env::var("CODCHI_PODMAN_STORE_IMAGE")
                        .map(PathBuf::from)
                        .context("Failed reading $CODCHI_PODMAN_STORE_IMAGE from environment. This indicates a broken build.")?;
        let container_label = podman::load(&rootfs.to_string_lossy())?;
        tracing::debug!("Loaded podman image {container_label} from {rootfs:?}");

        let mut cmd = Command::new("podman");
        cmd.arg("create");

        for (src, tgt) in [
            ("nix", &consts::store::DIR_NIX.0),
            (
                &consts::host::DIR_CONFIG.get_or_create()?.to_string_lossy(),
                &consts::store::DIR_CONFIG.0,
            ),
            (
                &consts::host::DIR_DATA.get_or_create()?.to_string_lossy(),
                &consts::store::DIR_DATA.0,
            ),
            //     // Mount all machine data as gcroots to prevent gc-ing auto roots from (e.g. direnv)
            //     LxdDevice::Disk {
            //         source: consts::host::DIR_DATA
            //             .get_or_create()?
            //             .join_str(consts::MACHINE_PREFIX)
            //             .clone(),
            //         path: "/nix/var/nix/gcroots/machine-data".to_string(),
            //     },
        ] {
            cmd.arg("--volume");
            cmd.arg(&format!("{src}:{tgt}"));
        }

        cmd.args(["--name", consts::CONTAINER_STORE_NAME]);

        cmd.arg(&container_label);

        cmd.wait_ok()?;

        //     anyhow::Ok(())
        // })()
        // .inspect_err(|_| {
        //     tracing::error!("Removing leftovers of store files...");
        //     let _ = podman::rm(consts::CONTAINER_STORE_NAME, true)
        //         .trace_err("Failed removing podman container");
        //     let _ = podman::rmi(&container_label).trace_err("Failed removing podman image");
        //     let _ = fs::remove_dir_all(consts::host::DIR_CONFIG.join_store());
        //     let _ = fs::remove_dir_all(consts::host::DIR_DATA.join_store());
        // })?;

        Ok(())
    }

    fn start(&self) -> anyhow::Result<Receiver<String>> {
        Command::new("podman")
            .args(["start", consts::CONTAINER_STORE_NAME])
            .wait_ok()?;

        let (tx, rx) = channel::<String>();

        thread::spawn(move || {
            Command::new("podman")
                .args(["logs", "-f", consts::CONTAINER_STORE_NAME])
                .output_ok_streaming(channel().1, |line| {
                    // tracing::info!(topic = log::TOPIC_STORE, "{line}")
                    tx.send(line)
                        // .trace_err("Failed sending store log line")
                        .ignore();
                })
                .trace_err("Failed streaming `podman logs`")
                .ignore();
        });

        anyhow::Ok(rx)
    }

    fn stop(&self) -> anyhow::Result<()> {
        Command::new("podman")
            .args(["stop", consts::CONTAINER_STORE_NAME])
            .wait_ok()?;
        Ok(())
    }
}
