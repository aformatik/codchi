//! Linux/rootless-Podman store driver.
//!
//! v1 (`v1/phases/01-podman-store.md`): the store is image-defined and fully
//! self-contained. There is no runtime provisioning — no host-written
//! `flake.nix`, no `nix profile install` from github, no `create-files` at init.
//! Bringing the store up is `podman create` from the baked image with the `/nix`
//! named volume, then `podman start`; the container's thin PID-1 init starts
//! `nix daemon` with zero server involvement (S1/S2).

use std::ffi::OsString;
use std::path::PathBuf;
use std::process::Command;

use codchi_shared::{
    CommandError, CommandExt, STORE_CONTAINER_NAME, STORE_NIX_DIR, STORE_NIX_VOLUME_NAME,
};

use super::{Store, StoreError, StorePlatformStatus};

const STORE_IMAGE_ENV: &str = "CODCHI_PODMAN_STORE_IMAGE";

/// Rootless-Podman implementation of the v1 store lifecycle.
pub struct PodmanStore {
    podman: OsString,
    image: PathBuf,
}

impl PodmanStore {
    /// Build the production adapter from the Nix wrapper's environment.
    pub fn from_env() -> Result<Self, StoreError> {
        let image = std::env::var_os(STORE_IMAGE_ENV)
            .map(PathBuf::from)
            .ok_or_else(|| {
                StoreError::new(format!(
                    "${STORE_IMAGE_ENV} is not set; the codchi-server package is incomplete"
                ))
            })?;
        Ok(Self::new("podman", image))
    }

    pub fn new(podman: impl Into<OsString>, image: PathBuf) -> Self {
        Self {
            podman: podman.into(),
            image,
        }
    }

    fn command(&self) -> Command {
        Command::new(&self.podman)
    }

    /// `podman container exists` is the one call where a non-zero exit is a
    /// normal answer (1 = absent), not a failure — so it can't go through
    /// `wait_ok`. Map only a genuine spawn failure into [`CommandError`].
    fn container_exists(&self) -> Result<bool, StoreError> {
        let mut command = self.command();
        command.args(["container", "exists", STORE_CONTAINER_NAME]);
        let output = command.output().map_err(|source| CommandError::Spawn {
            command: format!("{command:?}"),
            source,
        })?;
        match output.status.code() {
            Some(0) => Ok(true),
            Some(1) => Ok(false),
            _ => Err(StoreError::new(format!(
                "podman could not determine whether {STORE_CONTAINER_NAME} exists: {}",
                String::from_utf8_lossy(&output.stderr).trim()
            ))),
        }
    }

    fn load_image(&self) -> Result<String, StoreError> {
        let output = self
            .command()
            .args(["load", "-q", "-i"])
            .arg(&self.image)
            .output_utf8()?;
        output
            .split_whitespace()
            .last()
            .map(str::to_owned)
            .ok_or_else(|| {
                StoreError::new(format!(
                    "podman load returned no image name for {}",
                    self.image.display()
                ))
            })
    }
}

impl Store for PodmanStore {
    fn status(&self) -> Result<StorePlatformStatus, StoreError> {
        if !self.container_exists()? {
            return Ok(StorePlatformStatus::NotInstalled);
        }

        let running = self
            .command()
            .args([
                "container",
                "inspect",
                "--format={{.State.Running}}",
                STORE_CONTAINER_NAME,
            ])
            .output_utf8()?;
        match running.trim() {
            "true" => Ok(StorePlatformStatus::Running),
            "false" => Ok(StorePlatformStatus::Stopped),
            value => Err(StoreError::new(format!(
                "podman returned an invalid running state for {STORE_CONTAINER_NAME}: {value:?}"
            ))),
        }
    }

    fn register(&self) -> Result<(), StoreError> {
        let image = self.load_image()?;
        // Only `/nix` is a named volume: it holds the shared build cache + nix db
        // and must survive container recreation on store update/repair (S4). The
        // store keeps no durable host config tree; nix files are ephemeral (S6).
        let nix_volume = format!("{STORE_NIX_VOLUME_NAME}:{STORE_NIX_DIR}");

        self.command()
            .arg("create")
            .args(["--name", STORE_CONTAINER_NAME])
            .args(["--volume", &nix_volume])
            .arg(image)
            .wait_ok()
            .map_err(Into::into)
    }

    fn start(&self) -> Result<(), StoreError> {
        self.command()
            .args(["start", STORE_CONTAINER_NAME])
            .wait_ok()
            .map_err(Into::into)
    }

    fn probe_health(&self) -> Result<(), StoreError> {
        // `nix store ping` over the daemon socket is the readiness signal: it
        // runs the baked-in static `nix` *and* proves the in-container daemon is
        // answering. `/bin/run` supplies the daemon-pointing env. One round-trip,
        // cheap enough for the 15 s sentinel.
        self.command()
            .arg("exec")
            .arg(STORE_CONTAINER_NAME)
            .args(["run", "nix", "store", "ping", "--store", "daemon"])
            .wait_ok()
            .map_err(Into::into)
    }

    fn stop(&self) -> Result<(), StoreError> {
        self.command()
            .args(["stop", STORE_CONTAINER_NAME])
            .wait_ok()
            .map_err(Into::into)
    }
}
