use crate::{
    consts,
    logging::with_suspended_progress,
    platform::{CommandExt, PlatformStatus},
};
use anyhow::{anyhow, bail, Context, Result};
use inquire::InquireError;
use itertools::Itertools;
use known_folders::{get_known_folder_path, KnownFolder};
use log::{warn, Level};
use shared::util::{make_writeable_if_exists, with_tmp_file, LinuxPath, PathExt, ResultExt};
use std::{
    fs,
    io::{self, IsTerminal},
    path::{Path, PathBuf},
    process::Command,
    sync::OnceLock,
};
use sysinfo::System;
use version_compare::Version;
use wslapi::Library;

const WSL_VERSION_MIN: &str = env!("CODCHI_WSL_VERSION_MIN");
const WSL_VERSION_MAX: &str = env!("CODCHI_WSL_VERSION_MAX");

pub fn wsl_command() -> Command {
    let mut cmd = Command::new("wsl.exe");
    // cmd.env("WSL_ENV", "WSL_UTF8");
    cmd.env("WSL_UTF8", "1");
    cmd
}

pub fn get_api() -> Result<&'static Library> {
    static WSLAPI: OnceLock<Library> = OnceLock::new();
    WSLAPI.get_or_try_init(Library::new).context(
        "Failed to load wslapi.dll. This usually means the Windows feature 'Windows Subsystem for \
        Linux' is not active. Please see <https://codchi.dev/introduction/installation#prerequisites> \
        for the installation instructions.",
    )
}

pub fn get_wsl_version() -> Result<String> {
    let out = wsl_command().arg("--version").output_ok()?;

    let out = String::from_utf8_lossy(&out);
    let version_line = out.lines().next();

    version_line
        .map(str::to_lowercase)
        // .filter(|version_line| version_line.contains("wsl") && version_line.contains("version"))
        .and_then(|version_line| {
            version_line
                .split_whitespace()
                .last()
                .map(|str| str.to_string())
        })
        .filter(|version| Version::from(version).is_some())
        .ok_or(anyhow!(
            "Failed to parse WSL's version from output '{version_line:?}'. \
            Please see <https://codchi.dev/introduction/installation#prerequisites> \
            for the installation instructions."
        ))
}

pub fn check_wsl() -> Result<()> {
    get_api()?;

    let version_str = get_wsl_version()
        .context("Failed to run wsl.exe. Please install Windows Subsystem for Linux!
See <https://codchi.dev/introduction/installation#prerequisites> for instructions on how to do this.")?;
    let version = Version::from(&version_str);

    if !(Version::from(WSL_VERSION_MIN)..=Version::from(WSL_VERSION_MAX)).contains(&version) {
        warn!(
            "This version of codchi is only tested with WSL versions between {WSL_VERSION_MIN} \
              and {WSL_VERSION_MAX}, but you have version {version_str} installed. This can result \
              in unpredictable failures. Please update codchi and WSL to their latest versions!"
        )
    }

    Ok(())
}

pub fn get_platform_status(container_name: &str) -> Result<PlatformStatus> {
    if !get_api()?.is_distribution_registered(container_name) {
        Ok(PlatformStatus::NotInstalled)
    } else if wsl_command()
        .args(["--list", "--running", "--quiet"])
        .output_utf8_ok()?
        .lines()
        .contains(&container_name)
    {
        Ok(PlatformStatus::Running)
    } else {
        Ok(PlatformStatus::Stopped)
    }
}

/// Extract file from codchi.msix/VFS/Common Appdata/codchi
pub fn extract_from_msix<T, F: Fn(&PathBuf) -> Result<T>>(file_name: &str, f: F) -> Result<T> {
    let msix_path = get_known_folder_path(KnownFolder::ProgramData)
        .ok_or(anyhow!("FOLDERID_ProgramData missing"))?
        .join(consts::APP_NAME)
        .join(file_name);
    if fs::metadata(&msix_path).is_err() {
        bail!(
            "File '{file_name}' missing in MSIX. \
                  Search path was: {msix_path:?}. \
                  Directory contents: {:?}",
            msix_path
                .parent()
                .ok_or(anyhow!("Missing parent"))
                .and_then(|p| p.list_dir())
        );
    }
    with_tmp_file(&format!("msix-extract-{file_name}"), |tmp_path| {
        make_writeable_if_exists(&tmp_path)?;
        fs::copy(&msix_path, &tmp_path)?;

        if fs::metadata(&tmp_path).is_err() {
            bail!(
                "Failed copying file '{file_name}' in MSIX from {msix_path:?} to {tmp_path:?}. \
                  Search path was: {tmp_path:?}. \
                  Directory contents: {:?}",
                tmp_path
                    .parent()
                    .ok_or(anyhow!("Missing parent"))
                    .and_then(|p| p.list_dir())
            );
        }
        let result = f(tmp_path);
        make_writeable_if_exists(&tmp_path)?;
        result
    })
}

pub fn import<T, F: Fn() -> Result<T>>(
    rootfs_name: &str,
    name: &str,
    installation_path: &PathBuf,
    additional_setup: F,
) -> Result<T> {
    extract_from_msix(rootfs_name, |tmp_path| {
        wsl_command()
            .arg("--import")
            .arg(name)
            .arg(installation_path)
            .arg(&tmp_path)
            .args(["--version", "2"])
            .wait_ok()?;
        additional_setup()
    })
    .inspect_err(|_| {
        if !log::log_enabled!(Level::Debug) {
            log::error!("Removing leftovers of WSL container {name}...");
            let _ = wsl_command().arg("--terminate").arg(name).wait_ok();
            let _ = wsl_command().arg("--unregister").arg(name).wait_ok();
        }
    })
}

pub fn set_sparse(name: &str) -> Result<()> {
    if System::new_all().processes().iter().any(|(_, proc)| {
        proc.name().to_string_lossy().contains("vmmem")
            || proc.name().to_string_lossy().contains("vmmemWSL")
    }) {
        if io::stdin().is_terminal()
            && with_suspended_progress(|| {
                inquire::Confirm::new(&format!(
                    "Codchi needs to stop WSL in order to set \
WSL distribution '{name}' to sparse mode. WARNING: This will stop all WSL distributions \
including all running programs. Is this OK? [y/n]",
                ))
                .with_help_message("You can also do this manually at a later time.")
                .prompt()
            })
            .recover_err(|err| match err {
                InquireError::NotTTY => Ok(false),
                err => Err(err),
            })?
        {
            wsl_command().arg("--shutdown").wait_ok()?;
        } else {
            log::warn!(
                "WSL distro {name} was NOT set to sparse. \
You can do this manually with `wsl.exe --manage {name} --set-sparse true`. \
See <https://codchi.dev/usage/gc#large-wsl-distributions> for more information."
            );
            return Ok(());
        }
    }
    wsl_command()
        .args(["--manage", name, "--set-sparse", "true"])
        .wait_ok()?;

    Ok(())
}

// pub trait LinuxPathExt {
// fn to_host_path(self, instance_name: &str) -> PathBuf;
// }

impl LinuxPath {
    pub fn to_host_path(&self, instance_name: &str) -> PathBuf {
        Path::new(r"\\wsl$")
            .join(instance_name)
            .join(self.0.clone())
    }
}

pub fn win_path_to_wsl(path: &PathBuf) -> anyhow::Result<LinuxPath> {
    wsl_command()
        .args([
            "-d",
            &consts::CONTAINER_STORE_NAME,
            "--system",
            "--user",
            "root",
        ])
        .args([
            "wslpath",
            "-u",
            &path.display().to_string().replace("\\", "/"),
        ])
        .output_utf8_ok()
        .map(|path| LinuxPath(path.trim().to_owned()))
        .with_context(|| format!("Failed to run 'wslpath' with path {path:?}."))
}

pub fn recover_instance(rootfs: &str, instance_name: &str) -> anyhow::Result<()> {
    extract_from_msix(rootfs, |rootfs_tar| {
        let tar_from_wsl = win_path_to_wsl(rootfs_tar)?;
        extract_from_msix("busybox", |busybox| {
            let busybox_from_wsl = win_path_to_wsl(busybox)?;
            wsl_command()
                .args(["-d", instance_name, "--system", "--user", "root"])
                .args(["mount", "-o", "remount,rw", "/mnt/wslg/distro"])
                .wait_ok()?;
            wsl_command()
                .args(["-d", instance_name, "--system", "--user", "root"])
                .args([
                    &busybox_from_wsl.0,
                    "tar",
                    "-C",
                    "/mnt/wslg/distro",
                    "-xzf",
                    &tar_from_wsl.0,
                ])
                .wait_ok()?;

            let _ = wsl_command()
                .arg("--terminate")
                .arg(instance_name)
                .wait_ok()
                .trace_err("Failed stopping WSL instance");

            log::info!("Restored file system of `{instance_name}`.");
            Ok(())
        })
    })
}
