use crate::util::{make_writeable_if_exists, LinuxPath, PathExt, ResultExt};
use crate::{
    consts::{self, host},
    logging::with_suspended_progress,
    platform::{CommandExt, PlatformStatus},
};
use anyhow::{anyhow, bail, Context, Result};
use inquire::InquireError;
use itertools::Itertools;
use known_folders::{get_known_folder_path, KnownFolder};
use log::{warn, Level};
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
// use windows::Win32::System::Console::GetConsoleOutputCP as _;
// use codepage_strings::Coding as _;

// #[derive(Error, Debug)]
// pub enum Error {
//     #[error("Failed to run WSL.exe")]
//     IO(#[from] io::Error),
// }

// type Result<T> = std::result::Result<T, Error>;

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

    let version_str = match get_wsl_version() {
        Err(e) => {
            if let Some(e) = e.root_cause().downcast_ref::<io::Error>() {
                if e.kind() == io::ErrorKind::NotFound {
                    // todo ref docs
                    bail!("Failed to run wsl.exe. Please install Windows Subsystem for Linux!",)
                }
            }
            Err(e)
        }
        version => version,
    }?;
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

pub fn import<T, F: FnMut() -> Result<T>>(
    rootfs_name: &str,
    name: &str,
    installation_path: PathBuf,
    mut additional_setup: F,
) -> Result<T> {
    (|| {
        let msix_path = get_known_folder_path(KnownFolder::ProgramData)
            .ok_or(anyhow!("FOLDERID_ProgramData missing"))?
            .join(consts::APP_NAME)
            .join(rootfs_name);
        if fs::metadata(&msix_path).is_err() {
            bail!(
                "WSL rootfs for {name} missing in MSIX. \
                  Search path was: {msix_path:?}. \
                  Directory contents: {:?}",
                msix_path
                    .parent()
                    .ok_or(anyhow!("Missing parent"))
                    .and_then(|p| p.list_dir())
            );
        }

        let tmp_path = host::DIR_RUNTIME.get_or_create()?.join(rootfs_name);
        make_writeable_if_exists(&tmp_path)?;
        fs::copy(&msix_path, &tmp_path)?;

        if fs::metadata(&tmp_path).is_err() {
            bail!(
                "WSL rootfs for {name} missing in tmp path. \
                  Search path was: {tmp_path:?}. \
                  Directory contents: {:?}",
                tmp_path
                    .parent()
                    .ok_or(anyhow!("Missing parent"))
                    .and_then(|p| p.list_dir())
            );
        }

        wsl_command()
            .arg("--import")
            .arg(name)
            .arg(installation_path)
            .arg(&tmp_path)
            .args(["--version", "2"])
            .wait_ok()?;

        make_writeable_if_exists(&tmp_path)?;
        fs::remove_file(&tmp_path)?;
        additional_setup()
    })()
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
including all running programs. Is this OK?",
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
