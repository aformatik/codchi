use std::{fs, io, path::PathBuf, process::Command, sync::OnceLock};

use crate::{
    consts::{self, host, PathExt, ToPath},
    platform::{CommandExt, PlatformStatus},
    util::make_writeable_if_exists,
};
use anyhow::{anyhow, bail, Context, Result};
// use codepage_strings::Coding as _;
use itertools::Itertools;
use known_folders::{get_known_folder_path, KnownFolder};
use log::warn;
use version_compare::Version;
// use windows::Win32::System::Console::GetConsoleOutputCP as _;
use wslapi::Library;

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
    cmd.env("WSL_UTF8", "1");
    cmd
}

pub fn get_api() -> Result<&'static Library> {
    static WSLAPI: OnceLock<Library> = OnceLock::new();
    WSLAPI.get_or_try_init(Library::new).context(
        "Failed to load wslapi.dll. This usually means the Windows feature 'Windows Subsystem for \
        Linux' is not active. Please see <https://codchi.dev/docs/start/installation.html#prerequisites> \
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
            Please see <https://codchi.dev/docs/start/installation.html#prerequisites> \
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
        .args(&["--list", "--running", "--quiet"])
        .output_utf8_ok()?
        .lines()
        .contains(&container_name)
    {
        Ok(PlatformStatus::Running)
    } else {
        Ok(PlatformStatus::Stopped)
    }
}

pub fn import<T, F: Fn() -> Result<T>>(
    rootfs_name: &str,
    name: &str,
    installation_path: PathBuf,
    additional_setup: F,
) -> Result<T> {
    (|| {
        let msix_path = get_known_folder_path(KnownFolder::ProgramData)
            .ok_or(anyhow!("FOLDERID_ProgramData missing"))?
            .join(consts::APP_NAME)
            .join(rootfs_name);
        assert!(
            fs::metadata(&msix_path).is_ok(),
            "WSL rootfs for {name} missing in MSIX. Search path was: {msix_path:?}"
        );

        let tmp_path = host::DIR_RUNTIME.get_or_create()?.join(rootfs_name);
        make_writeable_if_exists(&tmp_path)?;
        fs::copy(&msix_path, &tmp_path)?;

        wsl_command()
            .arg("--import")
            .arg(name)
            .arg(installation_path)
            .arg(&tmp_path)
            .args(&["--version", "2"])
            .wait_ok()?;

        make_writeable_if_exists(&tmp_path)?;
        fs::remove_file(&tmp_path)?;
        additional_setup()
    })()
    .map_err(|err| {
        log::error!("Removing leftovers of WSL container {name}...");
        let _ = wsl_command().arg("--terminate").arg(name).wait_ok();
        let _ = wsl_command().arg("--unregister").arg(name).wait_ok();
        err
    })
}
