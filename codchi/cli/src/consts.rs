// use anyhow::{Context, Result};
use crate::platform::LinuxPath;
use once_cell::sync::Lazy;
use std::{
    env,
    fmt::Debug,
    fs, io,
    path::{Path, PathBuf},
};

pub const APP_NAME: &str = "codchi";

pub static GIT_BRANCH: &str = env!("CODCHI_GIT_BRANCH");
pub static CODCHI_FLAKE_URL: &str = concat!("github:aformatik/codchi/", env!("CODCHI_GIT_BRANCH"));

pub const CONTAINER_STORE_NAME: &str = "codchistore";

#[cfg(target_arch = "x86_64")]
pub const NIX_SYSTEM: &str = "x86_64-linux";
#[cfg(target_arch = "aarch64")]
pub const NIX_SYSTEM: &str = "aarch64_linux";

pub const STORE_NAME: &str = "store";
pub const MACHINE_PREFIX: &str = "machine";

// these are used for store / machine container init
pub const INIT_EXIT_ERR: &str = "INIT_ERR";
pub const INIT_EXIT_SUCCESS: &str = "INIT_SUCCESS";

pub trait ToPath: Sized {
    fn join_str(&self, name: &str) -> Self;

    /// Get store dir inside
    fn join_store(&self) -> Self {
        self.join_str(STORE_NAME)
    }

    /// Get store dir inside
    fn join_machine(&self, name: &str) -> Self {
        self.join_str(MACHINE_PREFIX).join_str(name)
    }
}

pub trait PathExt: AsRef<Path> + Sized + core::fmt::Debug {
    /// Create the directory recursively if it doesn't exist and return its path
    fn get_or_create(&self) -> io::Result<&Self> {
        if fs::metadata(self).is_err() {
            fs::create_dir_all(self)?;
        }
        Ok(self)
    }

    /// Remove the directory and all of its contents if it exists and create an empty folder
    fn cleanup_and_get(self) -> io::Result<Self> {
        if fs::metadata(&self).is_ok() {
            fs::remove_dir_all(&self)?;
        }
        fs::create_dir_all(&self)?;
        Ok(self)
    }

    /// Remove the directory and log::warn if an error occured
    fn remove(self) {
        if let Ok(meta) = fs::metadata(&self) {
            let result = if meta.is_dir() {
                fs::remove_dir_all(&self)
            } else {
                fs::remove_file(&self)
            };
            if let Err(err) = result {
                log::warn!("Could not remove '{self:?}'. Reason: {err}");
            }
        } else {
            log::trace!("Not removing non existant path '{self:?}'");
        }
    }

    fn assert_exists(&self) -> io::Result<()> {
        fs::metadata(self)?;
        Ok(())
    }
}

impl<P: AsRef<Path> + Debug> PathExt for P {}
impl ToPath for PathBuf {
    fn join_str(&self, path: &str) -> Self {
        self.join(path)
    }
}

pub mod host {
    use directories::BaseDirs;

    use crate::config::CodchiConfig;

    use super::*;
    pub static BASE_DIR: Lazy<BaseDirs> = Lazy::new(|| BaseDirs::new().unwrap());
    pub static DIR_CONFIG: Lazy<PathBuf> = Lazy::new(|| BASE_DIR.config_dir().join(APP_NAME));
    pub static DIR_DATA: Lazy<PathBuf> = Lazy::new(|| {
        CodchiConfig::get()
            .data_dir
            .clone()
            .map(PathBuf::from)
            .unwrap_or(BASE_DIR.data_local_dir().join(APP_NAME))
    });
    pub static DIR_NIX: Lazy<PathBuf> =
        Lazy::new(|| BASE_DIR.cache_dir().join(APP_NAME).join("nix"));
    pub static DIR_RUNTIME: Lazy<PathBuf> = Lazy::new(|| {
        BASE_DIR
            .runtime_dir()
            .map(Path::to_path_buf)
            .unwrap_or_else(env::temp_dir)
            .join(APP_NAME)
    });
}

pub mod store {
    use super::*;

    pub static DIR_CONFIG: Lazy<LinuxPath> = Lazy::new(|| LinuxPath("/config".to_string()));
    pub static DIR_DATA: Lazy<LinuxPath> = Lazy::new(|| LinuxPath("/data".to_string()));
    pub static DIR_NIX: Lazy<LinuxPath> = Lazy::new(|| LinuxPath("/nix".to_string()));

    pub const INIT_ENV: &str = "/.store-init-env";
    pub const INIT_LOG: &str = "/.store-init-log";

    impl ToPath for LinuxPath {
        fn join_str(&self, name: &str) -> Self {
            LinuxPath(format!("{}/{}", self.0, name))
        }
    }
}

pub mod machine {
    pub fn machine_name(name: &str) -> String {
        format!("codchi-{name}")
    }
    pub const INIT_ENV: &str = "/mnt/wsl/codchi/.machine-init-env";
    pub fn init_log(name: &str) -> String {
        format!("/mnt/wsl/codchi/.machine-init-log-{name}")
    }
}

pub mod user {
    use super::*;

    pub const ROOT_UID: &str = "0";
    pub const ROOT_GID: &str = "0";
    pub static ROOT_HOME: Lazy<LinuxPath> = Lazy::new(|| LinuxPath("/root".to_string()));

    pub const DEFAULT_NAME: &str = "codchi";
    pub static DEFAULT_HOME: Lazy<LinuxPath> = Lazy::new(|| LinuxPath("/home/codchi".to_string()));
    pub const DEFAULT_UID: &str = "1000";
    pub const DEFAULT_GID: &str = "100";
}

pub mod files {
    pub const STORE_ROOTFS_NAME: &str = "store.tar.gz";
    pub const MACHINE_ROOTFS_NAME: &str = "machine.tar.gz";
}
