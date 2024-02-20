// use anyhow::{Context, Result};
use directories::ProjectDirs;
use once_cell::sync::Lazy;
use std::{
    env, fs, io,
    path::{Path, PathBuf},
};

pub static GIT_BRANCH: &str = env!("CODCHI_GIT_BRANCH");
pub static CODCHI_FLAKE_URL: &str = concat!("github:aformatik/codchi/", env!("CODCHI_GIT_BRANCH"));

pub const CONTAINER_STORE_NAME: &str = "codchistore";

#[cfg(target_arch = "x86_64")]
pub const NIX_SYSTEM: &str = "x86_64-linux";
#[cfg(target_arch = "aarch64")]
pub const NIX_SYSTEM: &str = "aarch64_linux";

pub const STORE_NAME: &str = "store";
pub const MACHINE_PREFIX: &str = "machine";

pub trait ToPath: Sized {
    /// Get just the path without any IO
    fn as_path_buf(&self) -> &PathBuf;

    /// Get store dir inside
    fn join_store(&self) -> PathBuf {
        self.as_path_buf().join(STORE_NAME)
    }

    // /// Get store dir inside
    // fn join<P: AsRef<Path>>(&self, path: P) -> PathBuf {
    //     self.as_path_buf().join(path)
    // }

    /// Get store dir inside
    fn join_machine<P: AsRef<Path>>(&self, name: P) -> PathBuf {
        self.as_path_buf().join(MACHINE_PREFIX).join(name)
    }

    /// Create the directory recursively if it doesn't exist and return its path
    fn get_or_create(&self) -> io::Result<&PathBuf> {
        let path = self.as_path_buf();
        fs::create_dir_all(&path)?;
        Ok(path)
    }
}

impl ToPath for PathBuf {
    #[inline]
    fn as_path_buf(&self) -> &PathBuf {
        self
    }
}

pub mod host {
    use super::*;
    static CODCHI_PROJ_DIR: Lazy<ProjectDirs> =
        Lazy::new(|| ProjectDirs::from("dev", "codchi", "codchi").unwrap());
    pub static DIR_CONFIG: Lazy<PathBuf> = Lazy::new(|| {
        env::var_os("CODCHI_CONFIG_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| CODCHI_PROJ_DIR.config_dir().to_path_buf())
    });
    pub static DIR_DATA: Lazy<PathBuf> = Lazy::new(|| {
        env::var_os("CODCHI_DATA_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| CODCHI_PROJ_DIR.data_local_dir().to_path_buf())
    });
    pub static DIR_NIX: Lazy<PathBuf> = Lazy::new(|| {
        env::var_os("CODCHI_NIX_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| CODCHI_PROJ_DIR.cache_dir().join("/nix").to_path_buf())
    });
    pub static DIR_RUNTIME: Lazy<PathBuf> = Lazy::new(|| {
        env::var_os("CODCHI_RUNTIME_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| {
                CODCHI_PROJ_DIR
                    .runtime_dir()
                    .map(Path::to_path_buf)
                    .unwrap_or_else(|| env::temp_dir().join("codchi"))
            })
    });
}

pub mod store {
    use super::*;

    pub static DIR_CONFIG: Lazy<PathBuf> = Lazy::new(|| "/config".into());
    pub static DIR_DATA: Lazy<PathBuf> = Lazy::new(|| "/data".into());
    pub static DIR_NIX: Lazy<PathBuf> = Lazy::new(|| "/nix".into());
}

pub mod machine {
    pub fn machine_name(name: &str) -> String {
        format!("codchi-{name}")
    }
}

pub mod user {
    pub const ROOT_UID: &str = "0";
    pub const ROOT_GID: &str = "0";
    pub const ROOT_HOME: &str = "/root";

    pub const DEFAULT_NAME: &str = "codchi";
    pub const DEFAULT_HOME: &str = "/home/codchi";
    pub const DEFAULT_UID: &str = "1000";
    pub const DEFAULT_GID: &str = "100";
}
