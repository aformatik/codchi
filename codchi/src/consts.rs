use anyhow::{Context, Result};
use directories::ProjectDirs;
use once_cell::sync::Lazy;
use std::{
    env, fs,
    path::{Path, PathBuf},
};

static CODCHI_PROJ_DIR: Lazy<ProjectDirs> =
    Lazy::new(|| ProjectDirs::from("de", "aformatik", "codchi").unwrap());
static CODCHI_CONFIG_DIR: Lazy<PathBuf> = Lazy::new(|| {
    env::var_os("CODCHI_CONFIG_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| CODCHI_PROJ_DIR.config_dir().to_path_buf())
});
static CODCHI_DATA_DIR: Lazy<PathBuf> = Lazy::new(|| {
    env::var_os("CODCHI_DATA_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| CODCHI_PROJ_DIR.data_local_dir().to_path_buf())
});
static CODCHI_RUNTIME_DIR: Lazy<PathBuf> = Lazy::new(|| {
    env::var_os("CODCHI_RUNTIME_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            CODCHI_PROJ_DIR
                .runtime_dir()
                .map(Path::to_path_buf)
                .unwrap_or_else(|| env::temp_dir().join("codchi"))
        })
});

pub const CONTROLLER_NAME: &str = "codchi-controller";
pub static GIT_BRANCH: &str = env!("CODCHI_GIT_BRANCH");

#[cfg(target_arch = "x86_64")]
pub const NIX_SYSTEM: &str = "x86_64-linux";
#[cfg(target_arch = "aarch64")]
pub const NIX_SYSTEM: &str = "aarch64_linux";

#[derive(Debug)]
#[allow(dead_code)]
pub enum Dir {
    Config,
    Data,
    Runtime,
}

impl Dir {
    fn get_path(&self) -> PathBuf {
        match self {
            Dir::Config => CODCHI_CONFIG_DIR.clone(),
            Dir::Data => CODCHI_DATA_DIR.clone(),
            Dir::Runtime => CODCHI_RUNTIME_DIR.clone(),
        }
    }

    pub fn get_or_create(&self) -> Result<PathBuf> {
        let path = self.get_path();
        fs::create_dir_all(&path).with_context(|| {
            format!(
                "Failed to create {self:?} directory for codchi at path {}",
                path.to_string_lossy()
            )
        })?;
        Ok(path)
    }
}
