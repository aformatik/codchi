use anyhow::{Context, Result};
use directories::ProjectDirs;
use lazy_static::lazy_static;
use std::{
    env, fs,
    path::{Path, PathBuf},
};

pub const CONTROLLER_NAME: &str = "codchi-controller";

lazy_static! {
    static ref CODCHI_PROJ_DIR: ProjectDirs =
        ProjectDirs::from("de", "aformatik", "codchi").unwrap();
    static ref CODCHI_CONFIG_DIR: PathBuf = env::var_os("CODCHI_CONFIG_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| CODCHI_PROJ_DIR.config_dir().to_path_buf());
    static ref CODCHI_DATA_DIR: PathBuf = env::var_os("CODCHI_DATA_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| CODCHI_PROJ_DIR.data_local_dir().to_path_buf());
    static ref CODCHI_RUNTIME_DIR: PathBuf = env::var_os("CODCHI_RUNTIME_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            CODCHI_PROJ_DIR
                .runtime_dir()
                .map(Path::to_path_buf)
                .unwrap_or_else(|| env::temp_dir().join("codchi"))
        });
}



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
