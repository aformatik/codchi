use log::*;
use std::{
    fs,
    path::{Path, PathBuf},
    process::exit,
};

use lazy_static::lazy_static;

use crate::build;

lazy_static! {
    static ref CODCHI_CONFIG_DIR: PathBuf = dirs_next::config_dir()
        .expect("No config dir present")
        .join(build::PROJECT_NAME);
    static ref CODCHI_STATE_DIR: PathBuf = dirs_next::data_local_dir()
        .expect("No data local dir present")
        .join(build::PROJECT_NAME);
}

fn get_or_create_dir(path: &Path) {
    match fs::create_dir_all(path) {
        Err(e) => {
            error!("Could not create dir {}: {}", path.display(), e);
            exit(1);
        }
        Ok(()) => {}
    }
}

#[allow(dead_code)]
pub fn config_dir() -> &'static Path {
    let p = CODCHI_CONFIG_DIR.as_path();
    get_or_create_dir(p);
    p
}

pub fn state_dir() -> &'static Path {
    let p = CODCHI_STATE_DIR.as_path();
    get_or_create_dir(p);
    p
}
