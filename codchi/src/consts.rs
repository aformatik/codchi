use crate::util::LinuxPath;
use std::sync::LazyLock;
use std::{
    env,
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

impl ToPath for PathBuf {
    fn join_str(&self, path: &str) -> Self {
        self.join(path)
    }
}

pub mod host {
    use crate::config::CodchiConfig;
    use directories::BaseDirs;

    use super::*;
    pub static BASE_DIR: LazyLock<BaseDirs> = LazyLock::new(|| BaseDirs::new().unwrap());
    pub static DIR_CONFIG: LazyLock<PathBuf> =
        LazyLock::new(|| BASE_DIR.config_dir().join(APP_NAME));
    pub static DIR_DATA: LazyLock<PathBuf> = LazyLock::new(|| {
        CodchiConfig::get()
            .data_dir
            .clone()
            .map(PathBuf::from)
            .unwrap_or(BASE_DIR.data_local_dir().join(APP_NAME))
    });
    pub static DIR_NIX: LazyLock<PathBuf> = LazyLock::new(|| DIR_DATA.join("nix"));
    pub static DIR_RUNTIME: LazyLock<PathBuf> = LazyLock::new(|| {
        BASE_DIR
            .runtime_dir()
            .map(Path::to_path_buf)
            .unwrap_or_else(env::temp_dir)
            .join(APP_NAME)
    });
    pub static LOGFILE_STORE: LazyLock<PathBuf> = LazyLock::new(|| DIR_DATA.join("log/store.log"));
    pub fn machine_log(name: &str) -> PathBuf {
        DIR_DATA.join(format!("log/machine-{name}.log"))
    }
}

pub mod store {
    use super::*;

    pub static DIR_CONFIG: LazyLock<LinuxPath> = LazyLock::new(|| LinuxPath("/config".to_string()));
    pub static DIR_DATA: LazyLock<LinuxPath> = LazyLock::new(|| LinuxPath("/data".to_string()));
    pub static DIR_NIX: LazyLock<LinuxPath> = LazyLock::new(|| LinuxPath("/nix".to_string()));

    pub static LOGFILE_STORE: LazyLock<LinuxPath> =
        LazyLock::new(|| DIR_DATA.join_str("log/store.log"));

    pub fn machine_log(name: &str) -> LinuxPath {
        DIR_DATA.join_str(&format!("log/machine-{name}.log"))
    }

    impl ToPath for LinuxPath {
        fn join_str(&self, name: &str) -> Self {
            LinuxPath(format!("{}/{}", self.0, name))
        }
    }
}

pub mod machine {
    use super::*;
    pub fn machine_name(name: &str) -> String {
        format!("codchi-{name}")
    }
    pub static CODCHI_ENV: LazyLock<LinuxPath> =
        LazyLock::new(|| LinuxPath("/etc/codchi-env".to_string()));
    pub static CODCHI_ENV_TMP: LazyLock<LinuxPath> =
        LazyLock::new(|| LinuxPath("/tmp/codchi-env".to_string()));
}

pub mod user {
    use super::*;

    pub const ROOT_UID: &str = "0";
    pub const ROOT_GID: &str = "0";
    pub static ROOT_HOME: LazyLock<LinuxPath> = LazyLock::new(|| LinuxPath("/root".to_string()));

    pub const DEFAULT_NAME: &str = "codchi";
    pub static DEFAULT_HOME: LazyLock<LinuxPath> =
        LazyLock::new(|| LinuxPath("/home/codchi".to_string()));
    pub const DEFAULT_UID: &str = "1000";
    pub const DEFAULT_GID: &str = "100";
}

pub mod files {
    pub const STORE_ROOTFS_NAME: &str = "store.tar.gz";
    pub const MACHINE_ROOTFS_NAME: &str = "machine.tar.gz";
}
