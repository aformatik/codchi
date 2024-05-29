use std::sync::OnceLock;

use toml_edit::Document;

use crate::consts::{host, PathExt};

use super::*;

// #[serde_as]
#[derive(Clone, Debug, Deserialize, Serialize, Default)]
pub struct CodchiConfig {
    #[cfg(target_os = "windows")]
    pub vcxsrv: VcXsrvConfig,

    // $XDG_DATA_HOME/codchi by default
    pub data_dir: Option<String>
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct VcXsrvConfig {
    #[serde(default = "def_true")]
    pub enable: bool,
    #[serde(default = "def_false")]
    pub tray_icon: bool,
}

impl Default for VcXsrvConfig {
    fn default() -> Self {
        Self {
            enable: true,
            tray_icon: false,
        }
    }
}

// #[allow(unused)]
impl CodchiConfig {
    pub fn open_mut() -> Result<(LockedConfig, Document)> {
        let path = host::DIR_CONFIG.get_or_create()?.join("config.toml");
        LockedConfig::open_parse(
            path,
            true,
            |content| Ok(toml_edit::Document::from_str(content)?),
            || Ok(toml_edit::ser::to_document(&CodchiConfig::default())?),
        )
    }

    pub fn get() -> &'static Self {
        static CFG: OnceLock<CodchiConfig> = OnceLock::new();
        let result: Result<&'static CodchiConfig> = CFG.get_or_try_init(|| {
            let path = host::DIR_CONFIG.get_or_create()?.join("config.toml");
            let (_lock, cfg) = LockedConfig::open_parse(
                path,
                false,
                |content| Ok(toml_edit::de::from_str(content)?),
                || Ok(Self::default()),
            )?;
            log::trace!("Read codchi config: {cfg:?}");
            Ok(cfg)
        });
        result.expect("Failed initializing Driver.")
    }
}

const fn def_true() -> bool {
    true
}
const fn def_false() -> bool {
    false
}
