use crate::consts::host;
use crate::util::PathExt;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::{str::FromStr, sync::OnceLock};
use toml_edit::{value, DocumentMut};

use super::LockedConfig;

// #[serde_as]
#[derive(Clone, Debug, Deserialize, Serialize, Default, PartialEq, Eq)]
pub struct CodchiConfig {
    #[serde(default)]
    pub tray: TrayConfig,

    #[cfg(target_os = "windows")]
    #[serde(default)]
    pub vcxsrv: VcXsrvConfig,

    /// Allows internet access in WSL inside some company VPNs
    /// https://github.com/sakai135/wsl-vpnkit
    #[cfg(target_os = "windows")]
    #[serde(default)]
    pub enable_wsl_vpnkit: bool,

    /// Isolate the network of each code machine
    #[cfg(target_os = "windows")]
    #[serde(default = "def_true")]
    pub enable_wsl_netns: bool,

    // $XDG_DATA_HOME/codchi by default
    pub data_dir: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct VcXsrvConfig {
    #[serde(default = "def_false")]
    pub enable: bool,
    #[serde(default = "def_false")]
    pub tray: bool,
}

impl Default for VcXsrvConfig {
    fn default() -> Self {
        Self {
            enable: false,
            tray: false,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct TrayConfig {
    #[serde(default = "def_true")]
    pub autostart: bool,
}
impl Default for TrayConfig {
    fn default() -> Self {
        Self { autostart: true }
    }
}

// #[allow(unused)]
impl CodchiConfig {
    pub fn open_mut() -> Result<ConfigMut> {
        let path = host::DIR_CONFIG.get_or_create()?.join("config.toml");
        let (lock, doc) = LockedConfig::open_parse(
            path,
            true,
            |content| Ok(toml_edit::DocumentMut::from_str(content)?),
            || Ok(toml_edit::ser::to_document(&CodchiConfig::default())?),
        )?;
        Ok(ConfigMut { lock, doc })
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
        result.expect("Failed initializing Driver")
    }
}

const fn def_true() -> bool {
    true
}
const fn def_false() -> bool {
    false
}

pub struct ConfigMut {
    lock: LockedConfig,
    doc: DocumentMut,
}

impl ConfigMut {
    pub fn tray_autostart(&mut self, autostart: bool) {
        self.doc["tray"]["autostart"] = value(autostart);
    }

    #[cfg(target_os = "windows")]
    pub fn enable_wsl_vpnkit(&mut self, enable: bool) {
        self.doc["enable_wsl_vpnkit"] = value(enable);
    }

    #[cfg(target_os = "windows")]
    pub fn enable_wsl_netns(&mut self, enable: bool) {
        self.doc["enable_wsl_netns"] = value(enable);
    }

    #[cfg(target_os = "windows")]
    pub fn vcxsrv_enable(&mut self, enable: bool) {
        self.doc["vcxsrv"]["enable"] = value(enable);
    }

    #[cfg(target_os = "windows")]
    pub fn vcxsrv_tray(&mut self, enable: bool) {
        self.doc["vcxsrv"]["tray"] = value(enable);
    }

    pub fn write(self) -> Result<()> {
        self.lock.write(self.doc.to_string())
    }
}

#[cfg(test)]
mod tests {

    use super::CodchiConfig;

    #[test]
    fn empty_cfg_deserializes() {
        let result = toml_edit::de::from_str::<CodchiConfig>("");
        assert_eq!(Ok(Default::default()), result);
    }

    #[cfg(target_os = "windows")]
    #[test]
    fn partial_cfg_deserializes() {
        use crate::config::VcXsrvConfig;
        let result = toml_edit::de::from_str::<CodchiConfig>(
            "
vcxsrv.enable = false
",
        );
        assert_eq!(
            Ok(CodchiConfig {
                vcxsrv: VcXsrvConfig {
                    enable: false,
                    tray: false
                },
                ..Default::default()
            }),
            result
        );
    }

    #[cfg(target_os = "windows")]
    #[test]
    fn partial_cfg_deserializes2() {
        use crate::config::VcXsrvConfig;
        let result = toml_edit::de::from_str::<CodchiConfig>(
            "
vcxsrv.tray = true
",
        );
        assert_eq!(
            Ok(CodchiConfig {
                vcxsrv: VcXsrvConfig {
                    enable: false,
                    tray: true
                },
                ..Default::default()
            }),
            result
        );
    }
}
