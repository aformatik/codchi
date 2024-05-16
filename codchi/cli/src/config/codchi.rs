use toml_edit::Document;

use crate::consts::{host, PathExt};

use super::*;

// #[serde_as]
#[derive(Clone, Debug, Deserialize, Serialize, Default)]
pub struct CodchiConfig {}

#[allow(unused)]
impl CodchiConfig {
    fn open_mut() -> Result<(LockedConfig, Document)> {
        let path = host::DIR_CONFIG.get_or_create()?.join("config.toml");
        LockedConfig::open_parse(
            path,
            true,
            |content| Ok(toml_edit::Document::from_str(content)?),
            || Ok(toml_edit::ser::to_document(&CodchiConfig::default())?),
        )
    }

    fn open() -> Result<Self> {
        let path = host::DIR_CONFIG.get_or_create()?.join("config.toml");
        let (_lock, cfg) = LockedConfig::open_parse(
            path,
            false,
            |content| Ok(toml_edit::de::from_str(content)?),
            || Ok(Self::default()),
        )?;
        Ok(cfg)
    }
}
