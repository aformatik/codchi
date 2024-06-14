use anyhow::anyhow;
use std::path;

use super::*;
use crate::{
    consts::{host, PathExt, ToPath, MACHINE_PREFIX},
    util::Required,
};

#[serde_as]
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct MachineConfig {
    #[serde(skip)]
    pub name: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde_as(as = "Option<DisplayFromStr>")]
    pub nixpkgs_from: Option<ModuleName>,

    #[serde(default)]
    #[serde_as(as = "HashMap<DisplayFromStr, DisplayFromStr>")]
    pub modules: HashMap<ModuleName, CodchiModule>,

    #[serde(default)]
    pub secrets: HashMap<String, String>,
}

pub enum ConfigResult {
    Exists,
    /// Paths are case insensitive on windows but not in wsl, which leads to errors. Therefore we
    /// can't allow machines with case insensitive matching names. Returns real path of matching
    /// machine.
    SimilarExists(String),
    None,
}

impl MachineConfig {
    pub fn new(name: &str) -> Self {
        MachineConfig {
            name: name.to_string(),
            nixpkgs_from: Default::default(),
            modules: Default::default(),
            secrets: Default::default(),
        }
    }

    pub fn find(name: &str) -> Result<ConfigResult> {
        let path = host::DIR_CONFIG.join_machine(name).join("config.json");
        if fs::metadata(&path).is_err() {
            Ok(ConfigResult::None)
        } else if path
            .canonicalize()?
            .display()
            .to_string()
            .ends_with(&format!("{name}{}config.json", path::MAIN_SEPARATOR))
        {
            Ok(ConfigResult::Exists)
        } else {
            Ok(ConfigResult::SimilarExists(
                path.iter()
                    .nth_back(1)
                    .ok_or(anyhow!(
                        "Failed to extract machine name from path '{}'",
                        path.display()
                    ))?
                    .to_string_lossy()
                    .to_string(),
            ))
        }
    }

    /// Creates empty config file if missing. Make sure that the machine with the exact `name`
    /// (must match case sensitive!) exists
    pub fn open(name: &str, write_mode: bool) -> Result<(LockedConfig, Option<MachineConfig>)> {
        let path = host::DIR_CONFIG
            .join_machine(name)
            .get_or_create()?
            .join("config.json");
        LockedConfig::open_parse(
            &path,
            write_mode,
            |content| {
                let mut cfg: MachineConfig = serde_json::from_str(content)?;
                cfg.name = name.to_owned();
                log::trace!("Read machine config from {path:?}: {cfg:?}");
                Ok(Some(cfg))
            },
            || Ok(None),
        )
    }

    /// Only opens the config if it already exists
    pub fn open_existing(name: &str, write_mode: bool) -> Result<(LockedConfig, MachineConfig)> {
        match MachineConfig::find(name)? {
            ConfigResult::Exists => {}
            _ => anyhow::bail!("Machine '{name}' doesn't exist."),
        }
        let (lock, cfg) = Self::open(name, write_mode)?;
        Ok((lock, cfg.unwrap_or(Self::new(name))))
    }

    pub fn write(&self, lock: LockedConfig) -> Result<()> {
        let content = serde_json::to_string_pretty(self)?;
        lock.write(content)
    }

    pub fn list() -> Result<Vec<Self>> {
        let mut machines = Vec::new();
        for entry in fs::read_dir(host::DIR_CONFIG.join(MACHINE_PREFIX).get_or_create()?)? {
            let entry = entry?;
            if entry.metadata()?.is_dir() {
                log::trace!("Found possible machine {:?}.", entry.file_name());
                if let (_, Some(machine)) = Self::open(&entry.file_name().to_string_lossy(), false)?
                {
                    machines.push(machine);
                }
            }
        }
        machines.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(machines)
    }
}

pub type CodchiModule = FlakeUrl<Required>;

#[derive(Clone, Debug, Deserialize)]
pub struct EnvSecret {
    pub name: String,
    pub description: String,
}
// fn get_machines(&mut self) -> &mut toml_edit::Table {
//     if !self.doc.contains_table("machines") {
//         self.doc["machines"] = table();
//     }
//     self.doc["machines"]
//         .as_table_mut()
//         .expect("Config toml doesn't contain key 'machines'")
// }

// fn get_machine(&mut self, name: &str) -> Option<&mut toml_edit::Table> {
//     self.get_machines()
//         .get_mut(name)
//         .and_then(|t| t.as_table_mut())
//         .map(|t| {
//             if !t.contains_key("modules") {
//                 t["modules"] = array();
//             }
//             t
//         })
// }
