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

    #[serde_as(as = "HashMap<DisplayFromStr, DisplayFromStr>")]
    pub modules: HashMap<ModuleName, CodchiModule>,
}

impl MachineConfig {
    pub fn read(name: &str) -> Result<Option<MachineConfig>> {
        let (_, cfg) = Self::open(name, false)?;
        Ok(cfg)
    }
    pub fn open(name: &str, write_mode: bool) -> Result<(LockedConfig, Option<MachineConfig>)> {
        let path = host::DIR_CONFIG
            .join_machine(name)
            .get_or_create()?
            .join("config.json");
        LockedConfig::open_parse(
            path,
            write_mode,
            |content| {
                let mut cfg: MachineConfig = serde_json::from_str(content)?;
                cfg.name = name.to_owned();
                Ok(Some(cfg))
            },
            || Ok(None),
        )
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
                if let Some(machine) = Self::read(&entry.file_name().to_string_lossy())? {
                    machines.push(machine);
                }
            }
        }
        machines.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(machines)
    }
}

pub type CodchiModule = FlakeUrl<Required>;

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
