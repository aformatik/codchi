/// these types are for JSON / human output from codchi
use serde::{Deserialize, Serialize};
use serde_with::*;

use crate::platform::ConfigStatus;

pub type StatusOutput = Vec<MachineStatus>;

#[serde_as]
#[derive(Serialize, Deserialize)]
pub struct MachineStatus {
    pub name: String,
    #[serde_as(as = "serde_with::DisplayFromStr")]
    pub status: ConfigStatus,
    pub running: bool,
}

pub type ModLsOutput = Vec<Mod>;
#[derive(Serialize, Deserialize)]
pub struct Mod {
    pub name: String,
    pub url: String,
    pub flake_module: String,
}
