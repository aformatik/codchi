use crate::{
    consts::{self, ToPath},
    logging::with_suspended_progress,
    platform::{Driver, Machine, NixDriver, Store},
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct EnvSecret {
    pub name: String,
    pub description: String,
    pub value: Option<String>,
}

impl EnvSecret {
    pub fn prompt_value(&self) -> anyhow::Result<String> {
        let value = with_suspended_progress(|| {
            inquire::Password::new(&format!(
                "Please enter secret '{}' (Toggle input mask with <Ctrl+R>):",
                self.name
            ))
            .without_confirmation()
            .with_display_mode(inquire::PasswordDisplayMode::Masked)
            .with_help_message(self.description.trim())
            .with_display_toggle_enabled()
            .prompt()
        })?;
        Ok(value)
    }
}

pub trait MachineSecrets {
    /// combines secret definitions from the NixOS config with the values set by the user
    fn eval_env_secrets(&self) -> anyhow::Result<HashMap<String, EnvSecret>>;
}

impl MachineSecrets for Machine {
    fn eval_env_secrets(&self) -> anyhow::Result<HashMap<String, EnvSecret>> {
        let mut secrets: HashMap<String, EnvSecret> = Driver::store().cmd().eval(
            consts::store::DIR_CONFIG.join_machine(&self.config.name),
            "nixosConfigurations.default.config.codchi.secrets.env",
        )?;
        for (name, secret) in secrets.iter_mut() {
            secret.value = self.config.secrets.get(name).cloned();
        }
        Ok(secrets)
    }
}
