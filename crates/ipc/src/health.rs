use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Health {
    pub statuses: HashMap<HealthTopic, HealthCheck>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, Hash)]
pub enum HealthTopic {
    OS,
    Virtualization,
    Store,
    Machine,
    Other,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum HealthCheck {
    Ok,
    Err {
        name: String,
        last_errors: Vec<String>,
    },
}

impl Into<anyhow::Result<()>> for HealthCheck {
    fn into(self) -> anyhow::Result<()> {
        match self {
            HealthCheck::Ok => anyhow::Ok(()),
            HealthCheck::Err { name, last_errors } => {
                anyhow::bail!(
                    "Health check {name} failed. Relevant log lines: {}",
                    last_errors.join("\n")
                )
            }
        }
    }
}
