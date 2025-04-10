use super::{
    dialog_manager::{DialogIntent, DialogManager},
    status_entries::StatusEntries,
};
use crate::{
    config::MachineConfig,
    platform::{Machine, MachineDriver},
    secrets::{EnvSecret, MachineSecrets},
};
use anyhow::Result;
use itertools::Itertools;
use std::{
    collections::HashMap,
    path::PathBuf,
    sync::mpsc::{channel, Receiver, Sender},
    thread,
};

pub struct BackendBroker {
    unset_secrets: HashMap<String, (Machine, HashMap<String, Option<String>>)>,

    sender: Sender<(usize, ChannelDTO)>,
    receiver: Receiver<(usize, ChannelDTO)>,
}

impl BackendBroker {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        Self {
            unset_secrets: HashMap::new(),

            sender,
            receiver,
        }
    }

    pub fn update(
        &mut self,
        status_entries: &mut StatusEntries,
        dialog_manager: &mut DialogManager,
    ) -> Vec<BackendIntent> {
        self.receive_msgs(status_entries, dialog_manager)
    }

    fn receive_msgs(
        &mut self,
        status_entries: &mut StatusEntries,
        dialog_manager: &mut DialogManager,
    ) -> Vec<BackendIntent> {
        let mut intent = Vec::new();

        while let Ok((status_index, dto)) = self.receiver.try_recv() {
            status_entries.decrease(status_index);

            match dto {
                ChannelDTO::BuildFinished(machine) => {
                    let status_index = status_entries.create_entry(format!(
                        "Getting unset secrets of '{}'",
                        &machine.config.name
                    ));

                    let sender_clone = self.sender.clone();
                    thread::spawn(move || {
                        let unset_secrets_result = get_unset_secrets_for(&machine);
                        let dto = match unset_secrets_result {
                            Ok(unsets) => ChannelDTO::FoundUnsetSecrets(machine, unsets),
                            Err(_) => ChannelDTO::Default,
                        };
                        sender_clone.send((status_index, dto)).unwrap();
                    });
                }
                ChannelDTO::DuplicateFinished(duplicate_name) => {
                    intent.push(BackendIntent::DuplicatedMachine(duplicate_name))
                }
                ChannelDTO::DeleteFinished(machine_name) => {
                    intent.push(BackendIntent::DeletedMachine(machine_name))
                }
                ChannelDTO::FoundUnsetSecrets(machine, unsets) => {
                    for secret in &unsets {
                        dialog_manager.queue_generic(DialogIntent::Secret {
                            machine_name: machine.config.name.clone(),
                            name: secret.name.clone(),
                            value: "".to_string(),
                        })
                    }
                    let unsets_map = unsets
                        .into_iter()
                        .map(|secret| (secret.name, None))
                        .collect();
                    self.unset_secrets
                        .insert(machine.config.name.clone(), (machine, unsets_map));
                }
                ChannelDTO::WroteSecrets(mut machine) => {
                    // TODO: make it wörk async
                    if machine.build_install().is_ok() {
                        println!("wololo");
                    }
                }
                ChannelDTO::Default => {}
            }
        }

        intent
    }

    pub fn set_secret(
        &mut self,
        machine_name: String,
        secret_name: String,
        value: String,
        status_entries: &mut StatusEntries,
    ) {
        if let Some((mut machine, mut unset_secrets)) = self.unset_secrets.remove(&machine_name) {
            unset_secrets.insert(secret_name, Some(value));

            if !unset_secrets.values().contains(&None) {
                let status_index =
                    status_entries.create_entry(format!("Writing Secrets for '{}'", &machine_name));

                let sender_clone = self.sender.clone();
                thread::spawn(move || {
                    let dto = match write_secrets(&mut machine, unset_secrets) {
                        Ok(_) => ChannelDTO::WroteSecrets(machine),
                        Err(_) => ChannelDTO::Default,
                    };
                    sender_clone.send((status_index, dto)).unwrap();
                });
            } else {
                self.unset_secrets
                    .insert(machine_name, (machine, unset_secrets));
            }
        }
    }

    pub fn rebuild(
        &mut self,
        mut machine: Machine,
        update_modules: bool,
        status_entries: &mut StatusEntries,
    ) {
        let status_index =
            status_entries.create_entry(format!("Building Machine '{}'", machine.config.name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let dto = if machine.build(!update_modules).is_ok() {
                ChannelDTO::BuildFinished(machine)
            } else {
                ChannelDTO::Default
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn duplicate(
        &mut self,
        machine: Machine,
        duplicate_name: String,
        status_entries: &mut StatusEntries,
    ) {
        let status_index =
            status_entries.create_entry(format!("Duplicating Machine '{}'", machine.config.name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let dto = if machine.duplicate(&duplicate_name).is_ok() {
                ChannelDTO::DuplicateFinished(duplicate_name)
            } else {
                ChannelDTO::Default
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn tar(
        &mut self,
        machine: Machine,
        export_path: PathBuf,
        status_entries: &mut StatusEntries,
    ) {
        let status_index =
            status_entries.create_entry(format!("Exporting Machine '{}'", machine.config.name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            machine.tar(&export_path).unwrap();
            sender_clone
                .send((status_index, ChannelDTO::Default))
                .unwrap();
        });
    }

    pub fn stop(&mut self, machine: Machine, status_entries: &mut StatusEntries) {
        let status_index =
            status_entries.create_entry(format!("Stopping Machine '{}'", machine.config.name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            machine.stop(false).unwrap();
            sender_clone
                .send((status_index, ChannelDTO::Default))
                .unwrap();
        });
    }

    pub fn delete(&mut self, machine: Machine, status_entries: &mut StatusEntries) {
        let status_index =
            status_entries.create_entry(format!("Deleting Machine '{}'", machine.config.name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let machine_name = machine.config.name.clone();
            let dto = if machine.delete(true).is_ok() {
                ChannelDTO::DeleteFinished(machine_name)
            } else {
                ChannelDTO::Default
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }
}

enum ChannelDTO {
    BuildFinished(Machine),
    DuplicateFinished(String),
    DeleteFinished(String),

    FoundUnsetSecrets(Machine, Vec<EnvSecret>),
    WroteSecrets(Machine),

    Default,
}

pub enum BackendIntent {
    DuplicatedMachine(String),
    DeletedMachine(String),
}

fn get_unset_secrets_for(machine: &Machine) -> Result<Vec<EnvSecret>> {
    let mut all_secrets = machine.eval_env_secrets()?;
    let (_, cfg) = MachineConfig::open_existing(&machine.config.name, false)?;
    let set_secrets = cfg.secrets;

    all_secrets.retain(|name, _| !set_secrets.contains_key(name));

    Ok(all_secrets.into_values().collect())
}

fn write_secrets(machine: &mut Machine, secrets: HashMap<String, Option<String>>) -> Result<()> {
    let (lock, mut cfg) = MachineConfig::open_existing(&machine.config.name, true)?;
    for (name, value) in secrets {
        if let Some(val) = value {
            cfg.secrets.insert(name, val);
        }
    }

    cfg.write(lock)?;
    machine.config = cfg;

    Ok(())
}
