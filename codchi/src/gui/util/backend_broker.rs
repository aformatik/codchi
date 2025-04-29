use super::{
    dialog_manager::{DialogIntent, DialogManager},
    status_entries::StatusEntries,
};
use crate::{
    cli::{InputOptions, ModuleAttrPath},
    config::MachineConfig,
    gui::main_panel::machine_creation::{AuthUrl, RepositorySpecification},
    module,
    platform::{Machine, MachineDriver, NixDriver, Store},
    secrets::{EnvSecret, MachineSecrets},
    util::LinuxPath,
};
use anyhow::Result;
use git_url_parse::GitUrl;
use std::{
    collections::HashMap,
    path::PathBuf,
    process::Command,
    sync::mpsc::{channel, Receiver, Sender},
    thread,
};

pub struct BackendBroker {
    unresolved_machines: HashMap<String, (Machine, bool, HashMap<String, Option<String>>)>,

    sender: Sender<(usize, ChannelDTO)>,
    receiver: Receiver<(usize, ChannelDTO)>,
}

enum ChannelDTO {
    CreatedMachine(Machine, bool, bool),

    BuiltMachine(Machine, bool),
    DuplicatedMachine(String),
    DeletedMachine(String),

    RepositoryAccessed(AuthUrl, Result<()>),
    BranchesLoaded(AuthUrl, Result<Vec<String>>),
    TagsLoaded(AuthUrl, Result<Vec<String>>),
    ModulesLoaded(
        (AuthUrl, RepositorySpecification),
        Result<Vec<ModuleAttrPath>>,
    ),

    FoundUnsetSecrets(Machine, Vec<EnvSecret>, bool),
    WroteSecrets(Machine, bool),
    InstalledMachine(Machine, bool),

    Default,
}

pub enum BackendIntent {
    DuplicatedMachine(String),
    DeletedMachine(String),
    AccessedRepository(AuthUrl, Result<()>),
    LoadedBranches(AuthUrl, Result<Vec<String>>),
    LoadedTags(AuthUrl, Result<Vec<String>>),
    LoadedModules(
        (AuthUrl, RepositorySpecification),
        Result<Vec<ModuleAttrPath>>,
    ),
}

impl BackendBroker {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        Self {
            unresolved_machines: HashMap::new(),

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
                ChannelDTO::CreatedMachine(machine, dont_build, dont_run_init) => {
                    if !dont_build {
                        self.rebuild_machine(machine, true, dont_run_init, status_entries);
                    }
                }
                ChannelDTO::BuiltMachine(machine, dont_run_init) => {
                    self.get_unset_secrets(machine, dont_run_init, status_entries);
                }
                ChannelDTO::DuplicatedMachine(duplicate_name) => {
                    intent.push(BackendIntent::DuplicatedMachine(duplicate_name));
                }
                ChannelDTO::DeletedMachine(machine_name) => {
                    intent.push(BackendIntent::DeletedMachine(machine_name));
                }
                ChannelDTO::RepositoryAccessed(auth_url, result) => {
                    intent.push(BackendIntent::AccessedRepository(auth_url, result));
                }
                ChannelDTO::BranchesLoaded(auth_url, branches) => {
                    intent.push(BackendIntent::LoadedBranches(auth_url, branches));
                }
                ChannelDTO::TagsLoaded(auth_url, tags) => {
                    intent.push(BackendIntent::LoadedTags(auth_url, tags));
                }
                ChannelDTO::ModulesLoaded(repo, modules) => {
                    intent.push(BackendIntent::LoadedModules(repo, modules));
                }
                ChannelDTO::FoundUnsetSecrets(machine, unsets, dont_run_init) => {
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
                    self.unresolved_machines.insert(
                        machine.config.name.clone(),
                        (machine, dont_run_init, unsets_map),
                    );
                }
                ChannelDTO::WroteSecrets(machine, dont_run_init) => {
                    self.install_machine(machine, dont_run_init, status_entries);
                }
                ChannelDTO::InstalledMachine(machine, dont_run_init) => {
                    if !dont_run_init {
                        self.run_init_script(machine, dont_run_init, status_entries);
                    }
                }
                ChannelDTO::Default => {}
            }
        }

        intent
    }

    pub fn create_empty_machine(
        &mut self,
        machine_name: String,
        dont_build: bool,
        status_entries: &mut StatusEntries,
    ) {
        let status_index =
            status_entries.create_entry(format!("Creating empty machine '{}'", &machine_name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let dto = match module::init(&machine_name, None, &InputOptions::default(), &Vec::new())
            {
                Ok(machine) => ChannelDTO::CreatedMachine(machine, dont_build, false),
                Err(_) => ChannelDTO::Default,
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn create_machine(
        &mut self,
        machine_name: String,
        git_url: GitUrl,
        options: InputOptions,
        modules: Vec<ModuleAttrPath>,
        dont_run_init: bool,
        status_entries: &mut StatusEntries,
    ) {
        let status_index =
            status_entries.create_entry(format!("Creating machine '{}'", &machine_name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let dto = match module::init(&machine_name, Some(git_url), &options, &modules) {
                Ok(machine) => ChannelDTO::CreatedMachine(machine, options.no_build, dont_run_init),
                Err(_) => ChannelDTO::Default,
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn rebuild_machine(
        &mut self,
        mut machine: Machine,
        update_modules: bool,
        dont_run_init: bool,
        status_entries: &mut StatusEntries,
    ) {
        let status_index =
            status_entries.create_entry(format!("Building Machine '{}'", machine.config.name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let dto = if machine.build(!update_modules).is_ok() {
                ChannelDTO::BuiltMachine(machine, dont_run_init)
            } else {
                ChannelDTO::Default
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn duplicate_machine(
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
                ChannelDTO::DuplicatedMachine(duplicate_name)
            } else {
                ChannelDTO::Default
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn tar_machine(
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

    pub fn stop_machine(&mut self, machine: Machine, status_entries: &mut StatusEntries) {
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

    pub fn delete_machine(&mut self, machine: Machine, status_entries: &mut StatusEntries) {
        let status_index =
            status_entries.create_entry(format!("Deleting Machine '{}'", machine.config.name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let machine_name = machine.config.name.clone();
            let dto = if machine.delete(true).is_ok() {
                ChannelDTO::DeletedMachine(machine_name)
            } else {
                ChannelDTO::Default
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn access_repository(&mut self, auth_url: AuthUrl, status_entries: &mut StatusEntries) {
        let status_index =
            status_entries.create_entry(format!("Accessing Repository {}", auth_url.url));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let dto = match access_repo(&auth_url) {
                Ok(_) => ChannelDTO::RepositoryAccessed(auth_url, Ok(())),
                Err(err) => ChannelDTO::RepositoryAccessed(auth_url, Err(err)),
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn load_repository(&mut self, auth_url: AuthUrl, status_entries: &mut StatusEntries) {
        let status_index = status_entries.push(format!("Loading Repository {}", auth_url.url), 2);

        let sender_clone = self.sender.clone();
        let auth_url_clone = auth_url.clone();
        thread::spawn(move || {
            let branches_result = load_branches(&auth_url_clone);
            let dto = ChannelDTO::BranchesLoaded(auth_url_clone, branches_result);
            sender_clone.send((status_index, dto)).unwrap();
        });

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let tags_result = load_tags(&auth_url);
            let dto = ChannelDTO::TagsLoaded(auth_url, tags_result);
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn load_modules(
        &mut self,
        repo: (AuthUrl, RepositorySpecification),
        status_entries: &mut StatusEntries,
    ) {
        let status_index =
            status_entries.create_entry(format!("Loading Modules for {}", repo.0.url));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let modules_result = load_modules(&repo);

            let dto = ChannelDTO::ModulesLoaded(repo, modules_result);
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    fn get_unset_secrets(
        &mut self,
        machine: Machine,
        dont_run_init: bool,
        status_entries: &mut StatusEntries,
    ) {
        let status_index = status_entries.create_entry(format!(
            "Getting unset secrets of '{}'",
            &machine.config.name
        ));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let unset_secrets_result = get_unset_secrets_for(&machine);
            let dto = match unset_secrets_result {
                Ok(unsets) => {
                    if unsets.is_empty() {
                        ChannelDTO::WroteSecrets(machine, dont_run_init)
                    } else {
                        ChannelDTO::FoundUnsetSecrets(machine, unsets, dont_run_init)
                    }
                }
                Err(_) => ChannelDTO::Default,
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    fn install_machine(
        &mut self,
        mut machine: Machine,
        dont_run_init: bool,
        status_entries: &mut StatusEntries,
    ) {
        let status_index =
            status_entries.create_entry(format!("Installing machine '{}'", &machine.config.name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let install_result = machine.build_install();
            let dto = match install_result {
                Ok(_) => ChannelDTO::InstalledMachine(machine, dont_run_init),
                Err(_) => ChannelDTO::Default,
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    fn run_init_script(
        &mut self,
        machine: Machine,
        dont_run_init: bool,
        status_entries: &mut StatusEntries,
    ) {
        let status_index = status_entries.create_entry(format!(
            "Running Init-Script for '{}'",
            &machine.config.name
        ));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let _ = machine.run_init_script(dont_run_init);
            let dto = ChannelDTO::Default;
            sender_clone.send((status_index, dto)).unwrap();
        });
    }

    pub fn insert_secret(
        &mut self,
        machine_name: String,
        secret_name: String,
        value: String,
        status_entries: &mut StatusEntries,
    ) {
        if let Some((_, _, unset_secrets)) = self.unresolved_machines.get_mut(&machine_name) {
            unset_secrets.insert(secret_name, Some(value));

            if unset_secrets.values().all(|secret| secret.is_some()) {
                if let Some(entry) = self.unresolved_machines.remove(&machine_name) {
                    self.write_unset_secrets(entry, status_entries);
                }
            }
        }
    }

    fn write_unset_secrets(
        &mut self,
        entry: (Machine, bool, HashMap<String, Option<String>>),
        status_entries: &mut StatusEntries,
    ) {
        let (mut machine, dont_run_init, unset_secrets) = entry;
        let status_index =
            status_entries.create_entry(format!("Writing Secrets for '{}'", &machine.config.name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let dto = match write_secrets(&mut machine, unset_secrets) {
                Ok(_) => ChannelDTO::WroteSecrets(machine, dont_run_init),
                Err(_) => ChannelDTO::Default,
            };
            sender_clone.send((status_index, dto)).unwrap();
        });
    }
}

fn access_repo(auth_url: &AuthUrl) -> Result<Vec<ModuleAttrPath>> {
    let opts = InputOptions {
        dont_prompt: true,
        no_build: true,
        use_nixpkgs: None,
        auth: auth_url.get_auth().or(Some(String::from(":"))),
        branch: None,
        tag: None,
        commit: None,
    };
    let git_url = auth_url.get_git_url()?;
    let flake_url = crate::module::inquire_module_url(&opts, &git_url, false)?;
    let dummy_path = LinuxPath(String::from(""));
    let nix_url = flake_url.to_nix_url(dummy_path);

    let modules = crate::platform::Driver::store()
        .cmd()
        .list_nixos_modules(&nix_url)?;

    Ok(modules)
}

fn load_branches(auth_url: &AuthUrl) -> Result<Vec<String>> {
    let output = Command::new("git")
        .args(["ls-remote", "--heads", &auth_url.to_string()])
        .output()?;

    let branches = String::from_utf8_lossy(&output.stdout)
        .to_string()
        .lines()
        .filter_map(|line| line.split('\t').nth(1))
        .filter_map(|ref_name| ref_name.strip_prefix("refs/heads/"))
        .map(String::from)
        .collect();

    Ok(branches)
}

fn load_tags(auth_url: &AuthUrl) -> Result<Vec<String>> {
    let output = Command::new("git")
        .args(["ls-remote", "--tags", &auth_url.to_string()])
        .output()?;

    let tags = String::from_utf8_lossy(&output.stdout)
        .to_string()
        .lines()
        .filter_map(|line| line.split('\t').nth(1))
        .filter_map(|ref_name| ref_name.strip_prefix("refs/tags/"))
        .map(String::from)
        .collect();

    Ok(tags)
}

fn load_modules(repo: &(AuthUrl, RepositorySpecification)) -> Result<Vec<ModuleAttrPath>> {
    let (auth_url, repo_spec) = repo;

    let git_url = auth_url.get_git_url()?;
    let (branch, tag, commit) = repo_spec.to_triple();
    let opts = InputOptions {
        dont_prompt: true,
        auth: auth_url.get_auth(),
        no_build: false,
        use_nixpkgs: None,
        branch,
        tag,
        commit,
    };

    let flake_url = crate::module::inquire_module_url(&opts, &git_url, false)?;
    let dummy_path = LinuxPath(String::from(""));
    let nix_url = flake_url.to_nix_url(dummy_path);
    let modules = crate::platform::Driver::store()
        .cmd()
        .list_nixos_modules(&nix_url)?;

    Ok(modules)
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
