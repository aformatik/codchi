use crate::cli::{InputOptions, ModuleAttrPath, NixpkgsLocation};
use crate::gui::{MainPanel, MainPanelType};
use crate::platform::{Machine, NixDriver, Store};
use crate::util::LinuxPath;
use egui::*;
use git_url_parse::{GitUrl, GitUrlParseError};
use strum::{EnumIter, IntoEnumIterator};

use super::StatusEntries;
use anyhow::Error;
use std::{
    collections::VecDeque,
    fmt::Debug,
    process::Command,
    sync::{Arc, Mutex},
    thread,
};

pub struct MachineCreationMainPanel {
    status_text: StatusEntries,
    answer_queue: Arc<Mutex<VecDeque<(usize, ChannelDataType)>>>,

    creation_step: CreationStep,
    machine_form: MachineForm,
    next_panel_type: Option<MainPanelType>,

    url: String,
    user: String,
    password: String,
    use_nixpkgs: bool,

    branches: Option<Vec<String>>,
    tags: Option<Vec<String>>,

    git_ref: GitRef,
    branch: String,
    tag: String,
    commit: String,
}

#[derive(PartialEq, Clone, EnumIter)]
enum CreationStep {
    SpecifyGenerics,
    SpecifyRepository,
    SpecifyModules,
}

#[derive(Debug, PartialEq, Clone)]
struct MachineForm {
    name: String,
    git_url: GitUrl,
    options: InputOptions,
    do_clone: bool,
    module_paths: Option<ModulePaths>,
    dont_run_init: bool,
}

enum ChannelDataType {
    Access(String, Option<String>, bool),
    Branches(GitUrl, Result<Vec<String>, Error>),
    Tags(GitUrl, Result<Vec<String>, Error>),
    Modules(InputOptions, Result<Vec<ModuleAttrPath>, Error>),
    NewMachine(MachineForm, Machine),
    BuiltMachine(bool, Machine),
    ClearStatus,
}

#[derive(Debug, PartialEq, EnumIter, Clone, Default)]
enum GitRef {
    #[default]
    Branch,
    Tag,
    Commit,
}

#[derive(Debug, Clone, PartialEq)]
struct ModulePaths {
    unselected_module_paths: Vec<ModuleAttrPath>,
    selected_module_paths: Vec<ModuleAttrPath>,
}

impl Default for MachineCreationMainPanel {
    fn default() -> Self {
        MachineCreationMainPanel {
            status_text: StatusEntries::new(),
            answer_queue: Arc::new(Mutex::new(VecDeque::new())),

            creation_step: CreationStep::SpecifyGenerics,
            machine_form: MachineForm::default(),
            next_panel_type: None,

            url: String::from(""),
            user: String::from(""),
            password: String::from(""),
            use_nixpkgs: false,

            branches: None,
            tags: None,

            git_ref: GitRef::default(),
            branch: String::from(""),
            tag: String::from(""),
            commit: String::from(""),
        }
    }
}

impl MainPanel for MachineCreationMainPanel {
    fn update(&mut self, ui: &mut Ui) {
        let received_answer = if let Ok(mut answer_queue) = self.answer_queue.try_lock() {
            answer_queue.pop_front()
        } else {
            None
        };
        if let Some((index, data_type)) = received_answer {
            self.status_text.decrease(index);
            match data_type {
                ChannelDataType::Access(url, auth, accessible) => {
                    if accessible && self.url == url && self.get_auth() == auth {
                        if let Ok(git_url) = Self::get_git_url(&url, &auth) {
                            self.machine_form.git_url = git_url.clone();
                            self.machine_form.options.auth = auth.clone();

                            // load branches for repo
                            let branches_index = self.status_text.insert(
                                1,
                                String::from(format!("Loading branches for {}...", &self.url)),
                            );
                            let url_clone = url.clone();
                            let auth_clone = auth.clone();
                            let git_url_clone = git_url.clone();
                            let answer_queue_clone = self.answer_queue.clone();
                            thread::spawn(move || {
                                let result = Self::load_branches(&url_clone, &auth_clone);
                                answer_queue_clone.lock().unwrap().push_back((
                                    branches_index,
                                    ChannelDataType::Branches(git_url_clone, result),
                                ));
                            });

                            // load tags for repo
                            let tags_index = self.status_text.insert(
                                1,
                                String::from(format!("Loading tags for {}...", &self.url)),
                            );
                            let answer_queue_clone = self.answer_queue.clone();
                            thread::spawn(move || {
                                let result = Self::load_tags(&url, &auth);
                                answer_queue_clone.lock().unwrap().push_back((
                                    tags_index,
                                    ChannelDataType::Tags(git_url, result),
                                ));
                            });
                        }
                    }
                }
                ChannelDataType::Branches(git_url, branches) => {
                    if branches.is_ok() {
                        if git_url == self.machine_form.git_url {
                            self.branches = branches.ok();
                        }
                    }
                }
                ChannelDataType::Tags(git_url, tags) => {
                    if tags.is_ok() {
                        if git_url == self.machine_form.git_url {
                            self.tags = tags.ok();
                        }
                    }
                }
                ChannelDataType::Modules(options, result) => {
                    if let Ok(module_paths) = result {
                        self.machine_form.options = options;
                        self.machine_form.module_paths = Some(ModulePaths::new(module_paths));
                    }
                }
                ChannelDataType::NewMachine(machine_form, mut machine) => {
                    if self.machine_form == machine_form {
                        self.renew();
                    }
                    if !machine_form.options.no_build {
                        let index = self.status_text.insert(
                            1,
                            String::from(format!("Building machine '{}'...", machine.config.name)),
                        );

                        let answer_queue_clone = self.answer_queue.clone();
                        thread::spawn(move || {
                            let answer = if machine.build(false).is_ok() {
                                ChannelDataType::BuiltMachine(machine_form.dont_run_init, machine)
                            } else {
                                ChannelDataType::ClearStatus
                            };
                            answer_queue_clone
                                .lock()
                                .unwrap()
                                .push_back((index, answer));
                        });
                    }
                }
                ChannelDataType::BuiltMachine(dont_run_init, machine) => {
                    if !dont_run_init {
                        let index = self.status_text.insert(
                            1,
                            String::from(format!(
                                "Running Init-Script for '{}'...",
                                machine.config.name
                            )),
                        );

                        let answer_queue_clone = self.answer_queue.clone();
                        thread::spawn(move || {
                            let _ = machine.run_init_script(dont_run_init);
                            answer_queue_clone
                                .lock()
                                .unwrap()
                                .push_back((index, ChannelDataType::ClearStatus));
                        });
                    }
                }
                ChannelDataType::ClearStatus => {}
            }
        }

        ui.with_layout(Layout::right_to_left(Align::TOP), |ui| {
            ui.separator();
            ui.vertical(|ui| {
                self.creation_step_panel(ui);
                ui.separator();
                self.dialog_panel(ui);
            });
        });
    }

    fn modal_update(&mut self, _ctx: &Context) {}

    fn next_panel(&mut self) -> Option<MainPanelType> {
        self.next_panel_type.take()
    }

    fn pass_machine(&mut self, _machine: Machine) {}

    fn get_status_text(&self) -> &StatusEntries {
        &self.status_text
    }

    fn renew(&mut self) {
        self.creation_step = CreationStep::SpecifyGenerics;
        self.machine_form = MachineForm::default();
        self.url = String::from("");
        self.user = String::from("");
        self.password = String::from("");
        self.use_nixpkgs = false;
        self.branches = None;
        self.tags = None;
    }
}

impl MachineCreationMainPanel {
    pub fn creation_step_panel(&mut self, ui: &mut Ui) {
        ui.columns(CreationStep::iter().count(), |columns| {
            for creation_step in CreationStep::iter() {
                let i = creation_step.clone() as usize;
                columns[i].vertical_centered_justified(|ui| {
                    let text = RichText::from(format!("Step {}", (i + 1))).strong();
                    let button = if &self.creation_step == &creation_step {
                        Button::new(text).fill(Color32::GRAY)
                    } else {
                        Button::new(text)
                    };

                    let button_enabled = self.is_step_reachable(&creation_step);
                    let button_handle = ui.add_enabled(button_enabled, button);
                    if button_handle.clicked() {
                        self.creation_step = creation_step.clone();
                    }
                });
            }
        });
    }

    pub fn dialog_panel(&mut self, ui: &mut Ui) {
        match self.creation_step {
            CreationStep::SpecifyGenerics => {
                self.specify_generics_panel(ui);
            }
            CreationStep::SpecifyRepository => {
                self.specify_repository_panel(ui);
            }
            CreationStep::SpecifyModules => {
                self.specify_modules_panel(ui);
            }
        }
        ui.with_layout(Layout::bottom_up(Align::Center), |ui| {
            ui.separator();
            ui.horizontal(|ui| {
                if let Some(previous_step) = self.creation_step.previous() {
                    if ui.button(RichText::from("Previous").strong()).clicked() {
                        self.creation_step = previous_step;
                    }
                }
                ui.with_layout(Layout::right_to_left(Align::TOP), |ui| {
                    if let Some(next_step) = self.creation_step.next() {
                        if self.is_step_reachable(&next_step) {
                            let next_button = Button::new(RichText::from("Next").strong());
                            if ui.add(next_button).clicked() {
                                self.creation_step = next_step;
                            }
                        }
                        self.load_repo(ui);
                    } else {
                        // Last creation step
                        let text = RichText::from("Finish").strong();
                        let finish_button = Button::new(text).fill(Color32::DARK_GREEN);
                        if ui.add(finish_button).clicked() {
                            self.create_machine();
                        }
                    }
                })
            })
        });
    }

    fn create_machine(&mut self) {
        let index = self.status_text.insert(
            1,
            String::from(format!(
                "Creating new machine '{}'...",
                self.machine_form.name
            )),
        );

        let machine_form = self.machine_form.clone();
        let answer_queue_clone = self.answer_queue.clone();
        thread::spawn(move || {
            let machine = crate::module::init(
                &machine_form.name,
                Some(machine_form.git_url.clone()),
                &machine_form.options,
                &machine_form
                    .module_paths
                    .as_ref()
                    .unwrap()
                    .selected_module_paths,
            );
            let answer = if let Ok(new_machine) = dbg!(machine) {
                ChannelDataType::NewMachine(machine_form, new_machine)
            } else {
                ChannelDataType::ClearStatus
            };
            answer_queue_clone
                .lock()
                .unwrap()
                .push_back((index, answer));
        });
    }

    fn load_repo(&mut self, ui: &mut Ui) {
        match self.creation_step {
            CreationStep::SpecifyGenerics => {
                if ui.button("Load Repository").clicked() {
                    if !self.url.is_empty() {
                        self.git_ref = GitRef::default();
                        self.branches = None;
                        self.tags = None;
                        self.machine_form.options.branch = None;
                        self.machine_form.options.tag = None;
                        self.machine_form.options.commit = None;

                        let index = self.status_text.insert(
                            1,
                            String::from(format!("Reading repository {}...", &self.url)),
                        );

                        let url_clone = self.url.clone();
                        let auth_clone = self.get_auth();
                        let answer_queue_clone = self.answer_queue.clone();
                        thread::spawn(move || {
                            let result = Self::is_repo_accessible(&url_clone, &auth_clone);
                            answer_queue_clone.lock().unwrap().push_back((
                                index,
                                ChannelDataType::Access(url_clone, auth_clone, result.is_ok()),
                            ));
                        });
                    }
                }
            }
            CreationStep::SpecifyRepository => {
                if ui.button("Load Modules").clicked() {
                    let index = self.status_text.insert(
                        1,
                        String::from(format!("Loading modules for {}...", &self.url)),
                    );
                    let git_url_clone = self.machine_form.git_url.clone();
                    let use_nixpkgs = if self.use_nixpkgs {
                        Some(NixpkgsLocation::Remote)
                    } else {
                        Some(NixpkgsLocation::Local)
                    };
                    let auth = self.machine_form.options.auth.clone();
                    let (branch, tag, commit) = match self.git_ref {
                        GitRef::Branch => (Some(self.branch.clone()), None, None),
                        GitRef::Tag => (None, Some(self.tag.clone()), None),
                        GitRef::Commit => (None, None, Some(self.commit.clone())),
                    };
                    let answer_queue_clone = self.answer_queue.clone();
                    let opts = InputOptions {
                        dont_prompt: true,
                        no_build: self.machine_form.options.no_build,
                        use_nixpkgs,
                        auth,
                        branch,
                        tag,
                        commit,
                    };
                    thread::spawn(move || {
                        let modules_result = Self::load_modules(&git_url_clone, &opts);
                        answer_queue_clone
                            .lock()
                            .unwrap()
                            .push_back((index, ChannelDataType::Modules(opts, modules_result)));
                    });
                }
            }
            CreationStep::SpecifyModules => {}
        }
    }

    fn get_auth(&self) -> Option<String> {
        // uniform transformation for auth to prevent lock when calling "nix" commands
        Some(format!("{}:{}", &self.user, &self.password))
    }

    fn specify_generics_panel(&mut self, ui: &mut Ui) {
        ui.heading("Specify the url of the repository you would like to use");
        ui.separator();
        Grid::new("new_machine_grid_1")
            .num_columns(2)
            .show(ui, |ui| {
                ui.label("URL:\t");
                ui.add_sized(
                    ui.available_size(),
                    TextEdit::singleline(&mut self.url)
                        .hint_text("https://gitlab.example.com/my_repo"),
                );
                ui.end_row();

                ui.label("User:\t");
                ui.add_sized(ui.available_size(), TextEdit::singleline(&mut self.user));
                ui.end_row();

                ui.label("Password:\t");
                ui.add_sized(
                    ui.available_size(),
                    TextEdit::singleline(&mut self.password),
                );
                ui.end_row();

                ui.label("Machine Name:\t");
                ui.add_sized(
                    ui.available_size(),
                    TextEdit::singleline(&mut self.machine_form.name).hint_text("My awesome Name"),
                );
                ui.end_row();
            });
        ui.separator();
        ui.checkbox(
            &mut self.machine_form.options.no_build,
            "Don't build machine",
        );
        ui.checkbox(&mut self.machine_form.dont_run_init, "Skip init Script");
        if ui
            .checkbox(&mut self.use_nixpkgs, "Use provided package version")
            .clicked()
        {
            self.machine_form.options.use_nixpkgs = if self.use_nixpkgs {
                Some(NixpkgsLocation::Remote)
            } else {
                Some(NixpkgsLocation::Local)
            };
        }
    }

    fn specify_repository_panel(&mut self, ui: &mut Ui) {
        ui.heading("Specify the repository-details");
        ui.separator();
        ui.horizontal(|ui| {
            ComboBox::from_id_salt("git_ref_combo_box")
                .selected_text(self.git_ref.to_text())
                .show_ui(ui, |ui| {
                    for git_ref in GitRef::iter() {
                        let label = git_ref.to_text();
                        ui.selectable_value(&mut self.git_ref, git_ref, label);
                    }
                });
            match &self.git_ref {
                GitRef::Branch => {
                    if let Some(branches) = &self.branches {
                        ComboBox::from_id_salt("git_ref_branch_combo_box")
                            .selected_text(&self.branch)
                            .show_ui(ui, |ui| {
                                for branch in branches {
                                    ui.selectable_value(&mut self.branch, branch.clone(), branch);
                                }
                            });
                    }
                }
                GitRef::Tag => {
                    if let Some(tags) = &self.tags {
                        ComboBox::from_id_salt("git_ref_tag_combo_box")
                            .selected_text(&self.tag)
                            .show_ui(ui, |ui| {
                                for tag in tags {
                                    ui.selectable_value(&mut self.tag, tag.clone(), tag);
                                }
                            });
                    }
                }
                GitRef::Commit => {
                    ui.text_edit_singleline(&mut self.commit);
                }
            };
        });
        ui.separator();
        ui.checkbox(&mut self.machine_form.do_clone, "Clone Configuration");
    }

    fn specify_modules_panel(&mut self, ui: &mut Ui) {
        ui.heading("Specify the modules, your machine should have");
        ui.separator();
        if let Some(module_paths) = self.machine_form.module_paths.as_mut() {
            let num_cols = 2;
            ui.columns(num_cols, |columns| {
                fn add_column(
                    ui: &mut Ui,
                    text: &str,
                    module_path_vec: &mut Vec<ModuleAttrPath>,
                    pendant_vec: &mut Vec<ModuleAttrPath>,
                ) {
                    ui.vertical(|ui| {
                        ui.strong(text);
                        let mut clicked_index = None;
                        for i in 0..module_path_vec.len() {
                            let module_path = &module_path_vec[i];
                            let button_text =
                                format!("{}.{}", module_path.base, module_path.module);
                            if ui.button(button_text).clicked() {
                                clicked_index = Some(i);
                            }
                        }
                        if let Some(index) = clicked_index {
                            let unselected_module_path = module_path_vec.remove(index);
                            pendant_vec.push(unselected_module_path);
                        }
                    });
                }
                add_column(
                    &mut columns[0],
                    "Unselected Modules",
                    module_paths.unselected_module_paths.as_mut(),
                    module_paths.selected_module_paths.as_mut(),
                );
                add_column(
                    &mut columns[1],
                    "Selected Modules",
                    module_paths.selected_module_paths.as_mut(),
                    module_paths.unselected_module_paths.as_mut(),
                );
            });
        }
    }

    fn is_step_reachable(&self, creation_step: &CreationStep) -> bool {
        match creation_step {
            CreationStep::SpecifyGenerics => true,
            CreationStep::SpecifyRepository => self.tags.is_some() || self.branches.is_some(),
            CreationStep::SpecifyModules => self.machine_form.module_paths.is_some(),
        }
    }

    fn is_repo_accessible(url: &str, auth: &Option<String>) -> Result<Vec<ModuleAttrPath>, Error> {
        let opts = InputOptions {
            dont_prompt: true,
            no_build: true,
            use_nixpkgs: None,
            auth: auth.clone().or(Some(String::from(""))),
            branch: None,
            tag: None,
            commit: None,
        };
        let git_url = Self::get_git_url(url, auth)?;
        let flake_url = crate::module::inquire_module_url(&opts, &git_url, false)?;
        let dummy_path = LinuxPath(String::from(""));
        let nix_url = flake_url.to_nix_url(dummy_path);

        let modules = crate::platform::Driver::store()
            .cmd()
            .list_nixos_modules(&nix_url)?;

        Ok(modules)
    }

    fn load_branches(url: &str, auth: &Option<String>) -> Result<Vec<String>, Error> {
        let output = Command::new("git")
            .args(["ls-remote", "--heads", &Self::get_auth_url(url, auth)])
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

    fn load_tags(url: &str, auth: &Option<String>) -> Result<Vec<String>, Error> {
        let output = Command::new("git")
            .args(["ls-remote", "--tags", &Self::get_auth_url(url, auth)])
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

    fn load_modules(
        url: &GitUrl,
        opts: &crate::cli::InputOptions,
    ) -> Result<Vec<ModuleAttrPath>, Error> {
        let flake_url = crate::module::inquire_module_url(opts, url, false)?;
        let dummy_path = LinuxPath(String::from(""));
        let nix_url = flake_url.to_nix_url(dummy_path);
        let modules = crate::platform::Driver::store()
            .cmd()
            .list_nixos_modules(&nix_url)?;

        Ok(modules)
    }

    fn get_auth_url(url: &str, auth: &Option<String>) -> String {
        if let Some(auth_val) = auth {
            let url_parts: Vec<&str> = url.split("://").collect();
            if url_parts.len() == 2 {
                return format!("{}://{}@{}", url_parts[0], auth_val, url_parts[1]);
            }
        }
        url.to_string()
    }

    fn get_git_url(url: &str, auth: &Option<String>) -> Result<GitUrl, GitUrlParseError> {
        if let Some(auth_val) = auth {
            let url_parts: Vec<&str> = url.split("://").collect();
            if url_parts.len() == 2 {
                return GitUrl::parse(&format!("{}://{}@{}", url_parts[0], auth_val, url_parts[1]));
            }
        }
        GitUrl::parse(url)
    }
}

impl CreationStep {
    fn next(&self) -> Option<Self> {
        match self {
            CreationStep::SpecifyGenerics => Some(CreationStep::SpecifyRepository),
            CreationStep::SpecifyRepository => Some(CreationStep::SpecifyModules),
            CreationStep::SpecifyModules => None,
        }
    }

    fn previous(&self) -> Option<Self> {
        match self {
            CreationStep::SpecifyGenerics => None,
            CreationStep::SpecifyRepository => Some(CreationStep::SpecifyGenerics),
            CreationStep::SpecifyModules => Some(CreationStep::SpecifyRepository),
        }
    }
}

impl Default for MachineForm {
    fn default() -> Self {
        let options = InputOptions {
            dont_prompt: true,
            no_build: false,
            use_nixpkgs: None,
            auth: None,
            branch: None,
            tag: None,
            commit: None,
        };
        MachineForm {
            name: String::from(""),
            git_url: GitUrl::default(),
            options,
            do_clone: false,
            module_paths: None,
            dont_run_init: false,
        }
    }
}

impl GitRef {
    fn to_text(&self) -> String {
        match self {
            GitRef::Branch => String::from("Branch"),
            GitRef::Tag => String::from("Tag"),
            GitRef::Commit => String::from("Commit"),
        }
    }
}

impl ModulePaths {
    fn new(unselected_module_paths: Vec<ModuleAttrPath>) -> Self {
        Self {
            unselected_module_paths,
            selected_module_paths: Vec::new(),
        }
    }
}
