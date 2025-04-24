mod generics_panel;
mod modules_panel;
mod repository_panel;

use super::MainPanelIntent;
use crate::cli::{InputOptions, ModuleAttrPath, NixpkgsLocation};
use anyhow::Result;
use egui::*;
use generics_panel::GenericsPanel;
use git_url_parse::GitUrl;
use modules_panel::ModulesPanel;
use repository_panel::RepositoryPanel;
use strum::{EnumIter, IntoEnumIterator};

#[derive(Default)]
pub struct MachineCreation {
    creation_step: CreationStep,

    generics_panel: GenericsPanel,
    repository_panel: RepositoryPanel,
    modules_panel: ModulesPanel,
}

impl MachineCreation {
    pub fn update(&mut self, ui: &mut Ui) -> Vec<MainPanelIntent> {
        self.change_step(self.update_creation_step_buttons(ui));

        let intents = self.update_panel(ui);

        self.eval_intents(intents)
    }

    fn update_creation_step_buttons(&self, ui: &mut Ui) -> Option<CreationStep> {
        let (button_1_handle, button_2_handle, button_3_handle) = self.add_step_buttons(ui);

        if button_1_handle.clicked() {
            return Some(CreationStep::Generics);
        }
        if button_2_handle.clicked() {
            return Some(CreationStep::Repository);
        }
        if button_3_handle.clicked() {
            return Some(CreationStep::Modules);
        }

        None
    }

    fn add_step_buttons(&self, ui: &mut Ui) -> (Response, Response, Response) {
        let steps: Vec<CreationStep> = CreationStep::iter().collect();

        let button_1 = self.get_step_button(&steps[0]);
        let button_2 = self.get_step_button(&steps[1]);
        let button_3 = self.get_step_button(&steps[2]);

        let enabled_1 = self.is_step_reachable(&steps[0]);
        let enabled_2 = self.is_step_reachable(&steps[1]);
        let enabled_3 = self.is_step_reachable(&steps[2]);

        let width = (ui.available_width() - (2 as f32 * ui.spacing().item_spacing.x)) / 3 as f32;

        let result = ui
            .horizontal_top(|ui| {
                (
                    ui.add_enabled_ui(enabled_1, |ui| ui.add_sized([width, 0.0], button_1))
                        .inner,
                    ui.add_enabled_ui(enabled_2, |ui| ui.add_sized([width, 0.0], button_2))
                        .inner,
                    ui.add_enabled_ui(enabled_3, |ui| {
                        ui.add_sized([ui.available_width(), 0.0], button_3)
                    })
                    .inner,
                )
            })
            .inner;

        ui.separator();

        result
    }

    fn update_panel(&mut self, ui: &mut Ui) -> Vec<InternalIntent> {
        match self.creation_step {
            CreationStep::Generics => self.generics_panel.update(ui),
            CreationStep::Repository => self.repository_panel.update(ui),
            CreationStep::Modules => self.modules_panel.update(ui),
        }
    }

    fn get_step_button(&self, creation_step: &CreationStep) -> Button<'_> {
        let text = RichText::new(creation_step.to_text()).heading();
        if creation_step == &self.creation_step {
            Button::new(text).fill(Color32::DARK_GRAY)
        } else {
            Button::new(text)
        }
    }

    fn is_step_reachable(&self, creation_step: &CreationStep) -> bool {
        match &creation_step {
            CreationStep::Generics => true,
            CreationStep::Repository => self.repository_panel.is_current_auth_url_loaded(),
            CreationStep::Modules => self.modules_panel.is_current_repo_loaded(),
        }
    }

    fn change_step(&mut self, creation_step_option: Option<CreationStep>) {
        if let Some(creation_step) = creation_step_option {
            self.creation_step = creation_step;
        }
    }

    pub fn reset(&mut self) {
        *self = Self::default();
    }

    pub fn pass_url(&mut self, url: String) {
        self.reset();
        self.generics_panel.set_url(url);
    }

    pub fn is_auth_url_loaded(&self, auth_url: &AuthUrl) -> bool {
        self.repository_panel.is_auth_url_loaded(auth_url)
    }

    pub fn pass_branches(&mut self, auth_url: AuthUrl, branches: Vec<String>) {
        self.repository_panel
            .insert_branches(auth_url.clone(), branches);
        if self.repository_panel.is_auth_url_loaded(&auth_url) {
            self.to_repository_panel(Some(auth_url));
        }
    }

    pub fn pass_tags(&mut self, auth_url: AuthUrl, tags: Vec<String>) {
        self.repository_panel.insert_tags(auth_url.clone(), tags);
        if self.repository_panel.is_auth_url_loaded(&auth_url) {
            self.to_repository_panel(Some(auth_url));
        }
    }

    pub fn pass_modules(
        &mut self,
        repo: (AuthUrl, RepositorySpecification),
        modules: Vec<ModuleAttrPath>,
    ) {
        self.modules_panel.insert_modules(repo.clone(), modules);
        self.to_modules_panel(Some(repo));
    }

    pub fn to_repository_panel(&mut self, auth_url_option: Option<AuthUrl>) {
        if let Some(auth_url) = auth_url_option {
            self.repository_panel.set_current_auth_url(auth_url);
        }
        self.creation_step = CreationStep::Repository;
    }

    fn to_modules_panel(&mut self, repo_option: Option<(AuthUrl, RepositorySpecification)>) {
        if let Some(repo) = repo_option {
            self.modules_panel.set_current_repo(repo);
        }
        self.creation_step = CreationStep::Modules;
    }

    fn eval_intents(&mut self, intents: Vec<InternalIntent>) -> Vec<MainPanelIntent> {
        intents
            .into_iter()
            .filter_map(|intent| match intent {
                InternalIntent::ToGenericsPanel => {
                    self.creation_step = CreationStep::Generics;
                    None
                }
                InternalIntent::ToRepositoryPanel(auth_url_option) => {
                    if let Some(auth_url) = &auth_url_option {
                        if !self.repository_panel.is_auth_url_loaded(auth_url) {
                            return Some(MainPanelIntent::MachineCreation(
                                MachineCreationIntent::LoadRepository(auth_url.clone()),
                            ));
                        }
                    }
                    self.to_repository_panel(auth_url_option);
                    None
                }
                InternalIntent::ToModulesPanel(repo_option) => {
                    if let Some(repo) = &repo_option {
                        if !self.modules_panel.is_repo_loaded(repo) {
                            return Some(MainPanelIntent::MachineCreation(
                                MachineCreationIntent::LoadModules(repo.clone()),
                            ));
                        }
                    }
                    self.to_modules_panel(repo_option);
                    None
                }
                InternalIntent::CreateMachine => {
                    if self.modules_panel.get_current_repo().is_some() {
                        let temp = std::mem::take(self);

                        let modules = temp.modules_panel.get_current_selected_modules();
                        let (auth_url, repo_spec) = temp.modules_panel.current_repo.unwrap();

                        let use_nixpkgs = if temp.repository_panel.dont_use_nixpkgs {
                            Some(NixpkgsLocation::Local)
                        } else {
                            Some(NixpkgsLocation::Remote)
                        };
                        let auth = auth_url.get_auth();
                        let (branch, tag, commit) = repo_spec.to_triple();

                        let machine_name = temp.generics_panel.machine_name;
                        let git_url = auth_url.get_git_url().unwrap();
                        let options = InputOptions {
                            dont_prompt: true,
                            no_build: temp.generics_panel.dont_build,
                            use_nixpkgs,
                            auth,
                            branch,
                            tag,
                            commit,
                        };
                        let dont_run_init = temp.repository_panel.dont_run_init_script;

                        Some(MainPanelIntent::MachineCreation(
                            MachineCreationIntent::CreateMachine(
                                machine_name,
                                git_url,
                                options,
                                modules,
                                dont_run_init,
                            ),
                        ))
                    } else {
                        None
                    }
                }
            })
            .collect()
    }
}

#[derive(PartialEq, Clone, EnumIter, Default)]
enum CreationStep {
    #[default]
    Generics,
    Repository,
    Modules,
}

impl CreationStep {
    fn to_text(&self) -> String {
        match self {
            CreationStep::Generics => "General".to_string(),
            CreationStep::Repository => "Repository".to_string(),
            CreationStep::Modules => "Modules".to_string(),
        }
    }
}

pub enum MachineCreationIntent {
    CreateMachine(String, GitUrl, InputOptions, Vec<ModuleAttrPath>, bool),
    LoadRepository(AuthUrl),
    LoadModules((AuthUrl, RepositorySpecification)),
}

enum InternalIntent {
    ToGenericsPanel,
    ToRepositoryPanel(Option<AuthUrl>),
    ToModulesPanel(Option<(AuthUrl, RepositorySpecification)>),
    CreateMachine,
}

#[derive(Hash, PartialEq, Eq, Default, Clone)]
pub struct AuthUrl {
    pub url: String,
    pub username: String,
    pub password: String,
}

impl AuthUrl {
    pub fn get_auth(&self) -> Option<String> {
        if !self.username.is_empty() {
            Some(format!("{}:{}", &self.username, &self.password))
        } else {
            None
        }
    }

    pub fn get_git_url(&self) -> Result<GitUrl> {
        if let Some(auth) = self.get_auth() {
            let url_parts: Vec<&str> = self.url.split("://").collect();
            if url_parts.len() == 2 {
                return Ok(GitUrl::parse(&format!(
                    "{}://{}@{}",
                    url_parts[0], auth, url_parts[1]
                ))?);
            }
        }
        Ok(GitUrl::parse(&self.url)?)
    }

    pub fn to_string(&self) -> String {
        if let Some(auth_val) = self.get_auth() {
            let url_parts: Vec<&str> = self.url.split("://").collect();
            if url_parts.len() == 2 {
                return format!("{}://{}@{}", url_parts[0], auth_val, url_parts[1]);
            }
        }
        self.url.to_string()
    }
}

#[derive(EnumIter, Eq, PartialEq, Clone, Hash)]
pub enum RepositorySpecification {
    Branch(String),
    Tag(String),
    Commit(String),
}

impl Default for RepositorySpecification {
    fn default() -> Self {
        RepositorySpecification::Branch("".to_string())
    }
}

impl RepositorySpecification {
    fn to_text(&self) -> String {
        match self {
            RepositorySpecification::Branch(_) => "Branch".to_string(),
            RepositorySpecification::Tag(_) => "Tag".to_string(),
            RepositorySpecification::Commit(_) => "Commit".to_string(),
        }
    }

    fn to_full_text(&self) -> String {
        match self {
            RepositorySpecification::Branch(branch) => format!("Branch '{}'", branch),
            RepositorySpecification::Tag(tag) => format!("Tag '{}'", tag),
            RepositorySpecification::Commit(commit) => format!("Commit '{}'", commit),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            RepositorySpecification::Branch(branch) => branch.is_empty(),
            RepositorySpecification::Tag(tag) => tag.is_empty(),
            RepositorySpecification::Commit(commit) => commit.is_empty(),
        }
    }

    pub fn to_triple(&self) -> (Option<String>, Option<String>, Option<String>) {
        if self.is_empty() {
            (None, None, None)
        } else {
            match self {
                RepositorySpecification::Branch(branch) => (Some(branch.clone()), None, None),
                RepositorySpecification::Tag(tag) => (None, Some(tag.clone()), None),
                RepositorySpecification::Commit(commit) => (None, None, Some(commit.clone())),
            }
        }
    }
}
