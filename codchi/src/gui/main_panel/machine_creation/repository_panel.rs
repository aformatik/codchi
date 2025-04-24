use egui::*;
use std::collections::HashMap;
use strum::IntoEnumIterator;

use super::{AuthUrl, InternalIntent, RepositorySpecification};

#[derive(Default)]
pub struct RepositoryPanel {
    repo_infos: HashMap<AuthUrl, (Option<Vec<String>>, Option<Vec<String>>)>,

    repo_spec: RepositorySpecification,
    pub(super) dont_run_init_script: bool,
    pub(super) dont_use_nixpkgs: bool,

    current_auth_url: Option<AuthUrl>,
    is_current_initialized: bool,
}

impl RepositoryPanel {
    pub fn update(&mut self, ui: &mut Ui) -> Vec<InternalIntent> {
        self.update_ui(ui);

        self.update_buttons(ui)
    }

    pub fn update_ui(&mut self, ui: &mut Ui) {
        let Self {
            repo_infos,
            repo_spec: repo_specification,
            dont_run_init_script,
            dont_use_nixpkgs,
            current_auth_url,
            is_current_initialized: _,
        } = self;

        if let Some(auth_url) = current_auth_url {
            ui.heading("Repository Specification");
            ui.label(&auth_url.url);
            ui.separator();

            ui.with_layout(Layout::left_to_right(Align::Min), |ui| {
                ComboBox::from_id_salt("repo_specification_combobox")
                    .selected_text(repo_specification.to_text())
                    .width(150.0)
                    .show_ui(ui, |ui| {
                        for repo_spec in RepositorySpecification::iter() {
                            let text = repo_spec.to_text();
                            ui.selectable_value(repo_specification, repo_spec, text);
                        }
                    });

                match repo_specification {
                    RepositorySpecification::Branch(branch_name) => {
                        if let Some((branches_option, _)) = repo_infos.get(auth_url) {
                            if let Some(branches) = branches_option {
                                ComboBox::from_id_salt("branch_specification_combobox")
                                    .selected_text(branch_name.to_string())
                                    .width(ui.available_width())
                                    .show_ui(ui, |ui| {
                                        for branch in branches {
                                            ui.selectable_value(
                                                branch_name,
                                                branch.to_string(),
                                                branch,
                                            );
                                        }
                                    });
                            }
                        }
                    }
                    RepositorySpecification::Tag(tag_name) => {
                        if let Some((_, tags_option)) = repo_infos.get(auth_url) {
                            if let Some(tags) = tags_option {
                                ComboBox::from_id_salt("branch_specification_combobox")
                                    .selected_text(tag_name.to_string())
                                    .width(ui.available_width())
                                    .show_ui(ui, |ui| {
                                        for tag in tags {
                                            ui.selectable_value(tag_name, tag.to_string(), tag);
                                        }
                                    });
                            }
                        }
                    }
                    RepositorySpecification::Commit(commit) => {
                        let commit_line = TextEdit::singleline(commit).hint_text("Commit-Hash");
                        ui.add_sized([ui.available_width(), 0.0], commit_line);
                    }
                }
            });

            ui.separator();
            ui.checkbox(dont_run_init_script, "Skip Configuration's Init-Script");
            ui.checkbox(dont_use_nixpkgs, "Ignore provided package versions");
        }
    }

    fn update_buttons(&mut self, ui: &mut Ui) -> Vec<InternalIntent> {
        let mut intent = Vec::new();

        ui.with_layout(Layout::bottom_up(Align::Min), |ui| {
            ui.horizontal(|ui| {
                if ui.button("Previous").clicked() {
                    intent.push(InternalIntent::ToGenericsPanel);
                }
                ui.with_layout(Layout::right_to_left(Align::Max), |ui| {
                    if ui.button("Next").clicked() {
                        if let Some(current_auth_url) = &self.current_auth_url {
                            intent.push(InternalIntent::ToModulesPanel(Some((
                                current_auth_url.clone(),
                                self.repo_spec.clone(),
                            ))));
                        }
                    }
                });
            });
        });

        intent
    }

    pub fn set_current_auth_url(&mut self, auth_url: AuthUrl) {
        self.is_current_initialized = self.is_auth_url_loaded(&auth_url);
        self.current_auth_url = Some(auth_url);
    }

    pub fn insert_branches(&mut self, auth_url: AuthUrl, branches: Vec<String>) {
        let (branches_option, tags_option) =
            self.repo_infos.entry(auth_url).or_insert((None, None));
        *branches_option = Some(branches);

        self.is_current_initialized = branches_option.is_some() && tags_option.is_some();
    }

    pub fn insert_tags(&mut self, auth_url: AuthUrl, tags: Vec<String>) {
        let (branches_option, tags_option) =
            self.repo_infos.entry(auth_url).or_insert((None, None));
        *tags_option = Some(tags);

        self.is_current_initialized = branches_option.is_some() && tags_option.is_some();
    }

    pub fn is_current_auth_url_loaded(&self) -> bool {
        self.is_current_initialized
    }

    pub fn is_auth_url_loaded(&self, auth_url: &AuthUrl) -> bool {
        match self.repo_infos.get(auth_url) {
            Some((branches, tags)) => branches.is_some() && tags.is_some(),
            None => false,
        }
    }
}
