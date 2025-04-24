use super::{AuthUrl, InternalIntent, RepositorySpecification};
use crate::cli::ModuleAttrPath;
use egui::*;
use std::collections::HashMap;

#[derive(Default)]
pub struct ModulesPanel {
    un_selected_modules: HashMap<(AuthUrl, RepositorySpecification), ModulePaths>,

    pub(super) current_repo: Option<(AuthUrl, RepositorySpecification)>,
    is_current_initialized: bool,
}

impl ModulesPanel {
    pub fn update(&mut self, ui: &mut Ui) -> Vec<InternalIntent> {
        self.update_panel(ui);

        self.update_buttons(ui)
    }

    fn update_panel(&mut self, ui: &mut Ui) {
        ui.heading("Repository Specification");
        if let Some((auth_url, repo_spec)) = &self.current_repo {
            ui.label(&auth_url.url);
            if !repo_spec.is_empty() {
                ui.label(repo_spec.to_full_text());
            }
        }
        ui.separator();

        if let Some(current_repo) = &self.current_repo {
            if let Some(module_paths) = self.un_selected_modules.get_mut(current_repo) {
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
    }

    fn update_buttons(&mut self, ui: &mut Ui) -> Vec<InternalIntent> {
        let mut intent = Vec::new();

        ui.with_layout(Layout::bottom_up(Align::Min), |ui| {
            ui.horizontal(|ui| {
                if ui.button("Previous").clicked() {
                    if let Some((auth_url, _)) = &self.current_repo {
                        intent.push(InternalIntent::ToRepositoryPanel(Some(auth_url.clone())));
                    }
                }
                ui.with_layout(Layout::right_to_left(Align::Max), |ui| {
                    let create_button = Button::new("Create").fill(Color32::DARK_GREEN);
                    if ui.add(create_button).clicked() {
                        intent.push(InternalIntent::CreateMachine);
                    }
                });
            });
        });

        intent
    }

    pub fn set_current_repo(&mut self, repo: (AuthUrl, RepositorySpecification)) {
        self.is_current_initialized = self.is_repo_loaded(&repo);
        self.current_repo = Some(repo);
    }

    pub fn is_current_repo_loaded(&self) -> bool {
        self.is_current_initialized
    }

    pub fn is_repo_loaded(&self, repo: &(AuthUrl, RepositorySpecification)) -> bool {
        self.un_selected_modules.contains_key(repo)
    }

    pub fn insert_modules(
        &mut self,
        repo: (AuthUrl, RepositorySpecification),
        modules: Vec<ModuleAttrPath>,
    ) {
        let value = ModulePaths::new(modules);
        self.un_selected_modules.insert(repo, value);
        self.is_current_initialized = true;
    }

    pub fn get_current_selected_modules(&self) -> Vec<ModuleAttrPath> {
        if let Some(current_repo) = &self.current_repo
            && let Some(module_paths) = self.un_selected_modules.get(current_repo)
        {
            module_paths.selected_module_paths.clone()
        } else {
            Vec::new()
        }
    }

    pub fn get_current_repo(&self) -> &Option<(AuthUrl, RepositorySpecification)> {
        &self.current_repo
    }
}

struct ModulePaths {
    unselected_module_paths: Vec<ModuleAttrPath>,
    selected_module_paths: Vec<ModuleAttrPath>,
}

impl ModulePaths {
    fn new(unselected_module_paths: Vec<ModuleAttrPath>) -> Self {
        Self {
            unselected_module_paths,
            selected_module_paths: Vec::new(),
        }
    }
}
