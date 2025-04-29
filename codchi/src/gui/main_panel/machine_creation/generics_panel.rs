use super::{AuthUrl, InternalIntent};
use crate::gui::util::password_field;
use egui::*;

#[derive(Default)]
pub struct GenericsPanel {
    pub(super) machine_name: String,
    auth_url: AuthUrl,
    pub(super) dont_build: bool,

    occupied_loading_repo: bool,
    repo_inaccessible: bool,
}

impl GenericsPanel {
    pub fn update(&mut self, ui: &mut Ui) -> Vec<InternalIntent> {
        self.update_ui(ui);

        self.update_buttons(ui)
    }

    fn update_ui(&mut self, ui: &mut Ui) {
        ui.heading("Machine Creation");
        ui.separator();

        Grid::new("generics_grid").num_columns(2).show(ui, |ui| {
            ui.label("Machine Name");
            ui.add_sized(
                ui.available_size(),
                TextEdit::singleline(&mut self.machine_name).hint_text("My awesome Name"),
            );
            ui.end_row();

            ui.label("");
            ui.separator();
            ui.end_row();

            ui.label("URL");
            add_sized_colored(
                ui,
                &mut self.repo_inaccessible,
                ui.available_size(),
                TextEdit::singleline(&mut self.auth_url.url)
                    .hint_text("https://gitlab.example.com/my_repo"),
            );
            ui.end_row();

            ui.label("Username");
            add_sized_colored(
                ui,
                &mut self.repo_inaccessible,
                ui.available_size(),
                TextEdit::singleline(&mut self.auth_url.username),
            );
            ui.end_row();

            ui.label("Password");
            add_sized_colored(
                ui,
                &mut self.repo_inaccessible,
                ui.available_size(),
                password_field(&mut self.auth_url.password),
            );
            ui.end_row();

            ui.label("");
            ui.separator();
            ui.end_row();

            ui.checkbox(&mut self.dont_build, "Don't build");
        });
    }

    fn update_buttons(&mut self, ui: &mut Ui) -> Vec<InternalIntent> {
        let mut intent = Vec::new();

        ui.with_layout(Layout::bottom_up(Align::Max), |ui| {
            let button = Button::new("Next");
            let enabled = !self.occupied_loading_repo
                && !self.auth_url.url.is_empty()
                && !self.machine_name.is_empty();
            let button_handle = ui.add_enabled(enabled, button);
            if button_handle.clicked() {
                intent.push(InternalIntent::ToRepositoryPanel(Some(
                    self.auth_url.clone(),
                )));
                self.occupied_loading_repo = true;
            }
        });

        intent
    }

    pub fn set_url(&mut self, url: String) {
        self.auth_url.url = url;
    }

    pub fn finished_loading_repo(&mut self) {
        self.occupied_loading_repo = false;
    }

    pub fn set_repo_inaccessible(&mut self) {
        self.repo_inaccessible = true;
    }
}

fn add_sized_colored(
    ui: &mut Ui,
    condition: &mut bool,
    max_size: impl Into<Vec2>,
    widget: impl Widget,
) -> Response {
    ui.horizontal(|ui| {
        if *condition {
            ui.visuals_mut().widgets.inactive.bg_stroke.width = 1.0;
            ui.visuals_mut().widgets.inactive.bg_stroke.color = Color32::RED;
        }
        let handle = ui.add_sized(max_size, widget);
        if handle.gained_focus() {
            *condition = false;
        }
        handle
    })
    .inner
}
