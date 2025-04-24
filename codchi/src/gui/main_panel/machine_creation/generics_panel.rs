use super::{AuthUrl, InternalIntent};
use crate::gui::util::password_field;
use egui::*;

#[derive(Default)]
pub struct GenericsPanel {
    pub(super) machine_name: String,
    auth_url: AuthUrl,
    pub(super) dont_build: bool,
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
            ui.add_sized(
                ui.available_size(),
                TextEdit::singleline(&mut self.auth_url.url)
                    .hint_text("https://gitlab.example.com/my_repo"),
            );
            ui.end_row();

            ui.label("Username");
            ui.add_sized(
                ui.available_size(),
                TextEdit::singleline(&mut self.auth_url.username),
            );
            ui.end_row();

            ui.label("Password");
            ui.add_sized(
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
            if ui.button("Next").clicked() {
                if !self.auth_url.url.is_empty() {
                    intent.push(InternalIntent::ToRepositoryPanel(Some(
                        self.auth_url.clone(),
                    )));
                }
            }
        });

        intent
    }

    pub fn set_url(&mut self, url: String) {
        self.auth_url.url = url;
    }
}
