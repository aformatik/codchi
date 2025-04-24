use egui::*;

use super::MainPanelIntent;

#[derive(Default)]
pub struct EmptyMachineCreation {
    machine_name: String,
    dont_build: bool,
}

impl EmptyMachineCreation {
    pub fn update(&mut self, ui: &mut Ui) -> Vec<MainPanelIntent> {
        self.update_ui(ui);

        self.update_buttons(ui)
    }

    fn update_ui(&mut self, ui: &mut Ui) {
        ui.heading("Empty Machine Creation");
        ui.label("");
        ui.separator();

        Grid::new("empty_generics_grid")
            .num_columns(2)
            .show(ui, |ui| {
                ui.label("Machine Name");
                ui.add_sized(
                    ui.available_size(),
                    TextEdit::singleline(&mut self.machine_name).hint_text("My awesome Name"),
                );
                ui.end_row();

                ui.label("");
                ui.separator();
                ui.end_row();

                ui.checkbox(&mut self.dont_build, "Don't build");
            });
    }

    fn update_buttons(&mut self, ui: &mut Ui) -> Vec<MainPanelIntent> {
        let mut intent = Vec::new();

        ui.with_layout(Layout::bottom_up(Align::Min), |ui| {
            ui.with_layout(Layout::right_to_left(Align::Max), |ui| {
                let create_button = Button::new("Create").fill(Color32::DARK_GREEN);
                if ui.add(create_button).clicked() {
                    let temp = std::mem::take(self);
                    intent.push(MainPanelIntent::EmptyMachineCreation(
                        EmptyMachineCreationIntent::CreateMachine(
                            temp.machine_name,
                            temp.dont_build,
                        ),
                    ));
                }
            });
        });

        intent
    }

    pub fn reset(&mut self) {
        *self = Self::default();
    }
}

pub enum EmptyMachineCreationIntent {
    CreateMachine(String, bool),
}
