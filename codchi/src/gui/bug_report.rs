use crate::gui::create_modal;
use crate::gui::MainPanel;
use crate::gui::MainPanelType;
use crate::platform::Machine;

pub struct BugReportMainPanel {
    show_modal: bool,
    bug_report: String,
    next_panel_type: Option<MainPanelType>,
}

impl Default for BugReportMainPanel {
    fn default() -> Self {
        BugReportMainPanel {
            show_modal: false,
            bug_report: String::from(""),
            next_panel_type: None,
        }
    }
}

impl MainPanel for BugReportMainPanel {
    fn update(&mut self, ui: &mut egui::Ui) {
        ui.add_sized(
            [ui.available_width(), 100.0],
            egui::TextEdit::multiline(&mut self.bug_report).hint_text("Bug Description"),
        );
        ui.separator();
        let send_button = egui::Button::new("Submit").fill(egui::Color32::GRAY);
        ui.horizontal(|ui| {
            if ui.add(send_button).clicked() {
                self.bug_report = String::from("");

                self.show_modal = true;
                self.next_panel_type = Some(MainPanelType::MachineInspection);
            }
            if ui.button("Cancel").clicked() {
                self.next_panel_type = Some(MainPanelType::MachineInspection);
            }
        });
    }

    fn modal_update(&mut self, ctx: &egui::Context) {
        create_modal(ctx, "bug_report_modal", &mut self.show_modal, |ui| {
            ui.strong("Bug Report was submitted.");
            ui.label("Thank you for your feedback.");
        });
    }

    fn next_panel(&mut self) -> Option<MainPanelType> {
        self.next_panel_type.take()
    }

    fn pass_machine(&mut self, machine: Machine) {}
}
