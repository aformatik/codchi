use crate::gui::MainPanel;
use crate::gui::MainPanelType;
use crate::platform::Machine;

pub struct MachineCreationMainPanel {
    machine_form: MachineForm,
    next_panel_type: Option<MainPanelType>,
}

struct MachineForm {
    name: String,
}

impl Default for MachineCreationMainPanel {
    fn default() -> Self {
        MachineCreationMainPanel {
            machine_form: MachineForm::default(),
            next_panel_type: None,
        }
    }
}

impl MainPanel for MachineCreationMainPanel {
    fn update(&mut self, ui: &mut egui::Ui) {
        if ui.button("Finish").clicked() {
            self.next_panel_type = Some(MainPanelType::MachineInspection);
        }
    }

    fn modal_update(&mut self, ctx: &egui::Context) {}

    fn next_panel(&mut self) -> Option<MainPanelType> {
        self.next_panel_type.take()
    }

    fn pass_machine(&mut self, machine: Machine) {}
}

impl Default for MachineForm {
    fn default() -> Self {
        MachineForm {
            name: String::from(""),
        }
    }
}
