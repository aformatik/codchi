use crate::gui::MainPanel;
use crate::gui::MainPanelType;
use crate::platform::Machine;
use crate::platform::PlatformStatus;

pub struct MachineInspectionMainPanel {
    machine: Option<Machine>,
    next_panel_type: Option<MainPanelType>,
}

impl Default for MachineInspectionMainPanel {
    fn default() -> Self {
        MachineInspectionMainPanel {
            machine: None,
            next_panel_type: None,
        }
    }
}

impl MainPanel for MachineInspectionMainPanel {
    fn update(&mut self, ui: &mut egui::Ui) {
        if let Some(machine) = &self.machine {
            ui.heading(format!(
                "Machine - '{}'",
                machine.config.name
            ));
            ui.label(match machine.platform_status {
                PlatformStatus::NotInstalled => "not installed",
                PlatformStatus::Stopped => "not running",
                PlatformStatus::Running => "running",
            });
            ui.separator();
        }
    }

    fn modal_update(&mut self, ctx: &egui::Context) {}

    fn next_panel(&mut self) -> Option<MainPanelType> {
        self.next_panel_type.take()
    }

    fn pass_machine(&mut self, machine: Machine) {
        self.machine = Some(machine);
    }
}
