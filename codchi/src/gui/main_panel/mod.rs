pub mod machine_inspection;

use super::util::{
    dialog_manager::DialogManager, status_entries::StatusEntries, textures_manager::TexturesManager,
};
use egui::*;
use machine_inspection::{MachineInspection, MachineInspectionIntent};

pub struct MainPanel {
    pub panels: MainPanels,
    current_main_panel_type: MainPanelType,
}

impl MainPanel {
    pub fn new() -> Self {
        Self {
            panels: MainPanels::new(),
            current_main_panel_type: MainPanelType::MachineInspection,
        }
    }

    pub fn update(
        &mut self,
        ui: &mut Ui,
        status_entries: &mut StatusEntries,
        dialog_manager: &mut DialogManager,
        textures_manager: &mut TexturesManager,
    ) -> Vec<MainPanelIntent> {
        match self.current_main_panel_type {
            MainPanelType::MachineInspection => self.panels.get_machine_inspection().update(
                ui,
                status_entries,
                dialog_manager,
                textures_manager,
            ),
        }
    }

    pub fn reset(&mut self) {
        match self.current_main_panel_type {
            MainPanelType::MachineInspection => self.panels.get_machine_inspection().reset(),
        }

        self.current_main_panel_type = MainPanelType::MachineInspection;
    }
}

pub struct MainPanels {
    machine_inspection: Option<MachineInspection>,
}

impl MainPanels {
    pub fn new() -> Self {
        Self {
            machine_inspection: None,
        }
    }

    pub fn get_machine_inspection(&mut self) -> &mut MachineInspection {
        self.machine_inspection
            .get_or_insert(MachineInspection::new())
    }
}

pub enum MainPanelType {
    MachineInspection,
}

pub enum MainPanelIntent {
    MachineInspection(MachineInspectionIntent),
}
