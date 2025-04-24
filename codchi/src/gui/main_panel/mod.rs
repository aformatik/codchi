pub mod empty_machine_creation;
pub mod machine_creation;
pub mod machine_inspection;

use super::util::{
    dialog_manager::DialogManager, status_entries::StatusEntries, textures_manager::TexturesManager,
};
use egui::*;
use empty_machine_creation::{EmptyMachineCreation, EmptyMachineCreationIntent};
use machine_creation::{MachineCreation, MachineCreationIntent};
use machine_inspection::{MachineInspection, MachineInspectionIntent};

pub struct MainPanel {
    pub panels: MainPanels,
    current_main_panel_type: MainPanelType,
}

impl MainPanel {
    pub fn new() -> Self {
        Self {
            panels: MainPanels::default(),
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
        ScrollArea::both()
            .id_salt("main_panel_scroll")
            .auto_shrink(false)
            .show(ui, |ui| match self.current_main_panel_type {
                MainPanelType::MachineInspection => self.panels.get_machine_inspection().update(
                    ui,
                    status_entries,
                    textures_manager,
                ),
                MainPanelType::MachineCreation => self.panels.get_machine_creation().update(ui),
                MainPanelType::EmptyMachineCreation => {
                    self.panels.get_empty_machine_creation().update(ui)
                }
            })
            .inner
    }

    pub fn reset(&mut self) {
        match self.current_main_panel_type {
            MainPanelType::MachineInspection => self.panels.get_machine_inspection().reset(),
            MainPanelType::MachineCreation => self.panels.get_machine_creation().reset(),
            MainPanelType::EmptyMachineCreation => self.panels.get_empty_machine_creation().reset(),
        }

        self.change(MainPanelType::MachineInspection);
    }

    pub fn change(&mut self, main_panel_type: MainPanelType) {
        self.current_main_panel_type = main_panel_type;
    }
}

#[derive(Default)]
pub struct MainPanels {
    machine_inspection: Option<MachineInspection>,
    machine_creation: Option<MachineCreation>,
    empty_machine_creation: Option<EmptyMachineCreation>,
}

impl MainPanels {
    pub fn get_machine_inspection(&mut self) -> &mut MachineInspection {
        self.machine_inspection
            .get_or_insert(MachineInspection::new())
    }

    pub fn get_machine_creation(&mut self) -> &mut MachineCreation {
        self.machine_creation
            .get_or_insert(MachineCreation::default())
    }

    pub fn get_empty_machine_creation(&mut self) -> &mut EmptyMachineCreation {
        self.empty_machine_creation
            .get_or_insert(EmptyMachineCreation::default())
    }
}

pub enum MainPanelType {
    MachineInspection,
    MachineCreation,
    EmptyMachineCreation,
}

pub enum MainPanelIntent {
    MachineInspection(MachineInspectionIntent),
    MachineCreation(MachineCreationIntent),
    EmptyMachineCreation(EmptyMachineCreationIntent),
}
