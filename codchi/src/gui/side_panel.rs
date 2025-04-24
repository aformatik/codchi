use std::{
    collections::{HashMap, VecDeque},
    sync::mpsc::{channel, Receiver, Sender},
    thread,
};

use egui::*;
use std::time::Instant;

use crate::platform::{Machine, PlatformStatus};

use super::util::{status_entries::StatusEntries, textures_manager::TexturesManager};

pub struct GuiSidePanel {
    machine_button_names: Vec<String>,
    machine_load_names: VecDeque<String>,
    machines: HashMap<String, Machine>,

    last_reload: Instant,
    reloading_machine_name: Option<String>,

    sender: Sender<(usize, Option<Machine>)>,
    receiver: Receiver<(usize, Option<Machine>)>,

    initialized: bool,
}

impl GuiSidePanel {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        Self {
            machine_button_names: Vec::new(),
            machine_load_names: VecDeque::new(),
            machines: HashMap::new(),

            last_reload: Instant::now(),
            reloading_machine_name: None,

            sender,
            receiver,

            initialized: false,
        }
    }

    pub fn update(
        &mut self,
        ui: &mut Ui,
        status_entries: &mut StatusEntries,
        textures_manager: &TexturesManager,
    ) -> Vec<SidePanelIntent> {
        if self.reloading_machine_name.is_none() {
            let now = Instant::now();
            if !self.initialized || now.duration_since(self.last_reload).as_secs() >= 10 {
                self.initialized = true;
                self.refresh_machine_list();
                self.reload_machine_from_list(status_entries);
            }
        }

        self.handle_received_msgs(status_entries);

        self.update_ui(ui, textures_manager)
    }

    fn update_ui(&self, ui: &mut Ui, textures_manager: &TexturesManager) -> Vec<SidePanelIntent> {
        let mut intent = Vec::new();

        ScrollArea::vertical().auto_shrink(false).show(ui, |ui| {
            ui.vertical_centered_justified(|ui| {
                ui.menu_button(RichText::new("New").heading(), |ui| {
                    if ui.button("Standard").clicked() {
                        intent.push(SidePanelIntent::BeginMachineCreation);
                        ui.close_menu();
                    }
                    if ui.button("Empty").clicked() {
                        intent.push(SidePanelIntent::BeginEmptyMachineCreation);
                        ui.close_menu();
                    }
                });
            });
            ui.separator();

            ui.horizontal_top(|ui| {
                ui.separator();
                ui.with_layout(Layout::top_down_justified(Align::LEFT), |ui| {
                    for machine_button_name in &self.machine_button_names {
                        if let Some(machine) = self.machines.get(machine_button_name) {
                            let icon_option = if self
                                .reloading_machine_name
                                .as_ref()
                                .is_some_and(|machine_name| &machine.config.name == machine_name)
                            {
                                None
                            } else {
                                match machine.platform_status {
                                    PlatformStatus::NotInstalled => {
                                        textures_manager.get_image("dark_red")
                                    }
                                    PlatformStatus::Stopped => textures_manager.get_image("red"),
                                    PlatformStatus::Running => textures_manager.get_image("green"),
                                }
                            };
                            let button_text = RichText::new(&machine.config.name).strong();
                            let machine_button = Button::opt_image_and_text(
                                icon_option,
                                Some(WidgetText::RichText(button_text)),
                            );
                            let button_handle = ui.add(machine_button);
                            if button_handle.clicked() {
                                intent.push(SidePanelIntent::DisplayMachine(machine.clone()));
                            }
                        }
                    }
                })
            });
        });

        intent
    }

    fn refresh_machine_list(&mut self) {
        if let Ok(machine_list) = Machine::list(false) {
            self.machine_load_names = machine_list
                .iter()
                .map(|machine| machine.config.name.clone())
                .collect();

            self.machine_button_names
                .retain(|name| self.machine_load_names.contains(name));
        }
    }

    fn reload_machine_from_list(&mut self, status_entries: &mut StatusEntries) {
        self.reloading_machine_name = self.machine_load_names.pop_front();

        if let Some(machine_name) = &self.reloading_machine_name {
            let status_index = status_entries
                .create_entry(format!("Updating status for machine '{}'", machine_name));

            let machine_name_clone = machine_name.clone();
            let sender_clone = self.sender.clone();
            thread::spawn(move || {
                let machine_result = Machine::by_name(&machine_name_clone, true);

                sender_clone
                    .send((status_index, machine_result.ok()))
                    .unwrap();
            });
        } else {
            self.last_reload = Instant::now();
        }
    }

    fn handle_received_msgs(&mut self, status_entries: &mut StatusEntries) {
        while let Ok((status_index, machine_option)) = self.receiver.try_recv() {
            status_entries.decrease(status_index);

            if let Some(machine) = machine_option {
                let machine_name = machine.config.name.clone();
                self.machines.insert(machine_name.clone(), machine);

                if let Err(pos) = self.machine_button_names.binary_search(&machine_name) {
                    self.machine_button_names.insert(pos, machine_name);
                }

                self.reload_machine_from_list(status_entries);
            }
        }
    }

    pub fn add_machine(&mut self, machine_name: String, status_entries: &mut StatusEntries) {
        self.machine_load_names.push_front(machine_name);
        self.reload_machine_from_list(status_entries);
    }

    pub fn remove_machine(&mut self, machine_name: String) {
        self.machine_load_names.retain(|name| name != &machine_name);
        self.machine_button_names
            .retain(|name| name != &machine_name);
    }
}

pub enum SidePanelIntent {
    DisplayMachine(Machine),
    BeginMachineCreation,
    BeginEmptyMachineCreation,
}
