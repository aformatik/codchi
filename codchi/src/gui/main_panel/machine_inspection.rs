use super::{
    super::util::{
        dialog_manager::DialogManager, status_entries::StatusEntries,
        textures_manager::TexturesManager,
    },
    MainPanelIntent,
};
use crate::{
    config::Mod,
    gui::util::advanced_password_field,
    logging::CodchiOutput,
    platform::{platform::HostImpl, DesktopEntry, Host, Machine},
    secrets::{EnvSecret, MachineSecrets},
};
use anyhow::Result;
use egui::*;
use itertools::Itertools;
use std::collections::HashMap;
use std::{
    sync::mpsc::{channel, Receiver, Sender},
    thread,
};

pub struct MachineInspection {
    machine_data_map: HashMap<String, MachineData>,
    current_machine: Option<String>,

    sender: Sender<(usize, ChannelDTO)>,
    receiver: Receiver<(usize, ChannelDTO)>,
}

enum ChannelDTO {
    Machine(String, Result<Machine>),
    Applications(String, Result<Vec<DesktopEntry>>),
    Modules(String, Result<Vec<Mod>>),
    Secrets(String, Result<Vec<EnvSecret>>),
}

pub enum MachineInspectionIntent {
    Rebuild(Machine),
    Duplicate(Machine),
    Tar(Machine),
    Stop(Machine),
    Delete(Machine),
}

impl MachineInspection {
    pub fn update(
        &mut self,
        ui: &mut Ui,
        status_entries: &mut StatusEntries,
        _dialog_manager: &mut DialogManager,
        textures_manager: &mut TexturesManager,
    ) -> Vec<MainPanelIntent> {
        self.receive_msgs(status_entries);

        self.update_ui(ui, status_entries, textures_manager)
            .into_iter()
            .map(|machine_inspection_intent| {
                MainPanelIntent::MachineInspection(machine_inspection_intent)
            })
            .collect_vec()
    }

    pub fn reset(&mut self) {
        if let Some(current_machine) = &self.current_machine {
            self.machine_data_map.remove(current_machine);
        }

        self.current_machine = None;
    }

    fn receive_msgs(&mut self, status_entries: &mut StatusEntries) {
        while let Ok((status_index, channel_dto)) = self.receiver.try_recv() {
            status_entries.decrease(status_index);

            match channel_dto {
                ChannelDTO::Machine(machine_name, machine_result) => match machine_result {
                    Ok(machine) => {
                        let machine_data = MachineData::create(machine.clone());
                        self.machine_data_map.insert(machine_name, machine_data);

                        self.load_machine_data(machine, status_entries);
                    }
                    Err(_error) => self.load_machine(machine_name, status_entries),
                },
                ChannelDTO::Applications(machine_name, applications_result) => {
                    if let Some(machine_data_entry) = self.machine_data_map.get_mut(&machine_name) {
                        match applications_result {
                            Ok(applications) => machine_data_entry.set_applications(applications),
                            Err(_error) => machine_data_entry.set_applications(Vec::new()),
                        }
                    }
                }
                ChannelDTO::Modules(machine_name, modules_result) => {
                    if let Some(machine_data_entry) = self.machine_data_map.get_mut(&machine_name) {
                        match modules_result {
                            Ok(modules) => machine_data_entry.set_modules(modules),
                            Err(_error) => machine_data_entry.set_modules(Vec::new()),
                        }
                    }
                }
                ChannelDTO::Secrets(machine_name, secrets_result) => {
                    if let Some(machine_data_entry) = self.machine_data_map.get_mut(&machine_name) {
                        match secrets_result {
                            Ok(secrets) => machine_data_entry.set_secrets(secrets),
                            Err(_error) => machine_data_entry.set_secrets(Vec::new()),
                        }
                    }
                }
            }
        }
    }

    pub fn new() -> Self {
        let (sender, receiver) = channel();
        Self {
            machine_data_map: HashMap::new(),
            current_machine: None,

            sender,
            receiver,
        }
    }

    pub fn change(&mut self, machine: Machine, status_entries: &mut StatusEntries) {
        let machine_name = machine.config.name.clone();

        if !self.machine_data_map.contains_key(&machine_name) {
            self.machine_data_map
                .insert(machine_name.clone(), MachineData::create(machine));
        }

        let machine_data_option = self.machine_data_map.get_mut(&machine_name);
        if machine_data_option.is_some_and(|machine_data| !machine_data.is_initialized()) {
            self.load_machine(machine_name.clone(), status_entries)
        }

        self.current_machine = Some(machine_name);
    }

    fn load_machine(&self, machine_name: String, status_entries: &mut StatusEntries) {
        let status_index =
            status_entries.create_entry(format!("Loading Machine '{}'...", &machine_name));

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let machine = Machine::by_name(&machine_name, true);

            sender_clone
                .send((status_index, ChannelDTO::Machine(machine_name, machine)))
                .unwrap();
        });
    }

    fn load_machine_data(&self, machine: Machine, status_entries: &mut StatusEntries) {
        let status_index =
            status_entries.push(format!("Loading Data for '{}'...", &machine.config.name), 3);

        let machine_clone = machine.clone();
        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let applications = HostImpl::list_desktop_entries(&machine_clone);

            sender_clone
                .send((
                    status_index,
                    ChannelDTO::Applications(machine_clone.config.name, applications),
                ))
                .unwrap();
        });

        let machine_name_clone = machine.config.name.clone();
        let modules_clone = machine.config.modules.clone();
        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let modules = modules_clone.to_output();

            sender_clone
                .send((
                    status_index,
                    ChannelDTO::Modules(machine_name_clone, Ok(modules)),
                ))
                .unwrap();
        });

        let sender_clone = self.sender.clone();
        thread::spawn(move || {
            let secrets = machine
                .eval_env_secrets()
                .map(|secret_map| secret_map.into_values().collect_vec().to_output());

            sender_clone
                .send((
                    status_index,
                    ChannelDTO::Secrets(machine.config.name, secrets),
                ))
                .unwrap();
        });
    }

    fn update_ui(
        &mut self,
        ui: &mut Ui,
        status_entries: &mut StatusEntries,
        textures_manager: &mut TexturesManager,
    ) -> Vec<MachineInspectionIntent> {
        let mut intent = Vec::new();

        let mut machine_menu_intent = self.update_machine_menu(ui, status_entries);
        intent.append(&mut machine_menu_intent);

        if let Some(machine_name) = &self.current_machine {
            let machine_data = &self.machine_data_map[machine_name];
            let status_text = match machine_data.machine.platform_status {
                crate::platform::PlatformStatus::NotInstalled => "Not Installed",
                crate::platform::PlatformStatus::Stopped => "Stopped",
                crate::platform::PlatformStatus::Running => "Running",
            };
            ui.label(status_text);
            ui.separator();

            ui.heading("Applications");
            ui.add_space(3.0);
            if let Some(desktop_entries) = &machine_data.applications {
                for desktop_entry in desktop_entries {
                    let app_icon = match &desktop_entry.icon {
                        Some(icon_path) => textures_manager
                            .deliver_image(&desktop_entry.app_name, icon_path)
                            .map(|tex| {
                                Image::from_texture(tex).max_size(Vec2 { x: 12.0, y: 12.0 })
                            }),
                        None => None,
                    };
                    let app_text = WidgetText::RichText(RichText::new(&desktop_entry.app_name));
                    let app_button = Button::opt_image_and_text(app_icon, Some(app_text));
                    if ui.add(app_button).clicked() {
                        let _ =
                            HostImpl::execute(&machine_data.machine.config.name, &desktop_entry);
                    }
                }
                if ui.button("Shell").clicked() {
                    let _ = crate::platform::Driver::host().open_terminal(&[
                        &std::env::current_exe().unwrap().display().to_string(),
                        "exec",
                        &machine_data.machine.config.name,
                    ]);
                }
            } else {
                ui.horizontal(|ui| {
                    ui.add_space(20.0);
                    ui.label("Loading ...");
                });
            }
            ui.separator();

            ui.heading("Modules");
            if let Some(modules) = &machine_data.modules {
                if !modules.is_empty() {
                    Grid::new("modules_grid").show(ui, |ui| {
                        ui.strong("Name\t");
                        ui.strong("URL\t");
                        ui.strong("Path\t");
                        ui.end_row();

                        for module in modules {
                            ui.label(format!("{}\t", module.name));
                            ui.label(format!("{}\t", module.url));
                            ui.label(format!("{}\t", module.flake_module));
                            ui.end_row();
                        }
                    });
                } else {
                    ui.horizontal(|ui| {
                        ui.label("No modules");
                    });
                }
            } else {
                ui.horizontal(|ui| {
                    ui.add_space(20.0);
                    ui.label("Loading ...");
                });
            }
            ui.separator();

            ui.heading("Secrets");
            if let Some(secrets) = &machine_data.secrets {
                if !secrets.is_empty() {
                    Grid::new("secrets_grid").show(ui, |ui| {
                        ui.strong("Name\t");
                        ui.strong("Description\t");
                        ui.strong("Value\t");
                        ui.end_row();

                        for secret in secrets {
                            ui.label(format!("{}\t", secret.name));
                            ui.label(format!("{}\t", secret.description));
                            ui.add(advanced_password_field(
                                &format!("{}_{}", &machine_name, &secret.name),
                                |new_password| {
                                    let (lock, mut cfg) =
                                        crate::config::MachineConfig::open_existing(
                                            machine_name,
                                            true,
                                        )
                                        .unwrap();
                                    cfg.secrets.insert(secret.name.clone(), new_password);
                                    cfg.write(lock).unwrap();
                                },
                                &if secret.value.is_some() {
                                    secret.value.clone().unwrap()
                                } else {
                                    String::from("")
                                },
                            ));
                            ui.end_row();
                        }
                    });
                } else {
                    ui.horizontal(|ui| {
                        ui.label("No secrets");
                    });
                }
            } else {
                ui.horizontal(|ui| {
                    ui.add_space(20.0);
                    ui.label("Loading ...");
                });
            }
        }

        intent
    }

    fn update_machine_menu(
        &mut self,
        ui: &mut Ui,
        status_entries: &mut StatusEntries,
    ) -> Vec<MachineInspectionIntent> {
        let mut intent = Vec::new();

        if let Some(machine_name) = &self.current_machine {
            ui.horizontal(|ui| {
                let title_text = format!("Machine: {}", &machine_name);
                ui.add(widgets::Label::new(
                    RichText::from(title_text).heading().strong(),
                ));
                ui.with_layout(Layout::right_to_left(Align::BOTTOM), |ui| {
                    ui.menu_button("Actions", |ui| {
                        if ui.button("Rebuild").clicked() {
                            let machine_clone = self.machine_data_map[machine_name].machine.clone();
                            intent.push(MachineInspectionIntent::Rebuild(machine_clone));
                        }

                        if ui.button("Duplicate").clicked() {
                            let machine_clone = self.machine_data_map[machine_name].machine.clone();
                            intent.push(MachineInspectionIntent::Duplicate(machine_clone));
                        }

                        if ui.button("Export").clicked() {
                            let machine_clone = self.machine_data_map[machine_name].machine.clone();
                            intent.push(MachineInspectionIntent::Tar(machine_clone));
                        }

                        if ui.button("Stop").clicked() {
                            let machine_clone = self.machine_data_map[machine_name].machine.clone();
                            intent.push(MachineInspectionIntent::Stop(machine_clone));
                        }

                        let delete_button = Button::new("Delete").fill(Color32::DARK_RED);
                        if ui.add(delete_button).clicked() {
                            let machine_clone = self.machine_data_map[machine_name].machine.clone();
                            intent.push(MachineInspectionIntent::Delete(machine_clone));
                        }
                    });
                    if ui.button("\u{21BA}").on_hover_text("Reload").clicked() {
                        self.load_machine(machine_name.clone(), status_entries);
                    }
                });
            });
        }

        intent
    }
}

struct MachineData {
    machine: Machine,
    applications: Option<Vec<DesktopEntry>>,
    modules: Option<Vec<Mod>>,
    secrets: Option<Vec<EnvSecret>>,
}

impl MachineData {
    fn create(machine: Machine) -> Self {
        Self {
            machine,
            applications: None,
            modules: None,
            secrets: None,
        }
    }

    fn set_applications(&mut self, applications: Vec<DesktopEntry>) {
        self.applications = Some(applications);
    }

    fn set_modules(&mut self, modules: Vec<Mod>) {
        self.modules = Some(modules);
    }

    fn set_secrets(&mut self, secrets: Vec<EnvSecret>) {
        self.secrets = Some(secrets);
    }

    fn is_initialized(&self) -> bool {
        self.applications.is_some() && self.modules.is_some() && self.secrets.is_some()
    }
}
