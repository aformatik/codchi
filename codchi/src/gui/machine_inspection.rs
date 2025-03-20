use crate::config::Mod;
use crate::gui::{create_password_field, MainPanel, MainPanelMsgType, MainPanelType};
use crate::logging::CodchiOutput;
use crate::platform::{platform::HostImpl, DesktopEntry, Host, Machine, MachineDriver};
use crate::secrets::{EnvSecret, MachineSecrets};
use egui::*;
use itertools::Itertools;
use std::path::PathBuf;
use std::{
    collections::{HashMap, VecDeque},
    sync::{Arc, Mutex},
    thread,
};

use super::{StatusEntries, DTO};

pub struct MachineInspectionMainPanel {
    status_text: StatusEntries,

    machine_data_map: HashMap<String, MachineData>,
    current_machine: String,
    next_panel_type: Option<MainPanelType>,

    answer_queue: Arc<Mutex<VecDeque<(usize, String, ChannelDataType)>>>,
    frame_msg_callback: Box<dyn Fn(MainPanelMsgType)>,

    show_rebuild_spec_modal: bool,
    show_duplicate_spec_modal: bool,
    show_tar_spec_modal: bool,
    show_delete_confirmation_modal: bool,

    textures: HashMap<String, TextureHandle>,
}

struct MachineData {
    machine: Machine,
    applications: Option<Vec<DesktopEntry>>,
    modules: Option<Vec<Mod>>,
    secrets: Option<Vec<EnvSecret>>,
    initialized: bool,
}

pub(crate) enum ChannelDataType {
    Applications(Vec<DesktopEntry>),
    Modules(Vec<Mod>),
    Secrets(Vec<EnvSecret>),
    RebuiltMachine,
    DuplicatedMachine(Machine),
    DeletedMachine(String),
    ClearStatus,
}

impl MachineData {
    fn new(machine: Machine) -> Self {
        Self {
            machine,
            applications: None,
            modules: None,
            secrets: None,
            initialized: false,
        }
    }
}

impl MainPanel for MachineInspectionMainPanel {
    fn update(&mut self, ui: &mut Ui) {
        let received_answer = if let Ok(mut answer_queue) = self.answer_queue.try_lock() {
            answer_queue.pop_front()
        } else {
            None
        };
        if let Some((index, machine_name, data_type)) = received_answer {
            if self.status_text.decrease(index) {
                self.machine_data_map
                    .entry(machine_name.clone())
                    .and_modify(|machine_data| {
                        machine_data.initialized = true;
                    });
                // attempt to load currently inspecting machine
                if !self.current_machine.is_empty() {
                    let current_machine = Machine::by_name(&self.current_machine, true);
                    if let Ok(machine) = current_machine {
                        self.transfer_data(DTO::Machine(machine));
                    }
                }
            }
            match data_type {
                ChannelDataType::Modules(modules) => {
                    self.machine_data_map
                        .entry(machine_name)
                        .and_modify(|machine_data| {
                            machine_data.modules = Some(modules);
                        });
                }
                ChannelDataType::Secrets(secrets) => {
                    self.machine_data_map
                        .entry(machine_name)
                        .and_modify(|machine_data| {
                            machine_data.secrets = Some(secrets);
                        });
                }
                ChannelDataType::Applications(applications) => {
                    self.machine_data_map
                        .entry(machine_name)
                        .and_modify(|machine_data| {
                            machine_data.applications = Some(applications);
                        });
                }
                ChannelDataType::RebuiltMachine => {
                    self.reload_machine(Some(&machine_name));
                }
                ChannelDataType::DuplicatedMachine(machine) => {
                    (self.frame_msg_callback)(super::MainPanelMsgType::MachineInspection(
                        ChannelDataType::DuplicatedMachine(machine),
                    ));
                }
                ChannelDataType::DeletedMachine(machine_name) => {
                    self.current_machine = String::from("");
                    self.machine_data_map.remove(&machine_name);
                    (self.frame_msg_callback)(super::MainPanelMsgType::MachineInspection(
                        ChannelDataType::DeletedMachine(machine_name),
                    ));
                }
                ChannelDataType::ClearStatus => {}
            }
        }

        self.display_machine(ui);
    }

    fn modal_update(&mut self, ctx: &Context) {
        if self.show_rebuild_spec_modal {
            let modal = Modal::new(Id::new("rebuild_machine_spec_modal")).show(ctx, |ui| {
                let state_id = ui.id().with("rebuild_machine_spec_modal_checkbox");
                let mut checked = ui.data_mut(|d| d.get_temp::<bool>(state_id).unwrap_or(true));
                ui.heading(format!("Rebuild machine '{}'", &self.current_machine));
                ui.checkbox(&mut checked, "update modules");
                ui.horizontal(|ui| {
                    let rebuild_button = Button::new("Rebuild").fill(Color32::DARK_BLUE);
                    if ui.add(rebuild_button).clicked() {
                        let index = self.status_text.insert(
                            1,
                            String::from(format!("Building machine '{}'...", self.current_machine)),
                        );

                        let mut machine =
                            self.machine_data_map[&self.current_machine].machine.clone();
                        let answer_queue_clone = self.answer_queue.clone();
                        thread::spawn(move || {
                            let _ = machine.build(!checked);
                            answer_queue_clone.lock().unwrap().push_back((
                                index,
                                machine.config.name.clone(),
                                ChannelDataType::RebuiltMachine,
                            ));
                        });
                        self.show_rebuild_spec_modal = false;
                    }
                    if ui.button("Cancel").clicked() {
                        self.show_rebuild_spec_modal = false;
                    }
                });
                ui.data_mut(|d| d.insert_temp(state_id, checked));
            });
            if modal.should_close() {
                self.show_rebuild_spec_modal = false;
            }
        }
        if self.show_duplicate_spec_modal {
            let modal = Modal::new(Id::new("duplicate_machine_spec_modal")).show(ctx, |ui| {
                let state_id = ui.id().with("duplicate_machine_spec_modal_name");
                let mut new_machine_name =
                    ui.data_mut(|d| d.get_temp::<String>(state_id).unwrap_or(String::from("")));
                ui.heading(format!("Duplicate machine '{}'", &self.current_machine));
                let name_editor =
                    TextEdit::singleline(&mut new_machine_name).hint_text("New Machine Name");
                ui.add(name_editor);
                ui.horizontal(|ui| {
                    let duplicate_button = Button::new("Duplicate").fill(Color32::DARK_GREEN);
                    if ui.add(duplicate_button).clicked() {
                        let index = self.status_text.insert(
                            1,
                            String::from(format!(
                                "Duplicating machine '{}' as '{}'...",
                                self.current_machine, new_machine_name
                            )),
                        );

                        let machine = self.machine_data_map[&self.current_machine].machine.clone();
                        let new_machine_name_clone = new_machine_name.clone();
                        let answer_queue_clone = self.answer_queue.clone();
                        thread::spawn(move || {
                            let _ = machine.duplicate(&new_machine_name_clone);
                            let duplicated_machine =
                                Machine::by_name(&new_machine_name_clone, true);

                            let answer = if let Ok(machine) = duplicated_machine {
                                ChannelDataType::DuplicatedMachine(machine)
                            } else {
                                ChannelDataType::ClearStatus
                            };

                            answer_queue_clone.lock().unwrap().push_back((
                                index,
                                machine.config.name,
                                answer,
                            ));
                        });
                        self.show_duplicate_spec_modal = false;
                    }
                    if ui.button("Cancel").clicked() {
                        self.show_duplicate_spec_modal = false;
                    }
                });
                ui.data_mut(|d| d.insert_temp(state_id, new_machine_name));
            });
            if modal.should_close() {
                self.show_duplicate_spec_modal = false;
            }
        }
        if self.show_tar_spec_modal {
            let modal = Modal::new(Id::new("tar_machine_spec_modal")).show(ctx, |ui| {
                let state_id = ui.id().with("tar_machine_spec_modal_path");
                let mut tar_path =
                    ui.data_mut(|d| d.get_temp::<String>(state_id).unwrap_or(String::from("")));
                ui.heading(format!("Export machine '{}'", &self.current_machine));
                let path_editor = TextEdit::singleline(&mut tar_path).hint_text(".tar Path");
                ui.add(path_editor);
                ui.horizontal(|ui| {
                    let tar_button = Button::new("Tar").fill(Color32::DARK_GREEN);
                    if ui.add(tar_button).clicked() {
                        // TODO
                        let path = PathBuf::try_from(&tar_path).unwrap();
                        let index = self.status_text.insert(
                            1,
                            String::from(format!(
                                "Exporting files of {} to {path:?}...",
                                self.current_machine
                            )),
                        );

                        let machine = self.machine_data_map[&self.current_machine].machine.clone();
                        let answer_queue_clone = self.answer_queue.clone();
                        thread::spawn(move || {
                            let _ = machine.tar(&path);

                            answer_queue_clone.lock().unwrap().push_back((
                                index,
                                machine.config.name,
                                ChannelDataType::ClearStatus,
                            ));
                        });
                        self.show_tar_spec_modal = false;
                    }
                    if ui.button("Cancel").clicked() {
                        self.show_tar_spec_modal = false;
                    }
                });
                ui.data_mut(|d| d.insert_temp(state_id, tar_path));
            });
            if modal.should_close() {
                self.show_tar_spec_modal = false;
            }
        }
        if self.show_delete_confirmation_modal {
            let modal = Modal::new(Id::new("delete_machine_confirmation_modal")).show(ctx, |ui| {
                ui.heading(format!("Delete machine '{}'?", &self.current_machine));
                ui.horizontal(|ui| {
                    let delete_button = Button::new("Delete").fill(Color32::DARK_RED);
                    if ui.add(delete_button).clicked() {
                        let machine_data = &self.machine_data_map[&self.current_machine];
                        let index = self.status_text.insert(
                            1,
                            String::from(format!("Deleting machine '{}'...", self.current_machine)),
                        );

                        let machine_clone = machine_data.machine.clone();
                        let answer_queue_clone = self.answer_queue.clone();
                        thread::spawn(move || {
                            let machine_name = machine_clone.config.name.clone();
                            let delete_result = machine_clone.delete(true);
                            let answer = if dbg!(delete_result.is_ok()) {
                                ChannelDataType::DeletedMachine(machine_name.clone())
                            } else {
                                ChannelDataType::ClearStatus
                            };

                            answer_queue_clone.lock().unwrap().push_back((
                                index,
                                machine_name,
                                answer,
                            ));
                        });
                        self.show_delete_confirmation_modal = false;
                    }
                    if ui.button("Cancel").clicked() {
                        self.show_delete_confirmation_modal = false;
                    }
                });
            });
            if modal.should_close() {
                self.show_delete_confirmation_modal = false;
            }
        }
    }

    fn next_panel(&mut self) -> Option<MainPanelType> {
        self.next_panel_type.take()
    }

    fn transfer_data(&mut self, dto: DTO) {
        match dto {
            DTO::Machine(machine) => {
                let machine_name = machine.config.name.clone();
                if !self.machine_data_map.contains_key(&machine_name) {
                    let machine_data = MachineData::new(machine.clone());
                    self.machine_data_map
                        .insert(machine_name.clone(), machine_data);
                }

                if !self.machine_data_map[&machine_name].initialized {
                    let index = self.status_text.insert(
                        3,
                        String::from(format!("Loading machine {}...", &machine_name)),
                    );

                    let answer_queue_clone = self.answer_queue.clone();
                    let machine_name_clone = machine_name.clone();
                    let machine_clone = machine.clone();
                    thread::spawn(move || {
                        let applications =
                            HostImpl::list_desktop_entries(&machine_clone).unwrap_or(Vec::new());

                        answer_queue_clone.lock().unwrap().push_back((
                            index,
                            machine_name_clone,
                            ChannelDataType::Applications(applications),
                        ));
                    });

                    let answer_queue_clone = self.answer_queue.clone();
                    let modules_clone = machine.config.modules.clone();
                    let machine_name_clone = machine_name.clone();
                    thread::spawn(move || {
                        let modules = modules_clone.to_output();

                        answer_queue_clone.lock().unwrap().push_back((
                            index,
                            machine_name_clone,
                            ChannelDataType::Modules(modules),
                        ));
                    });

                    let answer_queue_clone = self.answer_queue.clone();
                    let machine_name_clone = machine_name.clone();
                    thread::spawn(move || {
                        let secrets = machine
                            .eval_env_secrets()
                            .expect("failed to load machine secrets")
                            .into_values()
                            .collect_vec()
                            .to_output();
                        answer_queue_clone.lock().unwrap().push_back((
                            index,
                            machine_name_clone,
                            ChannelDataType::Secrets(secrets),
                        ));
                    });
                }
                self.current_machine = machine_name;
            }
            DTO::Text(_) => todo!(),
        }
    }

    fn get_status_text(&self) -> &StatusEntries {
        &self.status_text
    }

    fn renew(&mut self) {
        self.current_machine = String::from("");
    }
}

impl MachineInspectionMainPanel {
    pub fn new(callback: Box<dyn Fn(MainPanelMsgType)>) -> Self {
        Self {
            status_text: StatusEntries::new(),

            machine_data_map: HashMap::new(),
            current_machine: String::from(""),
            next_panel_type: None,

            answer_queue: Arc::new(Mutex::new(VecDeque::new())),
            frame_msg_callback: callback,

            show_rebuild_spec_modal: false,
            show_duplicate_spec_modal: false,
            show_tar_spec_modal: false,
            show_delete_confirmation_modal: false,

            textures: HashMap::new(),
        }
    }

    fn display_machine(&mut self, ui: &mut Ui) {
        if !self.current_machine.is_empty() {
            ui.horizontal(|ui| {
                let title_text = format!("Machine: {}", &self.current_machine);
                ui.add(widgets::Label::new(
                    RichText::from(title_text).heading().strong(),
                ));
                ui.with_layout(Layout::right_to_left(Align::BOTTOM), |ui| {
                    ui.menu_button("Actions", |ui| {
                        if ui.button("Rebuild").clicked() {
                            self.show_rebuild_spec_modal = true;
                        }
                        if ui.button("Duplicate").clicked() {
                            self.show_duplicate_spec_modal = true;
                        }
                        if ui.button("Tar").clicked() {
                            self.show_tar_spec_modal = true;
                        }
                        if ui.button("Stop").clicked() {
                            let _ = self.machine_data_map[&self.current_machine]
                                .machine
                                .stop(false);
                        }
                        let delete_button = Button::new("Delete").fill(Color32::DARK_RED);
                        if ui.add(delete_button).clicked() {
                            self.show_delete_confirmation_modal = true;
                        }
                    });
                    let reload_button = Button::new("\u{21BA}");
                    if ui.add(reload_button).on_hover_text("Reload").clicked() {
                        self.reload_machine(None);
                    }
                });
            });
            let machine_data = &self.machine_data_map[&self.current_machine];
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
                    let icon = if let Some(icon_path) = &desktop_entry.icon {
                        if !self.textures.contains_key(&desktop_entry.app_name) {
                            let image = image::open(icon_path)
                                .expect("Failed to open image icon")
                                .to_rgba8();
                            let size = [image.width() as usize, image.height() as usize];
                            let texture = ui.ctx().load_texture(
                                "icon_texture",
                                ColorImage::from_rgba_unmultiplied(size, &image),
                                Default::default(),
                            );
                            self.textures
                                .insert(desktop_entry.app_name.clone(), texture);
                        }
                        let texture = &self.textures[&desktop_entry.app_name];
                        let image =
                            Image::from_texture(texture).max_size(Vec2 { x: 12.0, y: 12.0 });
                        Some(image)
                    } else {
                        None
                    };
                    let text = WidgetText::RichText(RichText::new(&desktop_entry.app_name));
                    let button = Button::opt_image_and_text(icon, Some(text));
                    let button_handle = ui.add(button);
                    if button_handle.clicked() {
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
                            ui.add(create_password_field(
                                &secret.name,
                                |new_password| {
                                    let (lock, mut cfg) =
                                        crate::config::MachineConfig::open_existing(
                                            &self.current_machine,
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
    }

    fn reload_machine(&mut self, name_option: Option<&str>) {
        let machine_name = if let Some(name) = name_option {
            name
        } else {
            &self.current_machine
        };
        self.machine_data_map.remove(machine_name);
        let machine_res = Machine::by_name(machine_name, true);
        if let Ok(machine) = machine_res {
            self.transfer_data(DTO::Machine(machine));
        }
    }
}
