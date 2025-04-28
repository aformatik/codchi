mod main_panel;
mod menubar;
mod side_panel;
mod util;

use anyhow::Result;
use egui::*;
use main_panel::{
    empty_machine_creation::EmptyMachineCreationIntent, machine_creation::MachineCreationIntent,
    machine_inspection::MachineInspectionIntent, MainPanel, MainPanelIntent, MainPanelType,
};
use menubar::MenubarIntent;
use side_panel::{GuiSidePanel, SidePanelIntent};
use std::{
    path::PathBuf,
    sync::mpsc::{channel, Receiver, Sender},
    thread,
};
use util::{
    backend_broker::{BackendBroker, BackendIntent},
    dialog_manager::{DialogIntent, DialogManager},
    status_entries::StatusEntries,
    textures_manager::TexturesManager,
};

use crate::config::CodchiConfig;

struct Gui {
    side_panel: GuiSidePanel,
    main_panel: MainPanel,

    status_entries: StatusEntries,
    dialog_manager: DialogManager,
    textures_manager: TexturesManager,

    backend_broker: BackendBroker,

    sender: Sender<(usize, ChannelDTO)>,
    receiver: Receiver<(usize, ChannelDTO)>,
}

enum ChannelDTO {
    Generic,
}

impl eframe::App for Gui {
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame) {
        self.receive_msgs();

        self.update_ui(ctx);
    }
}

impl Gui {
    fn new() -> Self {
        let (sender, receiver) = channel();
        Self {
            side_panel: GuiSidePanel::new(),
            main_panel: MainPanel::new(),

            status_entries: StatusEntries::new(),
            dialog_manager: DialogManager::new(),
            textures_manager: TexturesManager::new(),

            backend_broker: BackendBroker::new(),

            sender,
            receiver,
        }
    }

    fn receive_msgs(&mut self) {
        while let Ok((status_index, channel_dto)) = self.receiver.try_recv() {
            self.status_entries.decrease(status_index);
            match channel_dto {
                ChannelDTO::Generic => {}
            }
        }
    }

    fn update_ui(&mut self, ctx: &Context) {
        self.update_menubar(ctx);
        self.update_status_bar(ctx);

        self.update_side_panel(ctx);

        self.update_main_panel(ctx);

        self.update_modals(ctx);
        self.update_textures(ctx);

        self.update_backend_broker();
    }

    fn update_status_bar(&self, ctx: &Context) {
        TopBottomPanel::bottom("status_bar_panel")
            .exact_height(25.0)
            .resizable(false)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    let mut is_first_status = true;
                    for status in self.status_entries.get_status() {
                        if is_first_status {
                            is_first_status = false;
                            ui.add(egui::Spinner::new());
                        } else {
                            ui.separator();
                        }
                        ui.label(status);
                    }
                });
            });
    }

    fn update_menubar(&mut self, ctx: &Context) {
        TopBottomPanel::top("menubar_panel")
            .resizable(false)
            .show(ctx, |ui| {
                let intents = menubar::update(ui, &mut self.textures_manager);
                for intent in intents {
                    self.eval_menubar_intent(ctx, intent);
                }
            });
    }

    fn update_side_panel(&mut self, ctx: &Context) {
        SidePanel::left("side_panel")
            .exact_width(200.0)
            .resizable(false)
            .show(ctx, |ui| {
                ScrollArea::vertical().auto_shrink(false).show(ui, |ui| {
                    let intents = self.side_panel.update(
                        ui,
                        &mut self.status_entries,
                        &self.textures_manager,
                    );
                    for intent in intents {
                        self.eval_side_panel_intent(intent);
                    }
                });
            });
    }

    fn update_main_panel(&mut self, ctx: &Context) {
        CentralPanel::default().show(ctx, |ui| {
            ui.with_layout(Layout::right_to_left(Align::Min), |ui| {
                ui.separator();
                ui.vertical(|ui| {
                    let intents = self.main_panel.update(
                        ui,
                        &mut self.status_entries,
                        &mut self.dialog_manager,
                        &mut self.textures_manager,
                    );

                    for intent in intents {
                        self.eval_main_panel_intent(intent);
                    }
                })
            })
        });
    }

    fn update_modals(&mut self, ctx: &Context) {
        let intent_option = self.dialog_manager.update(ctx);
        if let Some(intent) = intent_option {
            self.eval_modals_intent(intent);
        }
    }

    fn update_textures(&mut self, ctx: &Context) {
        self.textures_manager.update(ctx);
    }

    fn update_backend_broker(&mut self) {
        let intents = self
            .backend_broker
            .update(&mut self.status_entries, &mut self.dialog_manager);
        for intent in intents {
            self.eval_backend_intent(intent);
        }
    }

    fn eval_menubar_intent(&mut self, ctx: &Context, menubar_intent: MenubarIntent) {
        match menubar_intent {
            menubar::MenubarIntent::Home => self.main_panel.reset(),
            menubar::MenubarIntent::OpenGithub => {
                ctx.open_url(OpenUrl::new_tab("https://github.com/aformatik/codchi/"))
            }
            menubar::MenubarIntent::OpenIssues => ctx.open_url(OpenUrl::new_tab(
                "https://github.com/aformatik/codchi/issues",
            )),
            menubar::MenubarIntent::ZoomIn => gui_zoom::zoom_in(ctx),
            menubar::MenubarIntent::ZoomOut => gui_zoom::zoom_out(ctx),
            menubar::MenubarIntent::RecoverStore => {
                let index = self
                    .status_entries
                    .push(String::from("Recovering Codchi store..."), 1);
                let sender_clone = self.sender.clone();
                thread::spawn(move || {
                    let _ = crate::platform::platform::store_recover();
                    sender_clone.send((index, ChannelDTO::Generic)).unwrap();
                });
            }
            menubar::MenubarIntent::ShowTray(val) => {
                let mut doc = CodchiConfig::open_mut().expect("Failed to open config");
                doc.tray_autostart(val);
                doc.write().expect("Failed to write config");
            }
            menubar::MenubarIntent::EnableVcxsrv(val) => {
                let mut doc = CodchiConfig::open_mut().expect("Failed to open config");
                doc.vcxsrv_enable(val);
                doc.write().expect("Failed to write config");
            }
            menubar::MenubarIntent::ShowTrayVcxsrv(val) => {
                let mut doc = CodchiConfig::open_mut().expect("Failed to open config");
                doc.vcxsrv_tray(val);
                doc.write().expect("Failed to write config");
            }
            menubar::MenubarIntent::EnableWslVpnkit(val) => {
                let mut doc = CodchiConfig::open_mut().expect("Failed to open config");
                doc.enable_wsl_vpnkit(val);
                doc.write().expect("Failed to write config");
            }
            menubar::MenubarIntent::InsertUrl(url) => {
                self.main_panel.panels.get_machine_creation().pass_url(url);
                self.main_panel.change(MainPanelType::MachineCreation);
            }
        }
    }

    fn eval_side_panel_intent(&mut self, side_panel_intent: SidePanelIntent) {
        match side_panel_intent {
            SidePanelIntent::DisplayMachine(machine) => {
                self.main_panel
                    .panels
                    .get_machine_inspection()
                    .change(machine, &mut self.status_entries);
                self.main_panel.change(MainPanelType::MachineInspection)
            }
            SidePanelIntent::BeginMachineCreation => {
                self.main_panel.change(MainPanelType::MachineCreation)
            }
            SidePanelIntent::BeginEmptyMachineCreation => {
                self.main_panel.change(MainPanelType::EmptyMachineCreation)
            }
        }
    }

    fn eval_modals_intent(&mut self, dialog_intent: DialogIntent) {
        match dialog_intent {
            util::dialog_manager::DialogIntent::Rebuild {
                machine,
                update_modules,
            } => self.backend_broker.rebuild_machine(
                machine,
                update_modules,
                false,
                &mut self.status_entries,
            ),
            util::dialog_manager::DialogIntent::Duplicate {
                machine,
                duplicate_name,
            } => self.backend_broker.duplicate_machine(
                machine,
                duplicate_name,
                &mut self.status_entries,
            ),
            util::dialog_manager::DialogIntent::Tar { machine, file_path } => self
                .backend_broker
                .tar_machine(machine, file_path, &mut self.status_entries),
            util::dialog_manager::DialogIntent::Stop { machine } => self
                .backend_broker
                .stop_machine(machine, &mut self.status_entries),
            util::dialog_manager::DialogIntent::Delete { machine, confirm } => {
                if confirm {
                    self.backend_broker
                        .delete_machine(machine, &mut self.status_entries);
                }
            }
            util::dialog_manager::DialogIntent::Secret {
                machine_name,
                name,
                value,
            } => self.backend_broker.insert_secret(
                machine_name,
                name,
                value,
                &mut self.status_entries,
            ),
        }
    }

    fn eval_main_panel_intent(&mut self, main_panel_intent: MainPanelIntent) {
        match main_panel_intent {
            MainPanelIntent::MachineInspection(intent) => {
                self.eval_machine_inspection_intent(intent)
            }
            MainPanelIntent::MachineCreation(intent) => self.eval_machine_creation_intent(intent),
            MainPanelIntent::EmptyMachineCreation(intent) => {
                self.eval_empty_machine_creation_intent(intent)
            }
        }
    }

    fn eval_machine_inspection_intent(
        &mut self,
        machine_inspection_intent: MachineInspectionIntent,
    ) {
        match machine_inspection_intent {
            MachineInspectionIntent::Rebuild(machine) => {
                self.dialog_manager.queue_generic(DialogIntent::Rebuild {
                    machine,
                    update_modules: true,
                });
            }
            MachineInspectionIntent::Duplicate(machine) => {
                self.dialog_manager.queue_generic(DialogIntent::Duplicate {
                    machine,
                    duplicate_name: "".to_string(),
                });
            }
            MachineInspectionIntent::Tar(machine) => {
                self.dialog_manager.queue_generic(DialogIntent::Tar {
                    machine,
                    file_path: PathBuf::default(),
                });
            }
            MachineInspectionIntent::Stop(machine) => {
                self.dialog_manager
                    .queue_generic(DialogIntent::Stop { machine });
            }
            MachineInspectionIntent::Delete(machine) => {
                self.dialog_manager.queue_generic(DialogIntent::Delete {
                    machine,
                    confirm: false,
                });
            }
        }
    }

    fn eval_machine_creation_intent(&mut self, machine_creation_intent: MachineCreationIntent) {
        match machine_creation_intent {
            MachineCreationIntent::CreateMachine(
                machine_name,
                git_url,
                options,
                modules,
                dont_run_init,
            ) => {
                self.backend_broker.create_machine(
                    machine_name,
                    git_url,
                    options,
                    modules,
                    dont_run_init,
                    &mut self.status_entries,
                );
            }
            MachineCreationIntent::LoadRepository(auth_url) => {
                if self
                    .main_panel
                    .panels
                    .get_machine_creation()
                    .is_auth_url_loaded(&auth_url)
                {
                    self.main_panel
                        .panels
                        .get_machine_creation()
                        .to_repository_panel(Some(auth_url));
                } else {
                    self.backend_broker
                        .access_repository(auth_url, &mut self.status_entries)
                }
            }
            MachineCreationIntent::LoadModules(repo) => {
                self.backend_broker
                    .load_modules(repo, &mut self.status_entries);
            }
        }
    }

    fn eval_empty_machine_creation_intent(
        &mut self,
        empty_machine_creation_intent: EmptyMachineCreationIntent,
    ) {
        match empty_machine_creation_intent {
            EmptyMachineCreationIntent::CreateMachine(machine_name, dont_build) => {
                self.backend_broker.create_empty_machine(
                    machine_name,
                    dont_build,
                    &mut self.status_entries,
                );
            }
        }
    }

    fn eval_backend_intent(&mut self, backend_intent: BackendIntent) {
        match backend_intent {
            BackendIntent::DuplicatedMachine(machine_name) => {
                self.side_panel
                    .add_machine(machine_name, &mut self.status_entries);
            }
            BackendIntent::DeletedMachine(machine_name) => {
                self.side_panel.remove_machine(&machine_name);
                self.main_panel
                    .panels
                    .get_machine_inspection()
                    .unselect_machine(&machine_name);
            }
            BackendIntent::AccessedRepository(auth_url) => {
                self.backend_broker
                    .load_repository(auth_url, &mut self.status_entries);
            }
            BackendIntent::LoadedBranches(auth_url, branches_result) => {
                if let Ok(branches) = branches_result {
                    self.main_panel
                        .panels
                        .get_machine_creation()
                        .pass_branches(auth_url, branches);
                }
            }
            BackendIntent::LoadedTags(auth_url, tags_result) => {
                if let Ok(tags) = tags_result {
                    self.main_panel
                        .panels
                        .get_machine_creation()
                        .pass_tags(auth_url, tags);
                }
            }
            BackendIntent::LoadedModules(repo, modules_result) => {
                if let Ok(modules) = modules_result {
                    self.main_panel
                        .panels
                        .get_machine_creation()
                        .pass_modules(repo, modules);
                }
            }
        }
    }
}

pub fn run() -> Result<()> {
    let options = eframe::NativeOptions {
        viewport: ViewportBuilder::default().with_inner_size((1280.0, 720.0)),
        ..Default::default()
    };
    eframe::run_native(
        "Codchi",
        options,
        Box::new(|cc| {
            // set custom theme
            cc.egui_ctx.set_visuals(get_visuals());

            // This gives us image support:
            egui_extras::install_image_loaders(&cc.egui_ctx);

            // Zoom in a bit initially
            let ppp = cc.egui_ctx.pixels_per_point();
            cc.egui_ctx.set_pixels_per_point(1.25 * ppp);

            Ok(Box::<Gui>::new(Gui::new()))
        }),
    )
    .unwrap();
    Ok(())
}

pub fn get_visuals() -> Visuals {
    let mut visuals = Visuals::dark();
    visuals.widgets.active.fg_stroke.color = Color32::WHITE;
    visuals.widgets.noninteractive.fg_stroke.color = Color32::LIGHT_GRAY;
    visuals
}
