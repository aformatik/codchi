mod machine_creation;
mod machine_inspection;

use crate::{
    config::CodchiConfig,
    platform::{Machine, PlatformStatus},
};
use egui::*;
use machine_creation::MachineCreationMainPanel;
use machine_inspection::MachineInspectionMainPanel;
use std::{
    any::Any,
    collections::HashMap,
    sync::mpsc::{channel, Receiver, Sender},
    thread,
    time::Instant,
};

pub fn run() -> anyhow::Result<()> {
    let options = eframe::NativeOptions {
        viewport: ViewportBuilder::default().with_inner_size((960.0, 540.0)),
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

            Ok(Box::<Gui>::new(Gui::new(load_textures(&cc.egui_ctx))))
        }),
    )
    .unwrap();
    Ok(())
}

struct Gui {
    main_panels: Vec<Box<dyn MainPanel>>,
    current_main_panel_index: usize,
    machines: Vec<Machine>,
    textures: HashMap<String, TextureHandle>,

    status_text: Option<String>,

    pending_msgs: usize,
    sender: Sender<ChannelDataType>,
    receiver: Receiver<ChannelDataType>,

    reloading_machine_index: Option<usize>,
    last_reload: Instant,
}

#[derive(Clone)]
pub enum MainPanelType {
    MachineInspection,
    MachineCreation,
    BugReport,
}

enum ChannelDataType {
    Machine(Machine, usize),
    StoreRecovered,
}

impl eframe::App for Gui {
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame) {
        if self.reloading_machine_index.is_none() {
            let now = Instant::now();
            if now.duration_since(self.last_reload).as_secs() >= 10 {
                self.reloading_machine_index = Some(0);
                self.reload_machine();
            }
        }
        if self.pending_msgs != 0 {
            let received_answer: Option<ChannelDataType> = self.receiver.try_recv().ok();
            if let Some(data_type) = received_answer {
                self.pending_msgs -= 1;
                if self.pending_msgs == 0 {
                    self.status_text = None;
                }
                match data_type {
                    ChannelDataType::Machine(machine, machine_index) => {
                        self.machines[machine_index] = machine;
                        self.reloading_machine_index = self.reload_machine();
                    }
                    ChannelDataType::StoreRecovered => {
                        self.status_text = Some(String::from(""));
                    }
                }
            }
        }

        self.menu_bar_panel(ctx);
        self.status_bar_panel(ctx);
        self.side_panel(ctx);
        self.main_panel(ctx);
    }
}

impl Gui {
    fn new(textures: HashMap<String, TextureHandle>) -> Self {
        let (sender, receiver) = channel();
        Self {
            main_panels: vec![
                Box::new(MachineInspectionMainPanel::default()),
                Box::new(MachineCreationMainPanel::default()),
            ],
            current_main_panel_index: 0,
            machines: Machine::list(true).expect("Machines could not be listed"),
            textures,

            status_text: None,

            pending_msgs: 0,
            sender,
            receiver,

            reloading_machine_index: None,
            last_reload: Instant::now(),
        }
    }

    fn menu_bar_panel(&mut self, ctx: &Context) {
        let height = 50.0;
        TopBottomPanel::top("menubar_panel")
            .resizable(false)
            .exact_height(height)
            .show(ctx, |ui| {
                ui.horizontal_centered(|ui| {
                    let codchi_button = Button::image(include_image!("../../assets/logo.png"));
                    if ui.add(codchi_button).clicked() {
                        self.current_main_panel_index =
                            Self::get_main_panel_index(MainPanelType::MachineInspection);
                    }
                    ui.separator();
                    ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                        ui.set_height(25.0);
                        let github_button =
                            Button::image(include_image!("../../assets/github_logo.png"));
                        let bug_report_button =
                            Button::image(include_image!("../../assets/bug_icon.png"));
                        if ui.add(github_button).clicked() {
                            ui.ctx()
                                .open_url(OpenUrl::new_tab("https://github.com/aformatik/codchi/"));
                        }
                        if ui.add(bug_report_button).clicked() {
                            ui.ctx().open_url(OpenUrl::new_tab(
                                "https://github.com/aformatik/codchi/issues",
                            ));
                        }
                        ui.menu_image_button(include_image!("../../assets/settings.png"), |ui| {
                            if ui.button("Zoom In").clicked() {
                                gui_zoom::zoom_in(ctx);
                                ui.close_menu();
                            }
                            if ui.button("Zoom Out").clicked() {
                                gui_zoom::zoom_out(ctx);
                                ui.close_menu();
                            }
                            ui.separator();
                            #[cfg(target_os = "windows")]
                            {
                                if ui.button("Recover store").clicked() {
                                    self.status_text =
                                        Some(String::from("Recovering Codchi store..."));
                                    let sender_clone = self.sender.clone();
                                    thread::spawn(move || {
                                        let _ = crate::platform::platform::store_recover();
                                        sender_clone.send(ChannelDataType::StoreRecovered).unwrap();
                                    });
                                    ui.close_menu();
                                }
                                ui.separator();
                            }
                            ui.add(create_advanced_checkbox(
                                "codchi_tray_checkbox",
                                CodchiConfig::get().tray.autostart,
                                |checked| {
                                    let mut doc =
                                        CodchiConfig::open_mut().expect("Failed to open config");
                                    doc.tray_autostart(checked);
                                    doc.write().expect("Failed to write config");
                                },
                                "Show tray icon",
                            ));
                            #[cfg(target_os = "windows")]
                            {
                                ui.menu_button("VcXsrv", |ui| {
                                    ui.add(create_advanced_checkbox(
                                        "vcxsrv_enable_checkbox",
                                        CodchiConfig::get().vcxsrv.enable,
                                        |checked| {
                                            let mut doc = CodchiConfig::open_mut()
                                                .expect("Failed to open config");
                                            doc.vcxsrv_enable(checked);
                                            doc.write().expect("Failed to write config");
                                        },
                                        "Enable",
                                    ));
                                    ui.add(create_advanced_checkbox(
                                        "vcxsrv_tray_checkbox",
                                        CodchiConfig::get().vcxsrv.tray,
                                        |checked| {
                                            let mut doc = CodchiConfig::open_mut()
                                                .expect("Failed to open config");
                                            doc.vcxsrv_tray(checked);
                                            doc.write().expect("Failed to write config");
                                        },
                                        "Show tray icon",
                                    ));
                                });
                                ui.add(create_advanced_checkbox(
                                    "wsl_vpnkit_enable_checkbox",
                                    CodchiConfig::get().enable_wsl_vpnkit,
                                    |checked| {
                                        let mut doc = CodchiConfig::open_mut()
                                            .expect("Failed to open config");
                                        doc.enable_wsl_vpnkit(checked);
                                        doc.write().expect("Failed to write config");
                                    },
                                    "Enable wsl-vpnkit",
                                ));
                            }
                        });
                    });
                });
            });
    }

    fn status_bar_panel(&self, ctx: &Context) {
        TopBottomPanel::bottom("statusbar_panel")
            .resizable(false)
            .show(ctx, |ui| {
                if let Some(text) = &self.status_text {
                    ui.label(text);
                } else if let Some(text) =
                    self.main_panels[self.current_main_panel_index].get_status_text()
                {
                    ui.label(text);
                }
            });
    }

    fn side_panel(&mut self, ctx: &Context) {
        let width = 200.0;
        SidePanel::left("side_panel")
            .exact_width(width)
            .resizable(false)
            .show(ctx, |ui| {
                ScrollArea::vertical().auto_shrink(false).show(ui, |ui| {
                    let new_machine_button = Button::new(RichText::new("New").heading());
                    let new_machin_button_handle =
                        ui.add_sized([ui.available_width(), 0.0], new_machine_button);
                    if new_machin_button_handle.clicked() {
                        self.current_main_panel_index =
                            Self::get_main_panel_index(MainPanelType::MachineCreation);
                    }
                    ui.separator();

                    ui.horizontal_top(|ui| {
                        ui.separator();
                        ui.with_layout(Layout::top_down_justified(Align::LEFT), |ui| {
                            for i in 0..self.machines.len() {
                                let machine = &self.machines[i];
                                let icon =
                                    if self.reloading_machine_index.is_some_and(|index| index == i)
                                    {
                                        let texture = &self.textures["gray"];
                                        Some(Image::from_texture(texture))
                                    } else {
                                        match machine.platform_status {
                                            PlatformStatus::NotInstalled => {
                                                let texture = &self.textures["yellow"];
                                                Some(Image::from_texture(texture))
                                            }
                                            PlatformStatus::Stopped => {
                                                let texture = &self.textures["red"];
                                                Some(Image::from_texture(texture))
                                            }
                                            PlatformStatus::Running => {
                                                let texture = &self.textures["green"];
                                                Some(Image::from_texture(texture))
                                            }
                                        }
                                    };
                                let button_text =
                                    RichText::strong(format!("{}", machine.config.name).into());
                                let machine_button = Button::opt_image_and_text(
                                    icon,
                                    Some(WidgetText::RichText(button_text)),
                                );
                                let button_handle = ui.add(machine_button);
                                if button_handle.clicked() {
                                    let machine_inspection_panel_index = Self::get_main_panel_index(
                                        MainPanelType::MachineInspection,
                                    );
                                    self.current_main_panel_index = machine_inspection_panel_index;
                                    self.main_panels[machine_inspection_panel_index]
                                        .pass_machine(machine.clone());
                                }
                            }
                        })
                    });
                });
            });
    }

    fn main_panel(&mut self, ctx: &Context) {
        CentralPanel::default().show(ctx, |ui| {
            ScrollArea::both()
                .id_salt("machine_info_scroll")
                .auto_shrink(false)
                .show(ui, |ui| {
                    ui.spacing_mut().scroll = style::ScrollStyle::solid();

                    let next_panel_type_option =
                        self.main_panels[self.current_main_panel_index].next_panel();
                    if let Some(next_panel_type) = next_panel_type_option {
                        self.current_main_panel_index = Self::get_main_panel_index(next_panel_type);
                    }

                    self.main_panels[self.current_main_panel_index].update(ui);
                })
        });
        for main_panel in &mut self.main_panels {
            main_panel.modal_update(ctx);
        }
    }

    fn get_main_panel_index(main_panel_type: MainPanelType) -> usize {
        match main_panel_type {
            MainPanelType::MachineInspection => 0,
            MainPanelType::MachineCreation => 1,
            MainPanelType::BugReport => 2,
        }
    }

    fn reload_machine(&mut self) -> Option<usize> {
        if let Some(machine_index) = self.reloading_machine_index {
            if let Some(machine) = self.machines.get_mut(machine_index) {
                self.pending_msgs += 1;
                self.status_text = Some(String::from(format!(
                    "Updating status for machine '{}'",
                    machine.config.name
                )));

                let machine_config = machine.config.clone();
                let machine_sender = self.sender.clone();
                thread::spawn(move || {
                    let machine =
                        Machine::read(machine_config, true).expect("Machine could not be read");

                    machine_sender
                        .send(ChannelDataType::Machine(machine, machine_index))
                        .expect("machine could not be sent");
                });
            }
            Some(machine_index + 1)
        } else {
            self.last_reload = Instant::now();
            None
        }
    }
}

pub trait MainPanel: Any {
    fn update(&mut self, ui: &mut Ui);

    fn modal_update(&mut self, ctx: &Context);

    fn next_panel(&mut self) -> Option<MainPanelType>;

    fn pass_machine(&mut self, machine: Machine);

    fn get_status_text(&self) -> &Option<String>;

    fn renew(&mut self);
}

pub fn create_modal<R>(
    ctx: &Context,
    id: &str,
    show_modal_bool: &mut bool,
    add_contents: impl FnOnce(&mut Ui) -> R,
) {
    if *show_modal_bool {
        let modal = Modal::new(Id::new(id)).show(ctx, |ui| {
            add_contents(ui);
            ui.vertical_centered(|ui| {
                if ui.button("Ok").clicked() {
                    *show_modal_bool = false;
                }
            });
        });
        if modal.should_close() {
            *show_modal_bool = false;
        }
    }
}

pub fn create_password_field(password: &String) -> impl Widget + '_ {
    move |ui: &mut Ui| create_password_field_ui(ui, password)
}

pub fn create_password_field_ui(ui: &mut Ui, password: &str) -> Response {
    let state_id = ui.id().with("show_plaintext");
    let mut show_plaintext = ui.data_mut(|d| d.get_temp::<bool>(state_id).unwrap_or(false));

    let result = ui.horizontal(|ui| {
        let response = ui
            .add(SelectableLabel::new(show_plaintext, "👁"))
            .on_hover_text("Show/hide password");

        if response.clicked() {
            show_plaintext = !show_plaintext;
        }

        let mut password = String::from(password);

        ui.add_sized(
            [200.0, ui.available_height()],
            TextEdit::singleline(&mut password)
                .interactive(false)
                .password(!show_plaintext),
        );
    });
    ui.data_mut(|d| d.insert_temp(state_id, show_plaintext));

    result.response
}

pub fn create_advanced_checkbox<'a>(
    id: &'a str,
    initial: bool,
    set: fn(enabled: bool),
    text: &'a str,
) -> impl Widget + 'a {
    move |ui: &mut Ui| create_advanced_checkbox_ui(ui, id, initial, set, text)
}

pub fn create_advanced_checkbox_ui(
    ui: &mut Ui,
    id: &str,
    initial: bool,
    write_closure: impl FnOnce(bool),
    text: &str,
) -> Response {
    let state_id = ui.id().with(id);
    let mut checked = ui.data_mut(|d| d.get_temp::<bool>(state_id).unwrap_or(initial));

    let result = ui.checkbox(&mut checked, text);

    if result.clicked() {
        write_closure(checked);
    }
    ui.data_mut(|d| d.insert_temp(state_id, checked));

    result
}

fn load_textures(ctx: &Context) -> HashMap<String, TextureHandle> {
    let green_square = ColorImage::new([10, 10], Color32::GREEN);
    let yellow_square = ColorImage::new([10, 10], Color32::YELLOW);
    let red_square = ColorImage::new([10, 10], Color32::RED);
    let gray_square = ColorImage::new(
        [10, 10],
        get_visuals().widgets.noninteractive.fg_stroke.color,
    );

    let green_handle = ctx.load_texture("green_texture", green_square, TextureOptions::default());
    let yellow_handle =
        ctx.load_texture("yellow_texture", yellow_square, TextureOptions::default());
    let red_handle = ctx.load_texture("red_texture", red_square, TextureOptions::default());
    let gray_handle = ctx.load_texture("gray_texture", gray_square, TextureOptions::default());

    let mut textures = HashMap::new();
    textures.insert("green".to_string(), green_handle);
    textures.insert("yellow".to_string(), yellow_handle);
    textures.insert("red".to_string(), red_handle);
    textures.insert("gray".to_string(), gray_handle);

    textures
}

fn get_visuals() -> Visuals {
    let mut visuals = Visuals::dark();
    visuals.widgets.active.fg_stroke.color = Color32::WHITE;
    visuals.widgets.noninteractive.fg_stroke.color = Color32::LIGHT_GRAY;
    visuals
}
