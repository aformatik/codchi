mod machine_creation;
mod machine_inspection;

use crate::{
    config::{CodchiConfig, MachineConfig},
    platform::{Machine, PlatformStatus},
};
use egui::*;
use machine_creation::MachineCreationMainPanel;
use machine_inspection::MachineInspectionMainPanel;
use std::{
    collections::{HashMap, VecDeque},
    sync::{Arc, Mutex},
    thread,
    time::Instant,
};
use strum::{EnumIter, IntoEnumIterator};

struct Gui {
    main_panels: MainPanels,
    machine_configs: Vec<MachineConfig>,
    machines: Vec<Machine>,
    textures: HashMap<String, TextureHandle>,

    status_text: StatusEntries,

    answer_queue: Arc<Mutex<VecDeque<(usize, ChannelDataType)>>>,

    reloading_machine_index: Option<usize>,
    last_reload: Instant,

    url_input_line: String,
}

struct MainPanels {
    panels: Vec<Box<dyn MainPanel>>,
    current_panel_index: usize,
}

#[derive(Clone, EnumIter)]
pub enum MainPanelType {
    MachineInspection,
    MachineCreation,
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
                self.machine_configs =
                    MachineConfig::list().expect("Machine-configs could not be listed");
                self.reload_machine();
            }
        }
        let received_answer = if let Ok(mut answer_queue) = self.answer_queue.try_lock() {
            answer_queue.pop_front()
        } else {
            None
        };
        if let Some((status_index, data_type)) = received_answer {
            self.status_text.decrease(status_index);
            match data_type {
                ChannelDataType::Machine(machine, machine_index) => {
                    if machine_index < self.machines.len()
                        && self.machines[machine_index].config.name == machine.config.name
                    {
                        self.machines[machine_index] = machine;
                    } else {
                        // machine is new
                        self.machines.insert(machine_index, machine);
                    }

                    let next_machine_index = machine_index + 1;
                    if next_machine_index < self.machine_configs.len() {
                        self.reloading_machine_index = Some(next_machine_index);
                    } else {
                        // remove remaining machines that were not listed by MachineConfig::list()
                        while self.machines.get(next_machine_index).is_some() {
                            self.machines.remove(next_machine_index);
                        }
                        self.reloading_machine_index = None;
                        self.last_reload = Instant::now();
                    }
                    self.reload_machine();
                }
                ChannelDataType::StoreRecovered => {}
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
        Self {
            main_panels: MainPanels::default(),
            machine_configs: MachineConfig::list().expect("Machine-configs could not be listed"),
            machines: Machine::list(true).expect("Machines could not be listed"),
            textures,

            status_text: StatusEntries::new(),

            answer_queue: Arc::new(Mutex::new(VecDeque::new())),

            reloading_machine_index: None,
            last_reload: Instant::now(),

            url_input_line: String::from(""),
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
                    if ui.add(codchi_button).on_hover_text("Home").clicked() {
                        self.main_panels.change(MainPanelType::MachineInspection);
                        self.main_panels.renew();
                    }
                    ui.separator();
                    ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                        ui.set_height(25.0);
                        let github_button =
                            Button::image(include_image!("../../assets/github_logo.png"));
                        let bug_report_button =
                            Button::image(include_image!("../../assets/bug_icon.png"));
                        if ui.add(github_button).on_hover_text("Github").clicked() {
                            ui.ctx()
                                .open_url(OpenUrl::new_tab("https://github.com/aformatik/codchi/"));
                        }
                        if ui
                            .add(bug_report_button)
                            .on_hover_text("Bug-Report")
                            .clicked()
                        {
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
                                    let index = self
                                        .status_text
                                        .insert(1, String::from("Recovering Codchi store..."));
                                    let answer_queue_clone = self.answer_queue.clone();
                                    thread::spawn(move || {
                                        let _ = crate::platform::platform::store_recover();
                                        answer_queue_clone
                                            .lock()
                                            .unwrap()
                                            .push_back((index, ChannelDataType::StoreRecovered));
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
                        })
                        .response
                        .on_hover_text("Setting");

                        let url_input_line = TextEdit::singleline(&mut self.url_input_line);
                        let url_input_line_handle = ui.add_sized([400.0, 0.0], url_input_line);
                        if url_input_line_handle.lost_focus()
                            && url_input_line_handle
                                .ctx
                                .input(|i| i.key_pressed(Key::Enter))
                        {
                            self.main_panels.change(MainPanelType::MachineCreation);
                            self.main_panels
                                .transfer_data(DTO::Text(self.url_input_line.take()));
                        }
                    });
                });
            });
    }

    fn status_bar_panel(&self, ctx: &Context) {
        TopBottomPanel::bottom("statusbar_panel")
            .resizable(false)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    let mut first_status = true;
                    for status_option in self.status_text.get_status() {
                        if let Some((_pending_msgs, status)) = status_option {
                            if first_status {
                                first_status = false;
                                ui.add(egui::Spinner::new());
                            } else {
                                ui.separator();
                            }
                            ui.label(status);
                        }
                    }
                    for status_option in self.main_panels.get_status_text().get_status() {
                        if let Some((_pending_msgs, status)) = status_option {
                            if first_status {
                                first_status = false;
                                ui.add(egui::Spinner::new());
                            } else {
                                ui.separator();
                            }
                            ui.label(status);
                        }
                    }
                });
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
                        self.main_panels.change(MainPanelType::MachineCreation);
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
                                    self.main_panels.change(MainPanelType::MachineInspection);
                                    self.main_panels
                                        .transfer_data(DTO::Machine(machine.clone()));
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

                    if let Some(next_panel_type) = self.main_panels.next_panel() {
                        self.main_panels.change(next_panel_type);
                    }

                    self.main_panels.update(ui);
                    self.main_panels.modal_update(ctx);
                })
        });
    }

    fn reload_machine(&mut self) {
        if let Some(machine_index) = self.reloading_machine_index {
            if let Some(machine_config) = self.machine_configs.get_mut(machine_index) {
                let index = self.status_text.insert(
                    1,
                    String::from(format!(
                        "Updating status for machine '{}'",
                        machine_config.name
                    )),
                );

                if let Some(machine) = self.machines.get(machine_index + 1)
                    && machine.config.name == machine_config.name
                {
                    // machine at machine_index was deleted
                    self.machines.remove(machine_index);
                }

                let machine_config_clone = machine_config.clone();
                let answer_queue_clone = self.answer_queue.clone();
                thread::spawn(move || {
                    let machine = Machine::read(machine_config_clone, true)
                        .expect("Machine could not be read");

                    answer_queue_clone
                        .lock()
                        .unwrap()
                        .push_back((index, ChannelDataType::Machine(machine, machine_index)));
                });
            }
        }
    }
}

impl Default for MainPanels {
    fn default() -> Self {
        let mut panels: Vec<Box<dyn MainPanel>> = Vec::new();
        for main_panel in MainPanelType::iter() {
            let panel: Box<dyn MainPanel> = match main_panel {
                MainPanelType::MachineInspection => Box::new(MachineInspectionMainPanel::default()),
                MainPanelType::MachineCreation => Box::new(MachineCreationMainPanel::default()),
            };
            panels.push(panel);
        }
        Self {
            panels,
            current_panel_index: 0,
        }
    }
}

impl MainPanel for MainPanels {
    fn update(&mut self, ui: &mut Ui) {
        self.get_current_main_panel_mut().update(ui);
    }

    fn modal_update(&mut self, ctx: &Context) {
        for main_panel in &mut self.panels {
            main_panel.modal_update(ctx);
        }
    }

    fn next_panel(&mut self) -> Option<MainPanelType> {
        self.get_current_main_panel_mut().next_panel()
    }

    fn transfer_data(&mut self, dto: DTO) {
        self.get_current_main_panel_mut().transfer_data(dto);
    }

    fn get_status_text(&self) -> &StatusEntries {
        self.get_current_main_panel().get_status_text()
    }

    fn renew(&mut self) {
        self.get_current_main_panel_mut().renew();
    }
}

impl MainPanels {
    fn get_current_main_panel(&self) -> &dyn MainPanel {
        self.panels[self.current_panel_index].as_ref()
    }

    fn get_current_main_panel_mut(&mut self) -> &mut dyn MainPanel {
        self.panels[self.current_panel_index].as_mut()
    }

    fn change(&mut self, new_main_panel: MainPanelType) {
        self.current_panel_index = new_main_panel as usize;
    }
}

pub trait MainPanel {
    fn update(&mut self, ui: &mut Ui);

    fn modal_update(&mut self, ctx: &Context);

    fn next_panel(&mut self) -> Option<MainPanelType>;

    fn transfer_data(&mut self, dto: DTO);

    fn get_status_text(&self) -> &StatusEntries;

    fn renew(&mut self);
}

pub enum DTO {
    Machine(Machine),
    Text(String),
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

pub fn create_password_field<'a>(
    id: &'a str,
    write_closure: impl FnOnce(String) + 'a,
    password: &'a str,
) -> impl Widget + 'a {
    move |ui: &mut Ui| create_password_field_ui(ui, id, write_closure, password)
}

pub fn create_password_field_ui(
    ui: &mut Ui,
    id: &str,
    write_closure: impl FnOnce(String),
    password: &str,
) -> Response {
    let state_id = ui.id().with("show_plaintext");
    let text_id = ui.id().with(id);
    let mut show_plaintext = ui.data_mut(|d| d.get_temp::<bool>(state_id).unwrap_or(false));
    let mut text = ui.data_mut(|d| {
        d.get_temp::<String>(text_id)
            .unwrap_or(String::from(password))
    });

    let result = ui.horizontal(|ui| {
        let response = ui
            .add(SelectableLabel::new(show_plaintext, "👁"))
            .on_hover_text("Show/hide password");

        if response.clicked() {
            show_plaintext = !show_plaintext;
        }

        ui.add_sized(
            [200.0, ui.available_height()],
            TextEdit::singleline(&mut text).password(!show_plaintext),
        );

        if password != text {
            if ui.button("Write").clicked() {
                write_closure(text.clone());
            }
        }
    });
    ui.data_mut(|d| d.insert_temp(state_id, show_plaintext));
    ui.data_mut(|d| d.insert_temp(text_id, text));

    result.response
}

pub fn create_advanced_checkbox<'a>(
    id: &'a str,
    initial: bool,
    write_closure: impl FnOnce(bool) + 'a,
    text: &'a str,
) -> impl Widget + 'a {
    move |ui: &mut Ui| create_advanced_checkbox_ui(ui, id, initial, write_closure, text)
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

pub struct StatusEntries {
    status: Vec<Option<(usize, String)>>,
    empty_entries: Vec<usize>,
}

impl StatusEntries {
    fn new() -> Self {
        Self {
            status: Vec::new(),
            empty_entries: Vec::new(),
        }
    }

    fn insert(&mut self, pending_msgs: usize, value: String) -> usize {
        if let Some(index) = self.empty_entries.pop() {
            self.status[index] = Some((pending_msgs, value));
            index
        } else {
            self.status.push(Some((pending_msgs, value)));
            self.status.len() - 1
        }
    }

    fn decrease(&mut self, index: usize) -> bool {
        if index < self.status.len() {
            if let Some((pending_msgs, _value)) = self.status[index].as_mut() {
                *pending_msgs = *pending_msgs - 1;
                if *pending_msgs == 0 {
                    self.status[index] = None;
                    self.empty_entries.push(index);
                    return true;
                }
            }
        }
        false
    }

    fn get_status(&self) -> &Vec<Option<(usize, String)>> {
        &self.status
    }
}

pub fn run() -> anyhow::Result<()> {
    let options = eframe::NativeOptions {
        viewport: ViewportBuilder::default().with_inner_size((1600.0, 900.0)),
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
