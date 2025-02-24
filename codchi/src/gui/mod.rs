mod bug_report;
mod machine_creation;
mod machine_inspection;

use crate::platform::Machine;
use bug_report::BugReportMainPanel;
use egui;
use machine_creation::MachineCreationMainPanel;
use machine_inspection::MachineInspectionMainPanel;
use std::any::Any;

pub fn run() -> anyhow::Result<()> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size((640.0, 480.0)),
        ..Default::default()
    };
    eframe::run_native(
        "Codchi",
        options,
        Box::new(|cc| {
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

struct Gui {
    main_panels: Vec<Box<dyn MainPanel>>,
    current_main_panel_index: usize,
    show_bug_report_modal: bool,
    machines: Vec<Machine>,
}

#[derive(Clone)]
enum MainPanelType {
    MachineInspection,
    MachineCreation,
    BugReport,
}

impl eframe::App for Gui {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.menu_bar_panel(ctx);
        self.status_bar_panel(ctx);
        self.side_panel(ctx);
        self.main_panel(ctx);
    }
}

impl Gui {
    fn new() -> Self {
        Self {
            main_panels: vec![
                Box::new(MachineInspectionMainPanel::default()),
                Box::new(MachineCreationMainPanel::default()),
                Box::new(BugReportMainPanel::default()),
            ],
            current_main_panel_index: 0,
            show_bug_report_modal: false,
            machines: Machine::list(true).expect("Machines could not be listed"),
        }
    }

    fn menu_bar_panel(&mut self, ctx: &egui::Context) {
        let height = 50.0;
        egui::TopBottomPanel::top("menubar_panel")
            .resizable(false)
            .exact_height(height)
            .show(ctx, |ui| {
                ui.horizontal_centered(|ui| {
                    let codchi_button =
                        egui::Button::image(egui::include_image!("../../assets/logo.png"));
                    if ui.add(codchi_button).clicked() {
                        self.current_main_panel_index =
                            Self::get_main_panel_index(MainPanelType::MachineInspection);
                    }
                    ui.separator();
                    ui.menu_button("Settings", |ui| {
                        if ui.button("Zoom In").clicked() {
                            egui::gui_zoom::zoom_in(ctx);
                            ui.close_menu();
                        }
                        if ui.button("Zoom Out").clicked() {
                            egui::gui_zoom::zoom_out(ctx);
                            ui.close_menu();
                        }
                    });
                    if ui.button("BugReport").clicked() {
                        self.current_main_panel_index =
                            Self::get_main_panel_index(MainPanelType::BugReport);
                    }
                    if ui.button("Github").clicked() {
                        ui.ctx().open_url(egui::OpenUrl::new_tab(
                            "https://github.com/aformatik/codchi/",
                        ));
                    }
                });
            });
    }

    fn status_bar_panel(&self, ctx: &egui::Context) {}

    fn side_panel(&mut self, ctx: &egui::Context) {
        let width = 200.0;
        egui::SidePanel::left("side_panel")
            .exact_width(width)
            .resizable(false)
            .show(ctx, |ui| {
                egui::ScrollArea::vertical()
                    .auto_shrink(false)
                    .show(ui, |ui| {
                        ui.with_layout(
                            egui::Layout::with_cross_justify(
                                egui::Layout::top_down(egui::Align::Center),
                                true,
                            ),
                            |ui| {
                                let new_machine_button =
                                    egui::Button::new(egui::RichText::new("New").heading());
                                if ui.add(new_machine_button).clicked() {
                                    self.current_main_panel_index =
                                        Self::get_main_panel_index(MainPanelType::MachineCreation);
                                }
                                ui.separator();
                                for machine in &self.machines {
                                    let machine_button = egui::Button::new(machine.config.name.clone());
                                    let button_handle = ui.add(machine_button);
                                    if button_handle.clicked() {
                                        let machine_inspection_panel_index =
                                            Self::get_main_panel_index(
                                                MainPanelType::MachineInspection,
                                            );
                                        self.current_main_panel_index =
                                            machine_inspection_panel_index;
                                        let machine = Machine::by_name(&machine.config.name.clone(), true)
                                            .expect("machine doesn't exist");
                                        self.main_panels[machine_inspection_panel_index].pass_machine(machine);
                                    }
                                }
                            },
                        );
                    });
            });
    }

    fn main_panel(&mut self, ctx: &egui::Context) {
        egui::CentralPanel::default().show(ctx, |ui| {
            egui::ScrollArea::both()
                .id_salt("machine_info_scroll")
                .auto_shrink(false)
                .show(ui, |ui| {
                    ui.spacing_mut().scroll = egui::style::ScrollStyle::solid();

                    let next_panel_type_option =
                        self.main_panels[self.current_main_panel_index].next_panel();
                    if let Some(next_panel_type) = next_panel_type_option {
                        self.current_main_panel_index = Self::get_main_panel_index(next_panel_type);
                    }

                    self.main_panels[self.current_main_panel_index].update(ui);
                })
        });
        for mut main_panel in &mut self.main_panels {
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
}

pub trait MainPanel: Any {
    fn update(&mut self, ui: &mut egui::Ui);

    fn modal_update(&mut self, ctx: &egui::Context);

    fn next_panel(&mut self) -> Option<MainPanelType>;

    fn pass_machine(&mut self, machine: Machine);
}

pub fn create_modal<R>(
    ctx: &egui::Context,
    id: &str,
    show_modal_bool: &mut bool,
    add_contents: impl FnOnce(&mut egui::Ui) -> R,
) {
    if *show_modal_bool {
        let modal = egui::Modal::new(egui::Id::new(id)).show(ctx, |ui| {
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
