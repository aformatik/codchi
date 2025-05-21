use egui::*;

use crate::config::CodchiConfig;

use super::util::textures_manager::TexturesManager;

#[derive(Debug)]
pub enum MenubarIntent {
    Home,
    OpenGithub,
    OpenIssues,
    ToggleMode,
    ZoomIn,
    ZoomOut,
    RecoverStore,
    ShowTray(bool),
    EnableVcxsrv(bool),
    ShowTrayVcxsrv(bool),
    EnableWslVpnkit(bool),
    InsertUrl(String),
}

pub fn update(ui: &mut Ui, textures_manager: &mut TexturesManager) -> Vec<MenubarIntent> {
    let mut intent = Vec::new();

    ui.horizontal_centered(|ui| {
        let codchi_button = match textures_manager.deliver("logo", "assets/logo.png") {
            Some(tex_handle) => {
                let image = Image::new(tex_handle).max_size(vec2(48.0, 48.0));
                Button::image(image)
            }
            None => Button::new("Home"),
        };
        if ui.add(codchi_button).on_hover_text("Home").clicked() {
            intent.push(MenubarIntent::Home);
            ui.close_menu();
        }
        ui.separator();

        ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
            let github_button =
                match textures_manager.deliver_svg("github_logo", "assets/github_logo.svg") {
                    Some(tex_handle) => Button::image(
                        Image::new(tex_handle)
                            .tint(ui.visuals().widgets.inactive.fg_stroke.color)
                            .max_size(vec2(24.0, 24.0)),
                    ),
                    None => Button::new("Github"),
                };
            if ui.add(github_button).on_hover_text("Github").clicked() {
                intent.push(MenubarIntent::OpenGithub);
                ui.close_menu();
            }

            let bug_report_button =
                match textures_manager.deliver_svg("bug_icon", "assets/bug_icon.svg") {
                    Some(tex_handle) => Button::image(
                        Image::new(tex_handle)
                            .tint(ui.visuals().widgets.inactive.fg_stroke.color)
                            .max_size(vec2(24.0, 24.0)),
                    ),
                    None => Button::new("Bug-Report"),
                };
            if ui
                .add(bug_report_button)
                .on_hover_text("Bug-Report")
                .clicked()
            {
                intent.push(MenubarIntent::OpenIssues);
                ui.close_menu();
            }

            let mut pressed_settings = update_settings_menu(ui, textures_manager);
            intent.append(&mut pressed_settings);

            if let Some(url) = singleline_enter(ui, "menubar_url_inputline", "url", 400.0) {
                intent.push(MenubarIntent::InsertUrl(url));
            }
        });
    });

    intent
}

fn update_settings_menu(ui: &mut Ui, textures_manager: &mut TexturesManager) -> Vec<MenubarIntent> {
    let mut intent = Vec::new();

    let menu_button = match textures_manager.deliver_svg("menu", "assets/menu_icon.svg") {
        Some(tex_handle) => Button::image(
            Image::new(tex_handle)
                .tint(ui.visuals().widgets.inactive.fg_stroke.color)
                .max_size(vec2(24.0, 24.0)),
        ),
        None => Button::new("Settings"),
    };
    menu::menu_custom_button(ui, menu_button, |ui| {
        if ui.button("Change mode").clicked() {
            intent.push(MenubarIntent::ToggleMode);
        }
        if ui.button("Zoom In").clicked() {
            intent.push(MenubarIntent::ZoomIn);
            ui.close_menu();
        }
        if ui.button("Zoom Out").clicked() {
            intent.push(MenubarIntent::ZoomOut);
            ui.close_menu();
        }
        ui.separator();
        #[cfg(target_os = "windows")]
        {
            if ui.button("Recover store").clicked() {
                intent.push(MenubarIntent::RecoverStore);
                ui.close_menu();
            }
            ui.separator();
        }

        if let Some(checked) = checkbox_clicked(
            ui,
            "codchi_tray_checkbox",
            CodchiConfig::get().tray.autostart,
            "Show tray icon",
        ) {
            intent.push(MenubarIntent::ShowTray(checked));
        }

        #[cfg(target_os = "windows")]
        {
            ui.menu_button("VcXsrv", |ui| {
                if let Some(checked) = checkbox_clicked(
                    ui,
                    "vcxsrv_enable_checkbox",
                    CodchiConfig::get().vcxsrv.enable,
                    "Enable",
                ) {
                    intent.push(MenubarIntent::EnableVcxsrv(checked));
                }

                if let Some(checked) = checkbox_clicked(
                    ui,
                    "vcxsrv_tray_checkbox",
                    CodchiConfig::get().vcxsrv.tray,
                    "Show tray icon",
                ) {
                    intent.push(MenubarIntent::ShowTrayVcxsrv(checked));
                }
            });
            if let Some(checked) = checkbox_clicked(
                ui,
                "wsl_vpnkit_enable_checkbox",
                CodchiConfig::get().enable_wsl_vpnkit,
                "Enable wsl-vpnkit",
            ) {
                intent.push(MenubarIntent::EnableWslVpnkit(checked));
            }
        }
    })
    .response
    .on_hover_text("Setting");

    intent
}

pub fn checkbox_clicked(ui: &mut Ui, id: &str, initial: bool, text: &str) -> Option<bool> {
    let state_id = ui.id().with(id);
    let mut checked = ui.data_mut(|d| d.get_temp::<bool>(state_id).unwrap_or(initial));

    let response = ui.checkbox(&mut checked, text);
    ui.data_mut(|d| d.insert_temp(state_id, checked));

    if response.clicked() {
        Some(checked)
    } else {
        None
    }
}

pub fn singleline_enter(ui: &mut Ui, id: &str, hint_text: &str, width: f32) -> Option<String> {
    let state_id = ui.id().with(id);
    let mut edit_text = ui.data_mut(|d| d.get_temp::<String>(state_id).unwrap_or("".to_string()));

    let single_line = TextEdit::singleline(&mut edit_text)
        .hint_text(hint_text)
        .min_size(Vec2 { x: width, y: 0.0 });
    let response = ui.add(single_line);

    if response.lost_focus() && response.ctx.input(|i| i.key_pressed(Key::Enter)) {
        ui.data_mut(|d| d.insert_temp(state_id, "".to_string()));
        Some(edit_text)
    } else {
        ui.data_mut(|d| d.insert_temp(state_id, edit_text));
        None
    }
}
