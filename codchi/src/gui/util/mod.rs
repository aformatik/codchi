pub mod backend_broker;
pub mod dialog_manager;
pub mod status_entries;
pub mod textures_manager;

use egui::*;

pub fn password_field<'a>(password: &'a mut String) -> impl Widget + 'a {
    move |ui: &mut Ui| password_field_ui(ui, password)
}

fn password_field_ui(ui: &mut Ui, password: &mut String) -> Response {
    let state_id = ui.id().with("show_plaintext");

    let mut show_plaintext = ui.data_mut(|d| d.get_temp::<bool>(state_id).unwrap_or(false));

    let result = ui.horizontal(|ui| {
        let response = ui
            .add(SelectableLabel::new(show_plaintext, "👁"))
            .on_hover_text("Show/hide password");

        if response.clicked() {
            show_plaintext = !show_plaintext;
        }

        ui.add_sized(
            [200.0, ui.available_height()],
            TextEdit::singleline(password).password(!show_plaintext),
        );
    });
    ui.data_mut(|d| d.insert_temp(state_id, show_plaintext));

    result.response
}

pub fn advanced_password_field<'a>(
    id: &'a str,
    write_closure: impl FnOnce(String) + 'a,
    password: &'a str,
) -> impl Widget + 'a {
    move |ui: &mut Ui| advanced_password_field_ui(ui, id, write_closure, password)
}

pub fn advanced_password_field_ui(
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
