use std::path::PathBuf;

use egui::*;

use crate::platform::Machine;

use super::password_field;

pub struct DialogManager {
    dialogs: Vec<GuiDialog>,
}

impl DialogManager {
    pub fn new() -> Self {
        Self {
            dialogs: Vec::new(),
        }
    }

    pub fn update(&mut self, ctx: &Context) -> Option<DialogIntent> {
        if !self.dialogs.is_empty() {
            let dialog_result = self.dialogs[0].update_ui(ctx);

            if let Some(result) = dialog_result {
                let dialog = self.dialogs.remove(0);

                match result {
                    DialogResult::Confirm => return Some(dialog.intent),
                    DialogResult::Cancel => {}
                }
            }
        }

        None
    }

    fn queue(
        &mut self,
        intent: DialogIntent,
        heading: String,
        primary_button_text: String,
        primary_button_color: Option<(u8, u8, u8)>,
        secondary_button_text: Option<String>,
    ) {
        self.queue_dialog(GuiDialog {
            intent,
            heading,
            primary_button_text,
            primary_button_color,
            secondary_button_text,
        });
    }

    fn queue_dialog(&mut self, gui_dialog: GuiDialog) {
        self.dialogs.push(gui_dialog);
    }

    pub fn queue_generic(&mut self, dialog_intent: DialogIntent) {
        dialog_intent.to_gui_modal(self);
    }
}

struct GuiDialog {
    intent: DialogIntent,
    heading: String,
    primary_button_text: String,
    primary_button_color: Option<(u8, u8, u8)>,
    secondary_button_text: Option<String>,
}

impl GuiDialog {
    fn update_ui(&mut self, ctx: &Context) -> Option<DialogResult> {
        let modal = Modal::new(Id::new("global_modal")).show(ctx, |ui| {
            ui.heading(&self.heading);
            ui.separator();

            let confirm_enabled = Self::display_intent_element(ui, &mut self.intent);

            Sides::new().show(
                ui,
                |ui_left| {
                    self.secondary_button_text
                        .as_ref()
                        .and_then(|text| Some(ui_left.button(text)))
                },
                |ui_right| {
                    let confirm_button = Button::new(&self.primary_button_text);
                    let confirm_button = match self.primary_button_color {
                        Some((r, g, b)) => confirm_button.fill(Color32::from_rgb(r, g, b)),
                        None => confirm_button,
                    };
                    ui_right.add_enabled(confirm_enabled, confirm_button)
                },
            )
        });

        let (cancel_button_option, confirm_button) = modal.inner;

        if let Some(cancel_button) = cancel_button_option {
            if cancel_button.clicked() {
                return Some(DialogResult::Cancel);
            }
        }

        if confirm_button.clicked() {
            return Some(DialogResult::Confirm);
        }

        None
    }

    fn display_intent_element(ui: &mut Ui, intent: &mut DialogIntent) -> bool {
        match intent {
            DialogIntent::Rebuild {
                machine: _,
                update_modules,
            } => {
                ui.checkbox(update_modules, "update modules");
                ui.separator();

                true
            }
            DialogIntent::Duplicate {
                machine: _,
                duplicate_name,
            } => {
                ui.add(TextEdit::singleline(duplicate_name).hint_text("Duplicate Name"));
                ui.separator();

                !duplicate_name.is_empty()
            }
            DialogIntent::Tar {
                machine: _,
                file_path,
            } => {
                let eligible_file = file_path.extension().is_some_and(|ext| ext == "gz");

                if ui.button("Select").clicked() {
                    if let Some(selected_folder) = rfd::FileDialog::new()
                        .add_filter("TAR (*.tar.gz)", &["tar.gz"])
                        .save_file()
                    {
                        *file_path = selected_folder;
                    }
                }
                if eligible_file {
                    ui.label(format!("{}", file_path.to_string_lossy()));
                }
                ui.separator();

                eligible_file
            }
            DialogIntent::Stop { machine: _ } => true,
            DialogIntent::Delete {
                machine: _,
                confirm,
            } => {
                ui.checkbox(confirm, "Are you sure?");
                ui.separator();

                *confirm
            }
            DialogIntent::Secret {
                machine_name: _,
                name: _,
                value,
            } => {
                ui.add(password_field(value));
                ui.separator();

                true
            }
        }
    }
}

enum DialogResult {
    Confirm,
    Cancel,
}

pub enum DialogIntent {
    Rebuild {
        machine: Machine,
        update_modules: bool,
    },
    Duplicate {
        machine: Machine,
        duplicate_name: String,
    },
    Tar {
        machine: Machine,
        file_path: PathBuf,
    },
    Stop {
        machine: Machine,
    },
    Delete {
        machine: Machine,
        confirm: bool,
    },

    Secret {
        machine_name: String,
        name: String,
        value: String,
    },
}

impl DialogIntent {
    pub fn to_gui_modal(self, dialog_manager: &mut DialogManager) {
        let (heading, p_text, p_color, s_text) = match &self {
            DialogIntent::Rebuild {
                machine,
                update_modules: _,
            } => {
                let heading = format!("Rebuilding Machine '{}'", &machine.config.name);
                let p_text = "Rebuild".to_string();
                let p_color = Some((0, 128, 0));
                let s_text = Some("Cancel".to_string());
                (heading, p_text, p_color, s_text)
            }
            DialogIntent::Duplicate {
                machine,
                duplicate_name: _,
            } => {
                let heading = format!("Duplicating Machine '{}'", &machine.config.name);
                let p_text = "Duplicate".to_string();
                let p_color = Some((0, 128, 0));
                let s_text = Some("Cancel".to_string());
                (heading, p_text, p_color, s_text)
            }
            DialogIntent::Tar {
                machine,
                file_path: _,
            } => {
                let heading = format!("Exporting Machine '{}'", &machine.config.name);
                let p_text = "Export".to_string();
                let p_color = Some((0, 128, 0));
                let s_text = Some("Cancel".to_string());
                (heading, p_text, p_color, s_text)
            }
            DialogIntent::Stop { machine } => {
                let heading = format!("Stopping Machine '{}'", &machine.config.name);
                let p_text = "Stop".to_string();
                let p_color = Some((128, 0, 0));
                let s_text = Some("Cancel".to_string());
                (heading, p_text, p_color, s_text)
            }
            DialogIntent::Delete {
                machine,
                confirm: _,
            } => {
                let heading = format!("Delete Machine '{}'", &machine.config.name);
                let p_text = "Delete".to_string();
                let p_color = Some((128, 0, 0));
                let s_text = Some("Cancel".to_string());
                (heading, p_text, p_color, s_text)
            }
            DialogIntent::Secret {
                machine_name,
                name,
                value: _,
            } => {
                let heading = format!("Setting '{}' for '{}'", name, &machine_name);
                let p_text = "Set".to_string();
                let p_color = Some((0, 128, 0));
                let s_text = None;
                (heading, p_text, p_color, s_text)
            }
        };

        dialog_manager.queue(self, heading, p_text, p_color, s_text);
    }
}
