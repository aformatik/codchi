#![windows_subsystem = "windows"]

use std::{
    env,
    os::windows::process::CommandExt,
    process::{Command, Stdio},
};
use windows::Win32::{
    Foundation::HWND,
    System::Threading::CREATE_NO_WINDOW,
    UI::WindowsAndMessaging::{MessageBoxW, MB_ICONERROR, MB_OK},
};

fn main() {
    let result = (|| {
        let args: Vec<String> = env::args().skip(1).collect();
        let codchi_exe = env::current_exe()?.parent().unwrap().join("codchi.exe");

        let child_output = Command::new(codchi_exe)
            .args(args)
            .creation_flags(CREATE_NO_WINDOW.0)
            .stderr(Stdio::piped())
            .output()?;

        if !child_output.status.success() {
            let stderr = String::from_utf8_lossy(&child_output.stderr);
            anyhow::bail!("{stderr}");
        }
        anyhow::Ok(())
    })();

    if let Err(e) = result {
        fn show_message_box(title: &str, message: &str) {
            use windows::core::PCWSTR;

            let title_wide: Vec<u16> = title.encode_utf16().chain(std::iter::once(0)).collect();
            let message_wide: Vec<u16> = message.encode_utf16().chain(std::iter::once(0)).collect();

            unsafe {
                MessageBoxW(
                    HWND::default(),
                    PCWSTR(message_wide.as_ptr()),
                    PCWSTR(title_wide.as_ptr()),
                    MB_OK | MB_ICONERROR,
                );
            }
        }

        show_message_box("Codchi had an error", &e.to_string());
    }
}
