#[allow(dead_code)]
use crate::{
    config::{codchi, CodchiConfig},
    platform::{Driver, Host, Machine, MachineDriver, PlatformStatus},
    util::{ResultExt, UtilExt},
};
use anyhow::Result;
use core::*;
use notify_rust::Notification;
use std::{env, sync::mpsc::channel, thread};
use tray_icon::menu::{CheckMenuItem, MenuItem};
use TrayItem::*;

mod core;

static LOGO: &[u8] = include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/logo.png"));
// static GREEN_SQUARE: &[u8] = include_bytes!(concat!(
//     env!("CARGO_MANIFEST_DIR"),
//     "/assets/green-square.png"
// ));
// static YELLOW_SQUARE: &[u8] = include_bytes!(concat!(
//     env!("CARGO_MANIFEST_DIR"),
//     "/assets/yellow-square.png"
// ));
// static RED_SQUARE: &[u8] = include_bytes!(concat!(
//     env!("CARGO_MANIFEST_DIR"),
//     "/assets/red-square.png"
// ));
// static WHITE_SQUARE: &[u8] = include_bytes!(concat!(
//     env!("CARGO_MANIFEST_DIR"),
//     "/assets/white-square.png"
// ));

#[derive(Clone, Debug)]
struct App {
    config: &'static CodchiConfig,
    machines: Vec<Machine>,
}

impl App {
    fn new() -> Result<Self> {
        Ok(Self {
            config: CodchiConfig::get(),
            machines: Machine::list(true)?,
        })
    }
}

pub fn run() -> Result<()> {
    let (tx, rx) = channel::<TrayOp<App>>();

    let tx2 = tx.clone();
    thread::spawn(move || loop {
        tx2.send(TrayOp::UpdateState(|app: &mut App| {
            *app = App::new().unwrap()
        }))
        .unwrap();
        thread::sleep(std::time::Duration::from_secs(10));
    });

    let app = App::new()?;
    let mut items = Vec::new();

    // Machines
    {
        items.extend(app.machines.iter().map(|orig| {
            let mk_button = |text: fn(&PlatformStatus) -> &'static str,
                             enabled: fn(&PlatformStatus) -> bool,
                             action: fn(&Machine)| {
                let name = orig.config.name.clone();
                let machine = orig.clone();
                let tx = tx.clone();
                TrayItem::dynamic::<MenuItem>(
                    Button {
                        text: text(&machine.platform_status).to_string(),
                        on_click: Box::leak(Box::new(move |_| {
                            let machine = machine.clone();
                            let tx = tx.clone();
                            thread::spawn(move || {
                                tx.send(TrayOp::WaitStatus(true)).unwrap();
                                action(&machine);
                                tx.send(TrayOp::WaitStatus(false)).unwrap();
                            });
                            TrayOp::Noop
                        })),
                    },
                    move |app: &App, inner| {
                        if let Some(ps) = app
                            .machines
                            .iter()
                            .find(|m| m.config.name == name)
                            .map(|m| m.platform_status.clone())
                        {
                            inner.set_text(text(&ps));
                            inner.set_enabled(enabled(&ps))
                        }
                    },
                )
            };
            TrayItem::dynamic::<tray_icon::menu::Submenu>(
                Submenu {
                    text: "".to_string(),
                    children: vec![
                        {
                            let name = orig.config.name.clone();
                            Button {
                                text: "Terminal".to_string(),
                                on_click: Box::leak(Box::new(move |_| {
                                    if let Err(err) = Driver::host()
                                        .open_terminal(&[
                                            &env::current_exe().unwrap().display().to_string(),
                                            "exec",
                                            &name,
                                        ])
                                        .trace_err(&format!("Failed opening terminal for {name}"))
                                    {
                                        Notification::new()
                                            .summary("Codchi Error")
                                            .body(&err.to_string())
                                            .auto_icon()
                                            .show()
                                            .ignore();
                                    }
                                    TrayOp::Noop
                                })),
                            }
                        },
                        Separator,
                        mk_button(
                            |ps| match ps {
                                PlatformStatus::Stopped => "Start",
                                _ => "Restart",
                            },
                            |_| true,
                            |m| {
                                m.stop(false).trace_err("restarting machine").ignore();
                                m.start().trace_err("restarting machine").ignore();
                            },
                        ),
                        mk_button(
                            |_| "Stop",
                            |ps| *ps == PlatformStatus::Running,
                            |m| m.stop(false).trace_err("stopping machine").ignore(),
                        ),
                    ],
                },
                {
                    let name = orig.config.name.clone();
                    move |app: &App, inner| {
                        // inner.set_icon(None);
                        if let Some(m) = app.machines.iter().find(|m| m.config.name == name) {
                            let (status, icon) = match m.platform_status {
                                PlatformStatus::NotInstalled => ("Needs rebuild", "🟨"),
                                PlatformStatus::Stopped => ("Stopped", "🟥"),
                                PlatformStatus::Running => ("Running", "🟩"),
                            };
                            // inner.set_icon(Some(FromRgba::load_icon(icon)));
                            inner.set_text(format!("{icon} {name}: {status}"));
                        }
                    }
                },
            )
        }));
    }
    // Settings
    {
        let mut settings = Vec::new();
        let mk_checkbox =
            |label: &str,
             get: fn(&CodchiConfig) -> bool,
             set: fn(&mut codchi::ConfigMut, enabled: bool)| {
                Checkbox {
                    text: label.to_string(),
                    checked: get(app.config),
                    on_click: Box::leak(Box::new(move |inner: CheckMenuItem| {
                        let mut doc = CodchiConfig::open_mut().expect("Failed to open config");
                        set(&mut doc, inner.is_checked());
                        doc.write().expect("Failed to write config");
                        TrayOp::Noop::<App>
                    })),
                }
            };
        settings.push(mk_checkbox(
            "Show tray icon",
            |config| config.tray.autostart,
            codchi::ConfigMut::tray_autostart,
        ));
        #[cfg(target_os = "windows")]
        {
            settings.push(Submenu {
                text: "VcXsrv".to_string(),
                children: vec![
                    mk_checkbox(
                        "Enable",
                        |config| config.vcxsrv.enable,
                        codchi::ConfigMut::vcxsrv_enable,
                    ),
                    mk_checkbox(
                        "Show tray icon",
                        |config| config.vcxsrv.tray,
                        codchi::ConfigMut::vcxsrv_tray,
                    ),
                    Separator,
                    Button {
                        text: "Restart (this will kill all running apps!)".to_string(),
                        on_click: &|_| {
                            Driver::host()
                                .start_vcxsrv(true)
                                .trace_err("Failed restarting VcXsrv")
                                .ignore();
                            TrayOp::Noop
                        },
                    },
                ],
            });
            settings.push(mk_checkbox(
                "Isolate the network of each code machine (requires restarting machines)",
                |config| config.enable_wsl_netns,
                |cfg, enabled| cfg.enable_wsl_netns(enabled),
            ));
            settings.push(mk_checkbox(
                "Enable wsl-vpnkit",
                |config| config.enable_wsl_vpnkit,
                |cfg, enabled| {
                    cfg.enable_wsl_vpnkit(enabled);
                    if enabled {
                        crate::platform::start_wsl_vpnkit(Driver::store())
                    } else {
                        crate::platform::stop_wsl_vpnkit(Driver::store())
                    }
                    .trace_err("Failed starting / stopping wsl-vpnkit")
                    .ignore();
                },
            ));
        }
        items.extend([
            Separator,
            Submenu {
                text: "Settings".to_string(),
                children: settings,
            },
        ]);
    }
    // Quit
    items.extend([
        Separator,
        Button {
            text: "Quit".to_string(),
            on_click: &|_| TrayOp::Quit,
        },
    ]);
    Tray::<App> {
        tooltip: "Codchi",
        icon: FromRgba::load_icon(LOGO),
        items,
    }
    .run(app, rx)?;

    Ok(())
}
