use anyhow::Result;
use log::{info, trace};
use tao::event_loop::{ControlFlow, EventLoopBuilder, EventLoopProxy};
use tokio::sync::mpsc;
use tray_icon::{
    menu::{AboutMetadata, Menu, MenuEvent, MenuItem, PredefinedMenuItem},
    TrayIcon, TrayIconBuilder, TrayIconEvent,
};

static LOGO: &[u8] =
    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/logo.png"));

#[derive(Clone, Debug)]
pub enum TrayMsg {
    Quit,
}

#[derive(Clone)]
pub struct TrayChan<T: 'static = TrayMsg>(EventLoopProxy<T>);
impl<T: std::fmt::Debug> TrayChan<T> {
    pub fn send(&self, x: T) {
        self.0
            .send_event(x)
            .expect("The main event loop disappeared...");
    }
}

pub async fn run(mut rx: mpsc::Receiver<TrayMsg>) -> Result<()> {
    let event_loop = EventLoopBuilder::<TrayMsg>::with_user_event().build();
    let chan = TrayChan(event_loop.create_proxy());

    let mut tray_icon = Some(build_tray_icon(chan.clone()));

    tokio::spawn(async move {
        while let Some(msg) = rx.recv().await {
            chan.send(msg)
        }
    });

    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Wait;

        if let tao::event::Event::UserEvent(msg) = event {
            match msg {
                TrayMsg::Quit => {
                    info!("Quitting controller...");
                    tray_icon.take();
                    *control_flow = ControlFlow::Exit;
                }
            }
        } else {
            trace!("Received window event: {:?}", event);
        }
    })
}

fn build_tray_icon(chan: TrayChan) -> Result<TrayIcon> {
    let icon = load_icon(LOGO);
    let tray_menu = Menu::new();

    let quit_i = MenuItem::new("Quit", true, None);
    let quit_id = quit_i.id().clone();
    tray_menu.append_items(&[
        &PredefinedMenuItem::about(
            None,
            Some(AboutMetadata {
                name: Some("codchi".to_string()),
                copyright: Some("Copyright codchi".to_string()),
                ..Default::default()
            }),
        ),
        &PredefinedMenuItem::separator(),
        &quit_i,
    ])?;

    let tray_icon = TrayIconBuilder::new()
        .with_menu(Box::new(tray_menu))
        .with_tooltip("Codchi controller")
        .with_icon(icon)
        .build()?;

    MenuEvent::set_event_handler(Some(move |evt: MenuEvent| {
        if evt.id == quit_id {
            chan.send(TrayMsg::Quit);
        } else {
            trace!("Received menu event from tray icon: {:?}", evt);
        }
    }));
    TrayIconEvent::set_event_handler(Some(|evt| {
        trace!("Received tray icon event: {:?}", evt);
    }));

    Ok(tray_icon)
}

fn load_icon(bytes: &[u8]) -> tray_icon::Icon {
    let (icon_rgba, icon_width, icon_height) = {
        let image = image::load_from_memory(bytes)
            .expect("Failed to parse image")
            .into_rgba8();
        let (width, height) = image.dimensions();
        let rgba = image.into_raw();
        (rgba, width, height)
    };
    tray_icon::Icon::from_rgba(icon_rgba, icon_width, icon_height).expect("Failed to open icon")
}
