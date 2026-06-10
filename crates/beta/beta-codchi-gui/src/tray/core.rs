use anyhow::Result;
use shared::util::{ResultExt, UtilExt};
use std::{collections::HashMap, fmt::Debug, sync::mpsc::Receiver, thread};
use tao::{
    event_loop::{ControlFlow, EventLoopBuilder, EventLoopProxy},
    window::WindowBuilder,
};
use tray_icon::{
    menu::{
        self, CheckMenuItem, IconMenuItem, IsMenuItem, Menu, MenuEvent, MenuId, MenuItem,
        PredefinedMenuItem, Submenu,
    },
    Icon, TrayIcon, TrayIconBuilder, TrayIconEvent,
};

#[derive(Clone, Debug)]
pub enum TrayOp<A> {
    Noop,
    Quit,
    UpdateState(fn(&mut A)),
    WaitStatus(bool),
    ItemClicked(MenuId),
    // OpenTrayMenu,
}

#[allow(dead_code)]
#[derive(Clone)]
pub enum TrayItem<'a, A: 'static> {
    Separator,
    Submenu {
        text: String,
        children: Vec<TrayItem<'a, A>>,
    },
    Label {
        text: String,
        icon: Option<menu::Icon>,
    },
    Button {
        text: String,
        on_click: &'a dyn Fn(MenuItem) -> TrayOp<A>,
    },
    Checkbox {
        text: String,
        checked: bool,
        on_click: &'a dyn Fn(CheckMenuItem) -> TrayOp<A>,
    },
    Dynamic {
        item: Box<TrayItem<'a, A>>,
        on_update: &'a dyn Fn(&A, &dyn IsMenuItem),
    },
}

impl<'a, A> TrayItem<'a, A> {
    pub fn dynamic<M: IsMenuItem + 'static>(
        item: TrayItem<'a, A>,
        on_update: impl Fn(&A, &M) + 'a,
    ) -> Self {
        fn unsafe_cast<M: IsMenuItem>(x: &dyn IsMenuItem) -> &M {
            unsafe { &*(x as *const dyn IsMenuItem as *const M) }
        }

        Self::Dynamic {
            item: Box::new(item),
            on_update: Box::leak(Box::new(move |state: &A, inner: &dyn IsMenuItem| {
                on_update(state, unsafe_cast(inner));
            })),
        }
    }
}

// pub type Lazy<A> = Box<dyn Fn(&A) -> TrayItem<A>>;
pub type DynamicItems<'a, A> = HashMap<MenuId, (TrayItem<'a, A>, Box<dyn IsMenuItem>)>;
pub type Handlers<'a, A> = HashMap<MenuId, &'a dyn Fn() -> TrayOp<A>>;

// #[derive(Clone)]
pub struct Tray<'a, A: 'static> {
    pub tooltip: &'static str,
    pub icon: Icon,
    pub items: Vec<TrayItem<'a, A>>,
}

#[allow(dead_code)] // tray must not be dropped!
struct TrayState<'a, A: 'static> {
    state: A,
    tray: TrayIcon,
    menu: Menu,
    dynamic_items: DynamicItems<'a, A>,
    handlers: Handlers<'a, A>,
}

impl<'a: 'static, A: 'static + Send + Debug + Clone> Tray<'a, A> {
    pub fn run(self, initial: A, update_rx: Receiver<TrayOp<A>>) -> Result<()> {
        let event_loop = EventLoopBuilder::<TrayOp<A>>::with_user_event().build();
        let chan = event_loop.create_proxy();

        let window = WindowBuilder::new()
            .with_visible(false)
            .build(&event_loop)?;

        let mut tray_state = None;

        let chan2 = chan.clone();
        thread::spawn(move || {
            update_rx
                .iter()
                .for_each(|op| chan2.send_event(op).trace_err("sending event").ignore())
        });

        event_loop.run(move |event, _, control_flow| {
            // We add delay of 16 ms (60fps) to event_loop to reduce cpu load.
            // This can be removed to allow ControlFlow::Poll to poll on each cpu cycle
            // Alternatively, you can set ControlFlow::Wait or use TrayIconEvent::set_event_handler,
            // see https://github.com/tauri-apps/tray-icon/issues/83#issuecomment-1697773065
            *control_flow = ControlFlow::WaitUntil(
                std::time::Instant::now() + std::time::Duration::from_millis(16),
            );

            if let tao::event::Event::NewEvents(tao::event::StartCause::Init) = event {
                // We create the icon once the event loop is actually running
                // to prevent issues like https://github.com/tauri-apps/tray-icon/issues/90
                tray_state = Some(self.build(initial.clone(), chan.clone()).unwrap());

                // We have to request a redraw here to have the icon actually show up.
                // Tao only exposes a redraw method on the Window so we use core-foundation directly.
                #[cfg(target_os = "macos")]
                unsafe {
                    use core_foundation::runloop::{CFRunLoopGetMain, CFRunLoopWakeUp};

                    let rl = CFRunLoopGetMain();
                    CFRunLoopWakeUp(rl);
                }
            }

            if let tao::event::Event::UserEvent(evt) = event {
                if let Some(tray) = tray_state.as_mut() {
                    let mut msg_stack = Some(evt);
                    while let Some(msg) = msg_stack.clone() {
                        msg_stack.take();
                        match msg {
                            TrayOp::Noop => {
                                log::trace!("noop");
                            }
                            TrayOp::Quit => {
                                log::info!("Quitting controller...");
                                *control_flow = ControlFlow::Exit;
                            }
                            TrayOp::UpdateState(f) => {
                                f(&mut tray.state);
                                self.update_items(tray).unwrap();
                            }
                            TrayOp::WaitStatus(waiting) => {
                                window.set_cursor_icon(if waiting {
                                    tao::window::CursorIcon::Wait
                                } else {
                                    tao::window::CursorIcon::Default
                                });
                            }
                            TrayOp::ItemClicked(item_id) => {
                                if let Some(handler) = tray.handlers.get(&item_id) {
                                    log::trace!("Found handler for item {item_id:?}");
                                    msg_stack = Some(handler());
                                }
                            } // TrayOp::OpenTrayMenu =>
                              // {
                              //     #[cfg(target_os = "windows")]
                              //     (|| {
                              //         let hwnd = tray.menu.hpopupmenu();
                              //         tray.menu.init_for_hwnd(hwnd)?;
                              //         tray.menu.show_for_hwnd(hwnd)?;
                              //         anyhow::Ok(())
                              //     })()
                              //     .trace_err("Failed to open tray menu")
                              //     .ignore()
                              // }
                        }
                    }
                }
                if *control_flow == ControlFlow::Exit {
                    tray_state.take();
                }
            } else {
                // very verbose on windows
                // log::trace!("Received window event: {:?}", event);
            }
        })
    }

    fn build(&self, state: A, chan: EventLoopProxy<TrayOp<A>>) -> Result<TrayState<'a, A>> {
        let menu = Menu::new();
        let mut handlers: Handlers<A> = HashMap::new();
        let mut items: DynamicItems<A> = HashMap::new();

        trait Container {
            fn add(&self, item: &dyn IsMenuItem) -> Result<()>;
        }

        impl Container for Menu {
            fn add(&self, item: &dyn IsMenuItem) -> Result<()> {
                self.append(item)?;
                Ok(())
            }
        }

        impl Container for Submenu {
            fn add(&self, item: &dyn IsMenuItem) -> Result<()> {
                self.append(item)?;
                Ok(())
            }
        }

        fn collect_items<'a, A: Clone>(
            handlers: &mut Handlers<'a, A>,
            items: &mut DynamicItems<'a, A>,
            parent: &impl Container,
            item: &TrayItem<'a, A>,
        ) -> Result<Option<Box<dyn IsMenuItem>>> {
            Ok(match &item {
                TrayItem::Separator => {
                    let inner = PredefinedMenuItem::separator();
                    parent.add(&inner)?;
                    Some(Box::new(inner))
                }
                TrayItem::Submenu { text, children } => {
                    let menu = Submenu::new(text, true);
                    for child in children {
                        collect_items(handlers, items, &menu, child)?;
                    }
                    parent.add(&menu)?;
                    Some(Box::new(menu))
                }
                TrayItem::Button { on_click, text } => {
                    let mi = MenuItem::new(text, true, None);
                    let on_click = *on_click;
                    parent.add(&mi)?;
                    let mi2 = mi.clone();
                    handlers.insert(
                        mi.id().clone(),
                        Box::leak(Box::new(move || on_click(mi2.clone()))),
                    );
                    Some(Box::new(mi))
                }
                TrayItem::Checkbox {
                    text,
                    checked,
                    on_click,
                } => {
                    let mi = CheckMenuItem::new(text, true, *checked, None);
                    let on_click = *on_click;
                    parent.add(&mi)?;
                    let mi2 = mi.clone();
                    handlers.insert(
                        mi.id().clone(),
                        Box::leak(Box::new(move || on_click(mi2.clone()))),
                    );
                    Some(Box::new(mi))
                }
                TrayItem::Label { text, icon } => {
                    let mi = IconMenuItem::new(text, false, icon.clone(), None);
                    parent.add(&mi)?;
                    Some(Box::new(mi))
                }
                TrayItem::Dynamic {
                    item: inner_item, ..
                } => {
                    if let Some(inner) = collect_items(handlers, items, parent, inner_item)? {
                        items.insert(inner.id().clone(), (item.clone(), inner));
                    }
                    None
                }
            })
        }

        for item in &self.items {
            collect_items(&mut handlers, &mut items, &menu, item)?;
        }

        let tray = TrayIconBuilder::new()
            .with_menu(Box::new(menu.clone()))
            .with_tooltip(self.tooltip)
            .with_icon(self.icon.clone())
            .with_menu_on_left_click(true)
            .build()?;

        let chan2 = chan.clone();
        MenuEvent::set_event_handler(Some(move |evt: MenuEvent| {
            log::trace!("Received menu event from tray icon: {:?}", evt);
            chan2
                .send_event(TrayOp::ItemClicked(evt.id().clone()))
                .trace_err("Failed sending tray item clicked event")
                .ignore();
        }));
        TrayIconEvent::set_event_handler(Some(move |evt| {
            log::trace!("Received tray icon event: {:?}", evt);
        }));

        Ok(TrayState {
            state,
            menu,
            tray,
            dynamic_items: items,
            handlers,
        })
    }

    fn update_items(&self, state: &mut TrayState<A>) -> Result<()> {
        fn update<A>(state: &A, item: &TrayItem<A>, inner: &dyn IsMenuItem) {
            match item {
                TrayItem::Submenu { children, .. } => children
                    .iter()
                    .for_each(|child| update(state, child, inner)),
                TrayItem::Dynamic { on_update, .. } => on_update(state, inner),
                _ => {}
            }
        }
        for (item, inner) in state.dynamic_items.values() {
            update(&state.state, item, &**inner);
        }
        Ok(())
    }
}

pub trait FromRgba: Sized {
    fn from_rgba(rgba: Vec<u8>, width: u32, height: u32) -> Self;
    fn load_icon(bytes: &[u8]) -> Self {
        let image = image::load_from_memory(bytes)
            .expect("Failed to parse image")
            .into_rgba8();
        let (width, height) = image.dimensions();
        let rgba = image.into_raw();
        Self::from_rgba(rgba, width, height)
    }
}

impl FromRgba for Icon {
    fn from_rgba(rgba: Vec<u8>, width: u32, height: u32) -> Self {
        Icon::from_rgba(rgba, width, height).expect("Failed to open icon")
    }
}
impl FromRgba for menu::Icon {
    fn from_rgba(rgba: Vec<u8>, width: u32, height: u32) -> Self {
        menu::Icon::from_rgba(rgba, width, height).expect("Failed to open icon")
    }
}
