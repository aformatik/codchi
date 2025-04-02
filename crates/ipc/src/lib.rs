#![feature(let_chains)]
#![feature(once_cell_try)]
// #![deny(unused_crate_dependencies)]

use std::{
    net::{IpAddr, Ipv4Addr},
    sync::LazyLock,
};

use tokio::runtime::Runtime;

pub mod client;
pub mod service;

pub const SERVER_ADDR: (IpAddr, u16) = (SERVER_IP, SERVER_PORT);
pub const SERVER_IP: IpAddr = IpAddr::V4(Ipv4Addr::LOCALHOST);
pub const SERVER_PORT: u16 = 28325;

/// single threaded tokio runtime
pub static RUNTIME_MT: LazyLock<Runtime> = LazyLock::new(|| {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("Failed creating tokio runtime...")
});
