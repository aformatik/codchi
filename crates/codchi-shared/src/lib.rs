//! Non-wire code shared by active Codchi v1 crates.
//!
//! Kept deliberately lean (D4): only what v1 actually needs is pulled forward
//! from the retired `beta-shared` grab-bag. The wire contract lives in
//! `codchi-api`; this crate holds host-side common code such as the per-user
//! transport path both the server and its clients must agree on.

pub mod command;
pub mod paths;

pub use command::{CommandError, CommandExt};
pub use paths::{
    STORE_CONTAINER_NAME, STORE_NIX_DIR, STORE_NIX_VOLUME_NAME, data_dir, logs_dir, runtime_dir,
    server_socket_path,
};
