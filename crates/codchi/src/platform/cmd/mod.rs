use serde::Deserialize;
use thiserror::Error;

pub use cmd::*;
pub use shared::cmd;

pub mod linux;
pub use linux::*;
pub mod nix;
