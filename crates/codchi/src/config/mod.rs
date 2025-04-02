use anyhow::Result;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use serde_with::{serde_as, DisplayFromStr};
use shared::config::codchi;
use std::{
    collections::HashMap,
    fmt::{self, Display},
    fs,
    str::FromStr,
};
use strum::EnumString;

pub mod flake;
pub mod machine;
pub mod output;
pub use codchi::*;
pub use flake::*;
pub use machine::*;
pub use output::*;
