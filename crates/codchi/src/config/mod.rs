use anyhow::Result;
use fs4::fs_std::FileExt;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use serde_with::{serde_as, DisplayFromStr};
use std::{
    collections::HashMap,
    fmt::{self, Display},
    fs,
    io::{Read, Seek, Write},
    path::Path,
    str::FromStr,
};
use strum::EnumString;

pub mod codchi;
pub mod flake;
pub mod machine;
pub mod output;
pub use codchi::*;
pub use flake::*;
pub use machine::*;
pub use output::*;

pub struct LockedConfig(fs::File);

impl LockedConfig {
    /// Read file to string and (write-)lock it (until `Self` is dropped)
    pub fn open<P: AsRef<Path> + fmt::Debug>(path: P, write_mode: bool) -> Result<(Self, String)> {
        if !write_mode && fs::metadata(&path).is_err() {
            fs::OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .truncate(false)
                .open(&path)?;
        }
        let file = fs::OpenOptions::new()
            .read(true)
            .write(write_mode)
            .create(write_mode)
            .open(&path)?;
        if write_mode {
            file.lock_exclusive()?;
        } else {
            file.lock_shared()?;
        }
        let mut file = LockedConfig(file);
        let content = {
            let size = file.0.metadata().map(|m| m.len() as usize).ok();
            let mut content = String::with_capacity(size.unwrap_or(0));
            file.0.read_to_string(&mut content)?;
            file.0.rewind()?;
            content
        };

        Ok((file, content))
    }

    /// Same as `open` but also parses the file contents
    pub fn open_parse<T, P, F, D>(
        path: P,
        write_mode: bool,
        parse: F,
        default: D,
    ) -> Result<(Self, T)>
    where
        P: AsRef<Path> + fmt::Debug,
        F: Fn(&str) -> Result<T>,
        D: Fn() -> Result<T>,
    {
        let (lock, content) = Self::open(&path, write_mode)?;
        let val = if !content.is_empty() {
            parse(&content)
                .map_err(|err| {
                    log::warn!("Failed parsing config at '{path:?}':\n{err}\n Using default value.")
                })
                .or_else(|_| default())
        } else {
            default()
        }?;
        Ok((lock, val))
    }

    // #[must_use]
    pub fn write(mut self, content: String) -> Result<()> {
        // self.get_machines().set_implicit(false); TODO
        let bytes = content.as_bytes();
        self.0.set_len(bytes.len() as u64)?;
        self.0.write_all(bytes)?;
        drop(self);
        Ok(())
    }
}

impl Drop for LockedConfig {
    fn drop(&mut self) {
        self.0
            .unlock()
            .unwrap_or_else(|err| panic!("Failed unlocking '{:?}'. Reason: {err}", self.0));
    }
}
