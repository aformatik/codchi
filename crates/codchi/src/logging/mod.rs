use std::{
    borrow::Cow,
    io::Write,
    sync::{Arc, OnceLock, RwLock},
};

use anyhow::{Context, Result};
use console::style;
use indicatif_log_bridge::LogWrapper;
use log::{Level, LevelFilter};
use progress::{Progress, ROOT_BAR};

mod nix;
pub mod output;
mod progress;
pub use output::*;

fn progress() -> &'static Arc<RwLock<Option<Progress>>> {
    static PROGRESS: OnceLock<Arc<RwLock<Option<Progress>>>> = OnceLock::new();
    PROGRESS.get_or_init(Arc::default)
}

pub fn init(cli_level: LevelFilter) -> Result<()> {
    let mut logger = env_logger::Builder::new();
    logger.filter_level(cli_level);
    logger.format(|buf, record| {
        {
            write!(buf, "[")?;
            let level = style(record.level());
            let level = match record.level() {
                Level::Error => level.red().bold(),
                Level::Warn => level.yellow(),
                Level::Info => level.green(),
                Level::Debug => level.blue(),
                Level::Trace => level.cyan(),
            };
            write!(buf, "{level}")?;

            if !record.target().starts_with("codchi") {
                write!(buf, " {}", record.target())?;
            };

            write!(buf, "] ")?;
        }
        writeln!(buf, "{}", record.args())
    });
    let logger = logger.parse_env("CODCHI_LOG").build();
    LogWrapper::new(ROOT_BAR.clone(), logger)
        .try_init()
        .context("Failed initializing logger")?;

    Ok(())
}

fn with_progress<F: FnOnce(&mut Progress)>(f: F) {
    let mut locked = progress().write().unwrap();
    let mut progress = locked.take().unwrap_or_default();
    f(&mut progress);
    *locked = Some(progress);
}

pub fn set_progress_status<M: Into<Cow<'static, str>>>(status: M) {
    with_progress(|progress| progress.set_status(status));
}

pub fn log_progress(fallback_target: &str, fallback_level: Level, msg: &str) {
    with_progress(|progress| progress.log(fallback_target, fallback_level, msg));
}

pub fn hide_progress() {
    let mut progress = progress().write().unwrap();
    drop(progress.take());
}

pub fn with_suspended_progress<R>(prompt: impl Fn() -> R) -> R {
    ROOT_BAR.suspend(prompt)
}

#[macro_export]
macro_rules! progress_scope {
    ($($body:tt)*) => {
        {
            let result = {
                $($body)*
            };
            $crate::logging::hide_progress();
            result
        }
    }
}
