use indicatif::ProgressBar;
use std::{borrow::Cow, fs, io, path::Path, thread, time::Duration};

use crate::ROOT_PROGRESS_BAR;

pub trait UtilExt {
    fn finally<F>(self, f: F) -> Self
    where
        F: FnOnce();

    fn peek<F>(self, f: F) -> Self
    where
        F: FnOnce(&Self);
}

impl<A> UtilExt for A {
    fn finally<F>(self, f: F) -> Self
    where
        F: FnOnce(),
    {
        f();
        self
    }

    #[inline]
    fn peek<F>(self, f: F) -> Self
    where
        F: FnOnce(&Self),
    {
        f(&self);
        self
    }
}

pub fn try_n_times<E, F>(interval: Duration, n: usize, f: F) -> Result<bool, E>
where
    F: Fn() -> Result<bool, E>,
{
    for _ in 0..n {
        if f()? {
            return Ok(true);
        }
        thread::sleep(interval);
    }
    Ok(false)
}

pub fn with_spinner<A, E, T, F>(msg: T, f: F) -> Result<A, E>
where
    T: Into<Cow<'static, str>>,
    F: Fn(&mut ProgressBar) -> Result<A, E>,
{
    let root = ROOT_PROGRESS_BAR
        .get()
        .expect("Root progressbar for logger not initialized.");
    let mut spinner = root.add(ProgressBar::new_spinner());

    spinner.enable_steady_tick(Duration::from_millis(100));
    spinner.set_message(msg);

    f(&mut spinner).finally(|| {
        spinner.finish_and_clear();
        root.remove(&spinner);
    })
}

pub fn make_writeable_if_exists<P: AsRef<Path>>(path: P) -> io::Result<()> {
    if let Ok(metadata) = fs::metadata(&path) {
        let mut perms = metadata.permissions();
        perms.set_readonly(false);
        fs::set_permissions(&path, perms)?;
    };
    Ok(())
}
