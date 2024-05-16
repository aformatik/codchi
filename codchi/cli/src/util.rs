use indicatif::ProgressBar;
use std::{borrow::Cow, fs, io, marker::PhantomData, path::Path, thread, time::Duration};

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
        #[allow(clippy::permissions_set_readonly_false)]
        perms.set_readonly(false);
        fs::set_permissions(&path, perms)?;
    };
    Ok(())
}

pub trait StringExt: Sized {
    fn none_if_empty(self) -> Option<Self>;
}

impl StringExt for String {
    fn none_if_empty(self) -> Option<Self> {
        if self.is_empty() {
            None
        } else {
            Some(self)
        }
    }
}

impl StringExt for &str {
    fn none_if_empty(self) -> Option<Self> {
        if self.is_empty() {
            None
        } else {
            Some(self)
        }
    }
}

pub trait Hkd {
    type Hk<A: Eq + Clone>: ToOption<A> + Eq + Clone;
}

#[derive(Default, Debug, Clone)]
pub struct Optional;
impl Hkd for Optional {
    type Hk<A: Eq + Clone> = Option<A>;
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Required;
impl Hkd for Required {
    type Hk<A: Eq + Clone> = A;
}

#[derive(Default, Debug, Clone)]
pub struct Empty;
impl Hkd for Empty {
    type Hk<A: Eq + Clone> = PhantomData<A>;
}

pub trait ToOption<T> {
    fn to_option(self) -> Option<T>;
}

impl<T> ToOption<T> for Option<T> {
    fn to_option(self) -> Option<T> {
        self
    }
}

impl<T> ToOption<T> for T {
    fn to_option(self) -> Option<T> {
        Some(self)
    }
}

impl<T> ToOption<T> for PhantomData<T> {
    fn to_option(self) -> Option<T> {
        None
    }
}

pub trait ResultExt<E> {
    fn recover_err<F>(self, f: F) -> Self
    where
        F: FnOnce(E) -> Self;
}

impl<T, E> ResultExt<E> for Result<T, E> {
    fn recover_err<F>(self, f: F) -> Self
    where
        F: FnOnce(E) -> Self,
    {
        match self {
            Ok(x) => Ok(x),
            Err(err) => f(err),
        }
    }
}
