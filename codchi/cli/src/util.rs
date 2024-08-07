use crate::consts::PathExt;
use anyhow::bail;
use std::{
    env,
    fmt::Display,
    fs, io,
    marker::PhantomData,
    path::{Path, PathBuf},
    thread,
    time::{Duration, Instant},
};

pub trait UtilExt {
    fn finally<F>(self, f: F) -> Self
    where
        F: FnOnce();

    fn peek<F>(self, f: F) -> Self
    where
        F: FnOnce(&Self);

    fn ignore(self);
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

    fn ignore(self) {}
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

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
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

    fn trace_err(self, msg: &str) -> Self;
}

impl<T, E: Display> ResultExt<E> for Result<T, E> {
    fn recover_err<F>(self, f: F) -> Self
    where
        F: FnOnce(E) -> Self,
    {
        match self {
            Ok(x) => Ok(x),
            Err(err) => f(err),
        }
    }

    fn trace_err(self, msg: &str) -> Self {
        self.map_err(|err| {
            log::debug!("{msg}: {err}");
            err
        })
    }
}

pub fn dbg_duration<F, R>(title: &str, f: F) -> R
where
    F: FnOnce() -> R,
{
    let start = Instant::now();
    let result = f();
    let duration = start.elapsed();

    log::debug!("Time elapsed in {title}: {duration:?}");

    result
}

pub fn with_tmp_file<F, T>(name: &str, f: F) -> anyhow::Result<T>
where
    F: Fn(&PathBuf) -> anyhow::Result<T>,
{
    let path = env::temp_dir().get_or_create()?.join(name);
    if path.try_exists().is_ok_and(|exists| exists) {
        bail!("Tmpfile {} already exists!", path.display());
    }

    f(&path).finally(|| {
        let _ = fs::remove_file(&path)
            .map_err(|err| log::warn!("Failed deleting tmpfile {}: {err}", path.display()));
    })
}

pub fn store_path_base(path: &str) -> String {
    path.split('/')
        .last()
        .unwrap_or("")
        .split_once('-')
        .map_or("", |x| x.1)
        .trim_end_matches(".drv")
        .to_string()
}
