use anyhow::{bail, Context};
use std::{
    env,
    fmt::{Debug, Display},
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

pub fn try_n_times<A, E, F>(interval: Duration, n: usize, f: F) -> Result<A, E>
where
    F: Fn() -> Result<A, E>,
{
    let mut i = 1;
    loop {
        match f() {
            Ok(x) => return Ok(x),
            Err(e) if i == n => return Err(e),
            _ => {}
        }
        i = i + 1;
        thread::sleep(interval);
    }
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
    let nonce: u32 = rand::random();
    let path = env::temp_dir()
        .get_or_create()?
        .join(format!("{name}-{nonce}"));
    if path.try_exists().is_ok_and(|exists| exists) {
        bail!("Tmpfile {} already exists!", path.display());
    }

    f(&path).finally(|| {
        let _ = fs::remove_file(&path)
            .map_err(|err| log::debug!("Failed deleting tmpfile {}: {err}", path.display()));
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

pub trait PathExt: AsRef<Path> + Sized + Debug {
    /// Create the directory recursively if it doesn't exist and return its path
    fn get_or_create(&self) -> anyhow::Result<&Self> {
        // Especially on windows folder creation fails silently / doesn't propagate to WSL
        for _ in 0..5 {
            if fs::metadata(self).is_err() {
                fs::create_dir_all(self)?;
                thread::sleep(Duration::from_millis(10));
            } else {
                return Ok(self);
            }
        }
        bail!("Failed to create directory '{self:?}' recursively after five tries...");
    }

    /// Remove the directory and all of its contents if it exists and create an empty folder
    fn cleanup_and_get(self) -> anyhow::Result<Self> {
        if fs::metadata(&self).is_ok() {
            fs::remove_dir_all(&self)?;
        }
        self.get_or_create()?;
        Ok(self)
    }

    /// Remove the directory and log::warn if an error occured
    fn remove(self) {
        if let Ok(meta) = fs::metadata(&self) {
            let result = if meta.is_dir() {
                fs::remove_dir_all(&self)
            } else {
                fs::remove_file(&self)
            };
            if let Err(err) = result {
                log::warn!("Could not remove '{self:?}'. Reason: {err}");
            }
        } else {
            log::trace!("Not removing non existant path '{self:?}'");
        }
    }

    fn assert_exists(&self) -> io::Result<()> {
        fs::metadata(self)?;
        Ok(())
    }

    /// Create the directory recursively if it doesn't exist and return its path
    fn list_dir(&self) -> anyhow::Result<Vec<String>> {
        if let Ok(meta) = fs::metadata(self) {
            if meta.is_dir() {
                let mut filenames = vec![];

                for entry in fs::read_dir(self)? {
                    let entry =
                        entry.with_context(|| format!("Failed to read an entry in {:?}", self))?;
                    filenames.push(entry.file_name().to_string_lossy().to_string());
                }
                Ok(filenames)
            } else {
                bail!("Path '{self:?}' is not a directory")
            }
        } else {
            bail!("Couldn't find path '{self:?}'")
        }
    }
}

impl<P: AsRef<Path> + Debug> PathExt for P {}

#[derive(Debug, Clone)]
pub struct LinuxPath(pub String);

impl std::fmt::Display for LinuxPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
