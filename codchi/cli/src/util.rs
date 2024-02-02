use spinoff::*;
use std::{borrow::Cow, thread, time::Duration};

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
    F: Fn(&mut Spinner) -> Result<A, E>,
{
    let mut spinner =
        Spinner::new_with_stream(spinners::Dots, msg, Color::Blue, spinoff::Streams::Stderr);

    f(&mut spinner).finally(|| spinner.clear())
}
