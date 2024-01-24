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

    fn peek<F>(self, f: F) -> Self
    where
        F: FnOnce(&Self),
    {
        f(&self);
        self
    }
}
