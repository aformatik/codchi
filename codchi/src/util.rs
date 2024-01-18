pub trait Finally {
    fn finally<F>(self, f: F) -> Self
    where
        F: FnOnce();
}

impl<A> Finally for A {
    fn finally<F>(self, f: F) -> Self
    where
        F: FnOnce(),
    {
        f();
        self
    }
}
