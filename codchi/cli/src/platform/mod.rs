mod cmd;
mod host;
mod machine;
mod store;

#[allow(clippy::module_inception)]
#[cfg_attr(target_os = "linux", path = "linux/mod.rs")]
#[cfg_attr(target_os = "windows", path = "windows/mod.rs")]
mod platform;

pub use self::cmd::*;
pub use self::host::*;
pub use self::machine::*;
pub use self::nix::NixDriver;
pub use self::store::*;

use self::platform::StoreImpl;
use anyhow::Result;
use std::sync::OnceLock;

pub struct Driver {
    store: StoreImpl,
}

impl Driver {
    fn get() -> &'static Driver {
        static DRIVER: OnceLock<Driver> = OnceLock::new();
        let result: Result<&'static Driver> = DRIVER.get_or_try_init(|| {
            Ok(Self {
                store: Store::init(private::Private)?,
            })
        });
        result.expect("Failed initializing Driver.")
    }

    pub fn store() -> &'static impl Store {
        &Self::get().store
    }
}

mod private {
    pub struct Private;
}
