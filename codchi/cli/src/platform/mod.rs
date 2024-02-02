mod cmd;
mod machine;
mod store;

#[allow(clippy::module_inception)]
#[cfg_attr(target_os = "linux", path = "linux/mod.rs")]
#[cfg_attr(target_os = "windows", path = "windows/mod.rs")]
mod platform;

pub use self::cmd::*;
pub use self::machine::*;
pub use self::nix::NixDriver;
use self::platform::StoreImpl;
pub use self::store::*;

use anyhow::Result;
use std::sync::OnceLock;

pub struct Driver {
    store: StoreImpl,
}

impl Driver {
    pub fn get() -> &'static Driver {
        static DRIVER: OnceLock<Driver> = OnceLock::new();
        let result: Result<&'static Driver> = DRIVER.get_or_try_init(|| {
            let store = Store::init(private::Private)?;
            Ok(Self { store })
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
