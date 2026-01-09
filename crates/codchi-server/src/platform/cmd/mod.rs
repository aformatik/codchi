mod linux;
mod nix;

pub use linux::*;
pub use nix::*;

// fn realpath(&self, path: &LinuxPath) -> anyhow::Result<LinuxPath> {
//     let realpath = self
//         .run("realpath", &[&path.0])
//         .output_utf8_ok()
//         .map(|path| path.trim().to_owned())?;
//
//     log::trace!("Resolved real path: '{path}' -> '{realpath}'");
//
//     Ok(LinuxPath(realpath))
// }
