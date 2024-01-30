// pub mod cli {
//     use std::{
//         io,
//         path::PathBuf,
//         process::{Command, Stdio},
//     };

//     use serde::Deserialize;

//     #[allow(dead_code)]
//     fn nix(args: &[&str]) -> io::Result<()> {
//         let mut cmd = Command::new("nix");
//         for arg in args.iter() {
//             cmd.arg(arg);
//         }

//         let status = cmd.spawn()?.wait()?;
//         if status.success() {
//             Ok(())
//         } else {
//             Err(io::Error::new(
//                 io::ErrorKind::Other,
//                 format!("Nix {:?} failed with {}", args, status),
//             ))
//         }
//     }

//     fn nix_output(args: &[&str]) -> io::Result<Vec<u8>> {
//         let mut cmd = Command::new("nix");
//         for arg in args.iter() {
//             cmd.arg(arg);
//         }
//         cmd.stdout(Stdio::piped());

//         let output = cmd.spawn()?.wait_with_output()?;
//         if output.status.success() {
//             Ok(output.stdout)
//         } else {
//             Err(io::Error::new(
//                 io::ErrorKind::Other,
//                 format!("Nix {:?} failed with {}", args, output.status),
//             ))
//         }
//     }

//     #[allow(dead_code)]
//     fn nix_json<D: for<'de> Deserialize<'de>>(args: &[&str]) -> io::Result<D> {
//         let json = nix_output(args)?;
//         serde_json::from_slice::<D>(&json).map_err(|err| {
//             io::Error::new(
//                 io::ErrorKind::Other,
//                 format!("Nix info: failed to parse json: {}", err),
//             )
//         })
//     }

//     pub fn build(flake_url: &str) -> io::Result<PathBuf> {
//         let bytes = nix_output(&["build", "--no-link", "--print-out-paths", flake_url])?;
//         let utf8 = String::from_utf8(bytes).map_err(|err| {
//             io::Error::new(
//                 io::ErrorKind::Other,
//                 format!("Nix info: failed decode UTF8: {}", err),
//             )
//         })?;

//         utf8.lines().next().map(PathBuf::from).ok_or_else(|| {
//             io::Error::new(
//                 io::ErrorKind::Other,
//                 format!("Nix info: build produced no output: {}", utf8),
//             )
//         })
//     }
// }
