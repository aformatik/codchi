use crate::Result;
use std::{
    env,
    io::BufRead,
    path::PathBuf,
    process::{Command, Stdio},
};

pub fn render(md_path: PathBuf, man_path: &PathBuf, _cmd: &clap::Command) -> Result<()> {
    let lua_filter_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR")?)
        .join("build")
        .join("filter.lua");
    let out = Command::new("pandoc")
        .arg(man_path)
        .arg("-o")
        .arg(&md_path)
        .arg("-L")
        .arg(lua_filter_path)
        .arg("--shift-heading-level-by=1")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?
        .wait_with_output()?;
    for line in out.stderr.lines() {
        println!("cargo:warning={}", line?);
    }
    if !out.status.success() {
        panic!("Failed to run pandoc on {:?}", man_path);
    }
    let out = Command::new("sed")
        // .args(["-i", r"s/\\\([^$]\)/\1/g"])
        .args(["-i", r"s/\\//g"])
        .arg(&md_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?
        .wait_with_output()?;
    for line in out.stderr.lines() {
        println!("cargo:warning={}", line?);
    }
    if !out.status.success() {
        panic!("Failed to run sed on {:?}", man_path);
    }

    Ok(())
}
