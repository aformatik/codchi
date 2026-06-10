use std::{
    fs::File,
    process::{Command, Stdio},
};

use serde::{Deserialize, Serialize};
use shared::cmd::{CommandExt, Result};

use crate::state::PlatformStatus;

#[allow(non_snake_case)]
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Info {}

pub fn info() -> Result<Info> {
    Command::new("podman")
        .args(["info", "--format=json"])
        .output_json()
}

#[allow(non_snake_case)]
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct PsContainer {
    pub Id: String,
    pub Names: Vec<String>,
    pub State: String,
}

pub fn ps() -> Result<Vec<PsContainer>> {
    Command::new("podman")
        .args(["ps", "--all", "--format=json"])
        .output_json()
}

pub fn ps_filter_name(name: &str) -> Result<Option<PsContainer>> {
    Ok(Command::new("podman")
        .args([
            "ps",
            "--all",
            "--format=json",
            &format!("--filter=name={name}"),
        ])
        .output_json::<Vec<PsContainer>>()?
        .first()
        .cloned())
}

pub fn get_platform_status(name: &str) -> Result<PlatformStatus> {
    Ok(match ps_filter_name(name)? {
        None => PlatformStatus::NotInstalled,
        Some(container) => {
            if container.State == "running" {
                PlatformStatus::Running
            } else {
                PlatformStatus::Stopped
            }
        }
    })
}

pub fn load(image_tar_path: &str) -> Result<String> {
    let output = Command::new("podman")
        .args(["load", "-q", "-i", image_tar_path])
        .output_utf8_ok()?;

    let image_label = output
        .lines()
        .next()
        .ok_or(anyhow::anyhow!("Invalid output from `podman load`"))?
        .split(" ")
        .last()
        .ok_or(anyhow::anyhow!(
            "Invalid output from `podman load`. Image name:tag is missing"
        ))?;
    Ok(image_label.to_string())
}

pub fn rmi(image_label: &str) -> Result<()> {
    Command::new("podman").args(["rmi", image_label]).wait_ok()
}
pub fn rm(container_name: &str, force: bool) -> Result<()> {
    let mut cmd = Command::new("podman");
    cmd.arg("rm");
    if force {
        cmd.arg("-f");
    }
    cmd.arg(container_name);
    cmd.wait_ok()
}
