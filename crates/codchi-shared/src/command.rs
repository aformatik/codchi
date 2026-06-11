//! Checked subprocess helpers shared by the host-side platform adapters.

use std::fmt::Debug;
use std::process::{Command, ExitStatus, Output};

use serde::de::DeserializeOwned;

/// A subprocess failure with the command and captured output preserved.
#[derive(Debug, thiserror::Error)]
pub enum CommandError {
    #[error("could not run {command}: {source}")]
    Spawn {
        command: String,
        #[source]
        source: std::io::Error,
    },
    #[error("{command} failed with {status}; stdout: {stdout}; stderr: {stderr}")]
    Failed {
        command: String,
        status: ExitStatus,
        stdout: String,
        stderr: String,
    },
    #[error("{command} returned non-UTF-8 output: {source}")]
    Utf8 {
        command: String,
        #[source]
        source: std::string::FromUtf8Error,
    },
    #[error("{command} returned invalid JSON: {source}")]
    Json {
        command: String,
        #[source]
        source: serde_json::Error,
    },
}

/// The small checked-command surface used by platform drivers.
pub trait CommandExt: Debug {
    fn output_checked(&mut self) -> Result<Output, CommandError>;

    fn output_utf8(&mut self) -> Result<String, CommandError> {
        let command = format!("{self:?}");
        String::from_utf8(self.output_checked()?.stdout)
            .map_err(|source| CommandError::Utf8 { command, source })
    }

    fn output_json<T: DeserializeOwned>(&mut self) -> Result<T, CommandError> {
        let command = format!("{self:?}");
        serde_json::from_slice(&self.output_checked()?.stdout)
            .map_err(|source| CommandError::Json { command, source })
    }

    fn wait_ok(&mut self) -> Result<(), CommandError> {
        self.output_checked().map(|_| ())
    }
}

impl CommandExt for Command {
    fn output_checked(&mut self) -> Result<Output, CommandError> {
        let command = format!("{self:?}");
        let output = self.output().map_err(|source| CommandError::Spawn {
            command: command.clone(),
            source,
        })?;
        if output.status.success() {
            return Ok(output);
        }
        Err(CommandError::Failed {
            command,
            status: output.status,
            stdout: String::from_utf8_lossy(&output.stdout).trim().to_owned(),
            stderr: String::from_utf8_lossy(&output.stderr).trim().to_owned(),
        })
    }
}
