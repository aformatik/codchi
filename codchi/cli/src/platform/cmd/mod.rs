use super::*;
use crate::util::UtilExt;
use anyhow::anyhow;
use serde::Deserialize;
use std::io::{BufRead, BufReader, Read};
use std::process::{Child, Stdio};
use std::sync::mpsc::{channel, Receiver, RecvTimeoutError, Sender};
use std::thread::{self, JoinHandle};
use std::time::Duration;
use std::{
    fmt::Debug,
    io,
    process::{exit, Command, ExitStatus},
    str::FromStr,
};
use thiserror::Error;

pub mod linux;
pub use linux::*;
pub mod nix;
// pub use nix::*;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to call command.")]
    IO(#[from] io::Error),

    #[error("Failed parsing JSON output.")]
    JSON(#[from] serde_json::Error),

    #[error("Failed parsing output string.")]
    Parse(#[from] anyhow::Error),

    #[error("{cmd} failed with exit status {exit_status:?}. Stderr:\n{stderr}")]
    Other {
        cmd: String,
        exit_status: ExitStatus,
        stderr: String,
    },
}
type Result<T> = std::result::Result<T, Error>;
type IsStderr = bool;
pub struct StreamingChild {
    pub rx: Receiver<(IsStderr, String)>,
    pub threads: Vec<JoinHandle<()>>,
    pub child: Child,
}

pub trait CommandExt: Debug {
    fn spawn(&mut self, out_ty: OutputType) -> Result<Child>;
    fn output_ok(&mut self) -> Result<Vec<u8>> {
        log::trace!("Running command: {self:?}");

        let out = self.spawn(OutputType::Collect)?.wait_with_output()?;
        if out.status.success() {
            Ok(out.stdout)
        } else {
            let stderr = String::from_utf8_lossy(&out.stderr).to_string();
            let stdout = String::from_utf8_lossy(&out.stdout).to_string();
            log::trace!(
                "Got error when running {self:?}:
Stdout:
{stdout}
Stderr:
{stderr}"
            );
            Err(Error::Other {
                cmd: format!("{self:?}"),
                exit_status: out.status,
                stderr,
            })
        }
        .map(|out| out.peek(|out| log::trace!("Got output:\n{}", String::from_utf8_lossy(out))))
    }
    fn output_utf8_ok(&mut self) -> Result<String> {
        let output = self.output_ok()?;
        Ok(String::from_utf8_lossy(&output).to_string())
    }

    fn output_json<T>(&mut self) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let output = self.output_ok()?;
        Ok(serde_json::from_slice(&output)?)
    }

    fn output_from_str<T, Err>(&mut self) -> Result<T>
    where
        T: FromStr<Err = Err>,
        Err: std::fmt::Display,
    {
        let output = self.output_ok()?;
        T::from_str(std::str::from_utf8(&output).expect("Invalid UTF8"))
            .map_err(|err| Error::Parse(anyhow!("{err}")))
    }

    fn wait_ok(&mut self) -> Result<()> {
        self.output_ok()?;
        Ok(())
    }

    fn retry_until_ok(&mut self) {
        while self.wait_ok().is_err() {
            thread::sleep(Duration::from_millis(500));
        }
    }

    /// Spawn child while streaming AND collecting both stderr and stdout.
    fn spawn_streaming(&mut self) -> Result<StreamingChild> {
        log::trace!("Running command: {self:?}");
        let mut child = self.spawn(OutputType::Collect)?;
        fn stream(
            stream: impl Read,
            chan: Sender<(IsStderr, String)>,
            is_err: bool,
        ) -> anyhow::Result<()> {
            let reader = BufReader::new(stream);
            let mut iter = reader.lines();
            while let Some(Ok(line)) = iter.next() {
                chan.send((is_err, line))?;
            }
            Ok(())
        }
        let (tx, rx) = channel();

        // must be set in self::spawn
        let child_out = std::mem::take(&mut child.stdout).expect("cannot attach to child stdout");
        let child_err = std::mem::take(&mut child.stderr).expect("cannot attach to child stderr");

        let tx2 = tx.clone();
        let t1 = thread::spawn(move || {
            stream(child_out, tx, false).expect("error streaming child stdout")
        });
        let t2 = thread::spawn(move || {
            stream(child_err, tx2, true).expect("error streaming child stderr")
        });

        Ok(StreamingChild {
            rx,
            threads: vec![t1, t2],
            child,
        })
    }

    // /// Spawn child while streaming and return immediatly
    // fn spawn_streaming_cancellable(
    //     &mut self,
    //     cancel: AtomicBool,
    //     streamer: fn(String),
    // ) -> Result<()> {
    //     let mut stream = self.spawn_streaming()?;
    //     thread::spawn(|| output_ok_stream(self, stream, streamer));
    //     Ok(())
    // }

    /// Wait for child to finish while streaming stderr and stdout.
    fn output_ok_streaming(
        &mut self,
        cancel: Receiver<()>,
        streamer: fn(String),
    ) -> Result<String> {
        let stream = self.spawn_streaming()?;
        let mut stream = stream;
        let (mut stdout, mut stderr) = (String::new(), String::new());

        let mut was_canceled = false;
        // stream output until canceled
        'l: loop {
            match stream.rx.recv_timeout(Duration::from_millis(100)) {
                Ok((is_err, line)) => {
                    // remove @nix log lines
                    if !line.starts_with("@nix") {
                        let out = if is_err { &mut stderr } else { &mut stdout };
                        out.push_str(&line);
                        out.push('\n');
                    }
                    streamer(line);
                }
                Err(RecvTimeoutError::Timeout) => {
                    if cancel.try_recv().is_ok() {
                        was_canceled = true;
                        stream.child.kill()?;
                    }
                }
                _else => break 'l,
            }
        }

        for t in stream.threads {
            t.join().map_err(|err| {
                io::Error::new(
                    io::ErrorKind::Other,
                    anyhow::anyhow!("Failed streaming child stdout: {err:?}"),
                )
            })?;
        }

        let status = stream.child.wait()?;
        // dont fail if cancelled
        if was_canceled || status.success() {
            Ok(stdout)
        } else {
            log::trace!("Got error when running {self:?}:\n{stderr}");
            Err(Error::Other {
                cmd: format!("{self:?}"),
                exit_status: status,
                stderr,
            })
        }
        .map(|out| out.peek(|out| log::trace!("Got output:\n{out}")))
    }

    fn exec(&mut self) -> Result<()> {
        log::trace!("Execing command: {self:?}");
        exit(self.spawn(OutputType::Inherit)?.wait()?.code().unwrap_or(1))
    }
}

#[derive(Debug, Clone)]
pub enum OutputType {
    Inherit,
    Collect,
    Discard,
}

impl CommandExt for Command {
    fn spawn(&mut self, out_ty: OutputType) -> Result<Child> {
        self.stdout(out_ty.clone());
        self.stderr(out_ty);
        Ok(Command::spawn(self)?)
    }
}

impl From<OutputType> for Stdio {
    fn from(val: OutputType) -> Self {
        match val {
            OutputType::Inherit => Stdio::inherit(),
            OutputType::Collect => Stdio::piped(),
            OutputType::Discard => Stdio::null(),
        }
    }
}
