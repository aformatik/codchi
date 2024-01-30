use super::tray;
use super::ControllerServiceClient;
use crate::consts::Dir;
use crate::ctrl::{ControllerServer, ControllerService};
use crate::platform::{self, Driver};
use anyhow::bail;
use anyhow::{anyhow, Context, Result};
use futures::{Future, StreamExt};
use log::debug;
use log::trace;
use log::*;
use parity_tokio_ipc::Endpoint;
use parity_tokio_ipc::SecurityAttributes;
use std::fs;
use std::fs::File;
use std::io;
use tarpc::server::Channel;
use tarpc::{
    context, serde_transport, server::BaseChannel, tokio_serde::formats::Bincode,
    tokio_util::codec::length_delimited::LengthDelimitedCodec,
};

use once_cell::sync::Lazy;
use thiserror::Error;
use tokio::{runtime::Runtime, sync::mpsc, task::JoinHandle};

#[allow(unused_imports)]
pub use self::ipc::{connect_client, force_client};

static RUNTIME: Lazy<Runtime> =
    Lazy::new(|| Runtime::new().expect("Failed creating tokio runtime."));

#[derive(Error, Debug)]
pub enum Error {
    #[error("The controller is already running.")]
    AlreadyRunning,

    #[error("Failed to connect to the controller. Please check the error logs of the controller.")]
    ConnectionRefused,

    #[error("Failed to start the controller.")]
    Other(#[from] anyhow::Error),

    #[error("Failed to start the controller.")]
    IO(#[from] io::Error),
}

/// Starts the controller (RPC server, tray, driver container).
/// If `run_in_foreground = true`, this blocks until the server finishes (crash or user exit).
/// Otherwise the server starts in the background as a daemon and the function immediately returns.
/// In both cases preconditions are checked in the caller's process to immediately show errors.
pub fn start(run_in_foreground: bool) -> Result<(), Error> {
    // We create a diffrent runtime than the static one, because the runtime seems to
    // be unusable after daemonization
    Runtime::new()?.block_on(async {
        // we try connecting / binding before we daemonize in order to display errors
        // to the user
        if ipc::connect_client_async().await?.is_some() {
            Err(Error::AlreadyRunning)?;
        }
        drop(
            ipc::bind_server()?
                .incoming()
                .context("Failed to open new socket")?,
        );
        platform::DRIVER.init_controller()?;
        Ok::<_, Error>(())
    })?;

    if run_in_foreground || daemonize()? {
        // Run the main loop if were in foreground mode or successfully daemonized (which means we are
        // the child now)
        RUNTIME.block_on(async {
            let (tray_tx, tray_rx) = mpsc::channel(32);

            let server: JoinHandle<Result<()>> = tokio::spawn(async move {
                let incoming = ipc::bind_server()?
                    .incoming()
                    .context("Failed to open new socket")?;

                futures::pin_mut!(incoming);
                while let Some(stream) = incoming.next().await {
                    let stream = stream.context("Failed accepting connection.")?;
                    let tray_chan = tray_tx.clone();

                    let transport = serde_transport::new(
                        LengthDelimitedCodec::builder().new_framed(stream),
                        Bincode::default(),
                    );
                    tokio::spawn(
                        BaseChannel::with_defaults(transport)
                            .execute(ControllerServer { tray_chan }.serve())
                            .for_each(|response| async move {
                                tokio::spawn(response);
                            }),
                    );
                }
                Ok(())
            });
            tray::run(tray_rx).await?;
            server.await?
        })?;
    }
    Ok(())
}

pub fn stop(fail_if_not_running: bool) -> Result<()> {
    ipc::connect_client(move |client| async move {
        match client {
            None => {
                if fail_if_not_running {
                    bail!("Controller is not running.");
                } else {
                    Ok(())
                }
            }
            Some(client) => {
                client
                    .quit(context::current())
                    .await
                    .context("Failed sending quit message.")?;

                info!("Stopped controller.");

                Ok(())
            }
        }
    })
}

/// Returns true if we're now in the child process, false otherwise
#[cfg(target_family = "windows")]
pub fn daemonize() -> Result<bool> {
    use crate::cli::*;
    use crate::ctrl::ipc;
    use crate::util::UtilExt;
    use anyhow::bail;
    use spinoff::*;
    use std::env;
    use std::os::windows::process::CommandExt;
    use std::process::Command;
    use std::time::Duration;
    use windows::Win32::System::Threading::*;

    let dir = Dir::Data.get_or_create()?;
    let exe = env::current_exe()?;
    let stdout = File::create(dir.join("ctrl.out.log"))?;
    let stderr = File::create(dir.join("ctrl.err.log"))?;
    let cmd = ControllerCmd::Start {
        run_in_foreground: true,
    };
    Command::new(exe)
        .args(cmd.to_args())
        .stdout(stdout)
        .stderr(stderr)
        .creation_flags(CREATE_NEW_PROCESS_GROUP.0 | CREATE_NO_WINDOW.0)
        .spawn()?;

    let mut spinner = Spinner::new_with_stream(
        spinners::Dots,
        "Starting controller in background...",
        Color::Blue,
        spinoff::Streams::Stderr,
    );
    Runtime::new()?
        .block_on(async {
            for _ in 0..15 {
                if let Some(_) = ipc::connect_client_async().await? {
                    return Ok(());
                }
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
            bail!("Failed to start controller within 2 second.");
        })
        .finally(|| spinner.clear())?;
    // We're always the parent here
    Ok(false)
}

/// Returns true if we're now in the child process, false otherwise
#[cfg(target_family = "unix")]
pub fn daemonize() -> Result<bool> {
    use daemonize::{Daemonize, Outcome};

    let dir = Dir::Data.get_or_create()?;
    let stdout = File::create(dir.join("ctrl.out.log"))?;
    let stderr = File::create(dir.join("ctrl.err.log"))?;
    let daemonize = Daemonize::new()
        // .pid_file(Dir::Runtime.get_or_create()?.join("codchi.pid")) // Every method except `new` and `start`
        .stdout(stdout) // Redirect stdout to `/tmp/daemon.out`.
        .stderr(stderr); // Redirect stderr to `/tmp/daemon.err`.

    match daemonize.execute() {
        Outcome::Parent(Ok(_)) => Ok(false),
        Outcome::Child(Ok(_)) => Ok(true),
        Outcome::Parent(Err(err)) => Err(err),
        Outcome::Child(Err(err)) => Err(err),
    }
    .context("Failed daemonizing controller")
}

mod ipc {
    use super::*;

    pub fn get_socket_path() -> Result<String> {
        if cfg!(target_family = "unix") {
            let path = Dir::Runtime.get_or_create()?.join("codchi.sock");
            Ok(path.to_string_lossy().to_string())
        } else if cfg!(target_family = "windows") {
            Ok(r"\\.\pipe\codchi.sock".to_string())
        } else {
            bail!("Unsupported OS.")
        }
    }

    pub fn bind_server() -> Result<Endpoint> {
        let path = get_socket_path()?;
        if fs::metadata(&path).is_ok() {
            drop(fs::remove_file(&path)); // ignore result
        }
        let mut endpoint = Endpoint::new(path);
        endpoint.set_security_attributes(SecurityAttributes::allow_everyone_create()?);
        Ok(endpoint)
    }

    pub async fn connect_client_async() -> Result<Option<ControllerServiceClient>> {
        let path = get_socket_path()?;
        if fs::metadata(&path).is_err() {
            Ok(None)
        } else {
            match Endpoint::connect(&path).await {
                Ok(con) => {
                    let transport = serde_transport::new(
                        LengthDelimitedCodec::builder().new_framed(con),
                        Bincode::default(),
                    );
                    Ok(Some(
                        ControllerServiceClient::new(Default::default(), transport).spawn(),
                    ))
                }
                Err(e) if e.kind() == io::ErrorKind::ConnectionRefused => Ok(None),
                Err(e) => Err(anyhow!("Failed to connect to controller. {e:?}")),
            }
        }
    }

    /// Connect to controller if its running
    pub fn connect_client<Fut, F, Res>(f: F) -> Result<Res>
    where
        Res: Send + 'static,
        F: FnOnce(Option<ControllerServiceClient>) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Res>> + Send + 'static,
    {
        RUNTIME.block_on(async {
            let client = connect_client_async().await?;
            f(client).await
        })
    }

    /// Starts server if not running and returns RPC client
    pub fn force_client<Fut, F, Res>(f: F) -> Result<Res>
    where
        Res: Send + 'static,
        F: (FnOnce(ControllerServiceClient) -> Fut) + Send + Sync + 'static,
        Fut: Future<Output = Result<Res>> + Send + 'static,
    {
        start_if_not_running()?;
        connect_client(move |client| async {
            let client = client.ok_or(Error::ConnectionRefused)?;
            f(client).await
        })
    }

    /// Starts the controller in the background if not already running.
    /// Returns true if controller was actually started by the caller.
    pub fn start_if_not_running() -> Result<bool> {
        if let Err(Error::AlreadyRunning) = start(false) {
            trace!("Controller is already running.");
            Ok(false)
        } else {
            debug!("Started controller in background.");
            Ok(true)
        }
    }
}
