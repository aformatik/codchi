use crate::platform::{self, Driver};

use self::tray::TrayMsg;
use anyhow::{Context, Result};
use futures::{Future, StreamExt};
use log::*;
use std::io;
use tarpc::{
    context, serde_transport,
    server::{BaseChannel, Channel},
    tokio_serde::formats::Bincode,
    tokio_util::codec::length_delimited::LengthDelimitedCodec,
};
use thiserror::Error;
use tokio::{runtime::Runtime, sync::mpsc, task::JoinHandle};

mod ipc;
mod tray;

lazy_static::lazy_static! {
    pub static ref RUNTIME: Runtime = Runtime::new().expect("Failed creating tokio runtime.");
}

#[tarpc::service]
pub trait ControllerService {
    async fn quit();
    async fn get_status() -> String;
}

#[derive(Clone)]
pub struct ControllerServer {
    pub tray_chan: mpsc::Sender<TrayMsg>,
}

#[tarpc::server]
impl ControllerService for ControllerServer {
    async fn quit(self, _: tarpc::context::Context) {
        self.tray_chan
            .send(TrayMsg::Quit)
            .await
            .expect("Tray handler disappeared...");
    }

    async fn get_status(self, _: tarpc::context::Context) -> String {
        "Hi".to_string()
    }
}

/// Starts server if not running and returns RPC client
pub fn force_client<Fut, F, Res>(f: F) -> Result<Res>
where
    Res: Send + 'static,
    F: (FnOnce(ControllerServiceClient) -> Fut) + Send + Sync + 'static,
    Fut: Future<Output = Result<Res>> + Send + 'static,
{
    start_if_not_running()?;
    ipc::connect_client(move |client| async {
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

    if run_in_foreground || utils::daemonize()? {
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
                            .execute(ControllerServer { tray_chan }.serve()),
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

pub fn stop() -> Result<()> {
    ipc::connect_client(|client| async move {
        match client {
            None => Err(anyhow::anyhow!("Controller is not running.")),
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

mod utils {
    use crate::consts::Dir;
    use anyhow::Result;
    use std::fs::File;

    /// Returns true if we're now in the child process, false otherwise
    #[cfg(target_family = "windows")]
    pub fn daemonize() -> Result<bool> {
        use crate::cli::*;
        use std::env;
        use std::os::windows::process::CommandExt;
        use std::process::{exit, Command};
        use windows::Win32::System::Threading::*;

        let dir = Dir::Data.get_or_create()?;
        let exe = env::current_exe()?;
        let stdout = File::create(dir.join("ctrl.out.log"))?;
        let stderr = File::create(dir.join("ctrl.err.log"))?;
        let cmd = Cmd::Controller(ControllerCmd::Start {
            run_in_foreground: true,
        });
        Command::new(exe)
            .args(cmd.to_args())
            .stdout(stdout)
            .stderr(stderr)
            .creation_flags(CREATE_NEW_PROCESS_GROUP.0 | CREATE_NO_WINDOW.0)
            .spawn()?;

        // We're always the parent here
        Ok(false)
    }

    /// Returns true if we're now in the child process, false otherwise
    #[cfg(target_family = "unix")]
    pub fn daemonize() -> Result<bool> {
        use anyhow::Context;
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
}
