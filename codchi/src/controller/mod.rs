use self::tray::TrayMsg;
use anyhow::{Context, Result};
use futures::StreamExt;
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

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to start the controller listener.")]
    Listener(#[from] io::Error),

    #[error("The controller is already running.")]
    AlreadyRunning,

    #[error("Failed to connect to the controller.")]
    NotRunning,
}

pub fn start(run_in_foreground: &bool) -> Result<()> {
    // We create a diffrent runtime than the static one, because the runtime seems to
    // be unusable after daemonization
    Runtime::new().unwrap().block_on(async {
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
        anyhow::Ok(())
    })?;

    // start_native should daemonize and continue with the main loop. On windows
    // this isn't as easy, so we create a detached process with --foreground and
    // exit
    utils::start_native(run_in_foreground)?;

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
                    BaseChannel::with_defaults(transport).execute(Controller { tray_chan }.serve()),
                );
            }
            Ok(())
        });
        tray::run(tray_rx).await?;
        server.await?
    })
}

pub fn stop() -> Result<()> {
    ipc::with_client_connected(|client| async move {
        client
            .quit(context::current())
            .await
            .context("While sending quit message")?;

        info!("Stopped controller.");

        Ok(())
    })
}

#[tarpc::service]
pub trait ControllerService {
    async fn quit();
    async fn ping();
}

#[derive(Clone)]
pub struct Controller {
    pub tray_chan: mpsc::Sender<TrayMsg>,
}

#[tarpc::server]
impl ControllerService for Controller {
    async fn quit(self, _: tarpc::context::Context) {
        self.tray_chan
            .send(TrayMsg::Quit)
            .await
            .expect("Tray handler disappeared...");
    }
    async fn ping(self, _: tarpc::context::Context) {
        info!("Ping");
    }
}

mod utils {
    use crate::consts::Dir;
    use anyhow::Result;
    use std::fs::File;

    #[cfg(target_family = "windows")]
    pub fn start_native(run_in_foreground: &bool) -> Result<()> {
        use crate::cli::*;
        use std::env;
        use std::os::windows::process::CommandExt;
        use std::process::{exit, Command};
        use windows::Win32::System::Threading::*;

        if !run_in_foreground {
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
            exit(0);
        }
        Ok(())
    }

    #[cfg(target_family = "unix")]
    pub fn start_native(run_in_foreground: &bool) -> Result<()> {
        use anyhow::Context;
        use daemonize::Daemonize;
        if !run_in_foreground {
            let dir = Dir::Data.get_or_create()?;
            let stdout = File::create(dir.join("ctrl.out.log"))?;
            let stderr = File::create(dir.join("ctrl.err.log"))?;
            let daemonize = Daemonize::new()
                // .pid_file(Dir::Runtime.get_or_create()?.join("codchi.pid")) // Every method except `new` and `start`
                .stdout(stdout) // Redirect stdout to `/tmp/daemon.out`.
                .stderr(stderr); // Redirect stderr to `/tmp/daemon.err`.

            daemonize.start().context("Failed daemonizing controller")?;
        }
        Ok(())
    }
}
