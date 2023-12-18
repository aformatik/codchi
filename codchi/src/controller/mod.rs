use self::{
    ipc::{ServerAnswer, ServerMsg},
    tray::TrayMsg,
};
use crate::{
    cli::{Cli, ControllerCmd},
    consts::Dir,
};
use anyhow::{anyhow, Context, Result};
use futures::{Future, SinkExt, StreamExt, TryStreamExt};
use log::*;
use parity_tokio_ipc::{Connection, Endpoint, SecurityAttributes};
use serde::{Deserialize, Serialize};
use std::{
    fs::{self, File},
    io::{self},
};
use thiserror::Error;
use tokio::{
    io::{split, AsyncRead, AsyncWrite, ReadHalf, WriteHalf},
    runtime::Runtime,
    sync::mpsc,
    task::JoinHandle,
};
use tokio_serde::{formats::SymmetricalMessagePack, SymmetricallyFramed};
use tokio_util::codec::{FramedRead, FramedWrite, LengthDelimitedCodec};

mod ipc;
mod tray;

lazy_static::lazy_static! {
    pub static ref RUNTIME: Runtime = Runtime::new().expect("Failed creating tokio runtime.");
}

type MPRead<R, Stream> = SymmetricallyFramed<
    FramedRead<ReadHalf<Stream>, LengthDelimitedCodec>,
    R,
    SymmetricalMessagePack<R>,
>;
type MPWrite<S, Stream> = SymmetricallyFramed<
    FramedWrite<WriteHalf<Stream>, LengthDelimitedCodec>,
    S,
    SymmetricalMessagePack<S>,
>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to start the controller listener.")]
    Listener(#[from] io::Error),

    #[error("The controller is already running.")]
    AlreadyRunning,

    #[error("Failed to connect to the controller.")]
    NotRunning,
}

struct Controller {}
impl Controller {
    fn get_socket_path() -> Result<String> {
        if cfg!(target_family = "unix") {
            let path = Dir::Runtime.get_or_create()?.join("codchi.sock");
            Ok(path.to_string_lossy().to_string())
        } else if cfg!(target_family = "windows") {
            Ok(r"\\.\pipe\codchi.sock".to_string())
        } else {
            Err(anyhow!("Unsupported OS."))
        }
    }

    pub async fn connect_client_async() -> Result<Option<Connection>> {
        let path = Self::get_socket_path()?;
        if fs::metadata(&path).is_err() {
            Ok(None)
        } else {
            match Endpoint::connect(&path).await {
                Ok(con) => Ok(Some(con)),
                Err(e) if e.kind() == io::ErrorKind::ConnectionRefused => Ok(None),
                Err(e) => Err(anyhow!("Failed to connect to controller. {e:?}")),
            }
        }
    }
    pub fn with_client<Fut, Res>(f: impl FnOnce(Option<Connection>) -> Fut) -> Result<Res>
    where
        Res: Send + 'static,
        Fut: Future<Output = Result<Res>> + Send + 'static,
    {
        RUNTIME.block_on(async {
            let con = Self::connect_client_async().await?;

            f(con).await
        })
    }

    fn bind_server() -> Result<Endpoint> {
        let path = Self::get_socket_path()?;
        if fs::metadata(&path).is_ok() {
            drop(fs::remove_file(path.clone())); // ignore result
        }
        let mut endpoint = Endpoint::new(path);
        endpoint.set_security_attributes(SecurityAttributes::allow_everyone_create()?);
        Ok(endpoint)
    }

    fn split_framed<R, S, Stream>(stream: Stream) -> (MPRead<R, Stream>, MPWrite<S, Stream>)
    where
        Stream: AsyncRead + AsyncWrite,
        R: for<'de> Deserialize<'de>,
        S: Serialize,
    {
        let (reader, writer) = split(stream);
        let deserializer = SymmetricallyFramed::new(
            FramedRead::new(reader, LengthDelimitedCodec::new()),
            SymmetricalMessagePack::<R>::default(),
        );
        let serializer = tokio_serde::SymmetricallyFramed::new(
            FramedWrite::new(writer, LengthDelimitedCodec::new()),
            SymmetricalMessagePack::<S>::default(),
        );
        (deserializer, serializer)
    }

    async fn handle_client_msg<Stream>(
        tray_chan: &mpsc::Sender<TrayMsg>,
        msg: &ServerMsg,
        en: &mut MPWrite<ServerAnswer, Stream>,
    ) -> Result<()>
    where
        Stream: AsyncWrite,
    {
        match msg {
            ServerMsg::Ping => {
                if let Err(e) = en.send(ServerAnswer::Pong).await {
                    warn!("Error while sending server answer: {e}")
                }
                Ok(())
            }
            ServerMsg::Quit => tray_chan
                .send(TrayMsg::Quit)
                .await
                .context("Tray handler disappeared..."),
        }
    }
}

impl Cli {
    pub fn controller(&self, ctrl_args: &ControllerCmd) -> Result<()> {
        match ctrl_args {
            ControllerCmd::Stop {} => Controller::with_client(|con| async {
                let con = con.ok_or(Error::NotRunning)?;

                let (_, mut en) = Controller::split_framed::<ServerAnswer, ServerMsg, _>(con);

                en.send(ServerMsg::Quit)
                    .await
                    .context("While sending quit message")?;

                info!("Stopped controller.");

                Ok(())
            }),
            ControllerCmd::Start { run_in_foreground } => {
                // We create a diffrent runtime than the static one, because the runtime seems to
                // be unusable after daemonization
                Runtime::new().unwrap().block_on(async {
                    // we try connecting / binding before we daemonize in order to display errors
                    // to the user
                    if Controller::connect_client_async().await?.is_some() {
                        Err(Error::AlreadyRunning)?;
                    }
                    drop(
                        Controller::bind_server()?
                            .incoming()
                            .context("Failed to open new socket")?,
                    );
                    anyhow::Ok(())
                })?;

                // start_native should daemonize and continue with the main loop. On windows
                // this isn't as easy, so we create a detached process with --foreground and
                // exit
                self.start_native(run_in_foreground)?;

                RUNTIME.block_on(async {
                    let (tray_tx, tray_rx) = mpsc::channel(32);

                    let server: JoinHandle<Result<()>> = tokio::spawn(async move {
                        let incoming = Controller::bind_server()?
                            .incoming()
                            .context("Failed to open new socket")?;

                        futures::pin_mut!(incoming);
                        while let Some(stream) = incoming.next().await {
                            let stream = stream.context("Failed accepting connection.")?;
                            let (mut de, mut en) = Controller::split_framed(stream);
                            let tray_chan = tray_tx.clone();

                            tokio::spawn(async move {
                                loop {
                                    match de.try_next().await {
                                        Ok(Some(msg)) => {
                                            if let Err(e) = Controller::handle_client_msg(
                                                &tray_chan, &msg, &mut en,
                                            )
                                            .await
                                            {
                                                warn!("Error while handling client message: {e}");
                                            }
                                        }
                                        Ok(None) => {
                                            debug!("Client disconnected.");
                                            break;
                                        }
                                        Err(e) => {
                                            warn!("Error while receiving client message: {e}");
                                            break;
                                        }
                                    }
                                }
                            });
                        }
                        Ok(())
                    });
                    tray::run(tray_rx).await?;
                    server.await?
                })
            }
        }
    }

    #[cfg(target_family = "windows")]
    fn start_native(&self, run_in_foreground: &bool) -> Result<()> {
        use crate::cli::*;
        use std::env;
        use std::os::windows::process::CommandExt;
        use std::process::{exit, Command};
        use windows::Win32::System::Threading::*;

        if !run_in_foreground {
            let dir = Dir::Data.get_or_create()?;
            let exe = env::current_exe()?;
            let stdout = File::create(dir.clone().join("ctrl.out.log"))?;
            let stderr = File::create(dir.clone().join("ctrl.err.log"))?;
            let cli = Cli {
                verbose: self.verbose.clone(),
                command: Cmd::Controller(ControllerCmd::Start {
                    run_in_foreground: true,
                }),
            };
            Command::new(exe)
                .args(cli.to_args())
                .stdout(stdout)
                .stderr(stderr)
                .creation_flags(CREATE_NEW_PROCESS_GROUP.0 | CREATE_NO_WINDOW.0)
                .spawn()?;
            exit(0);
        }
        Ok(())
    }

    #[cfg(target_family = "unix")]
    fn start_native(&self, run_in_foreground: &bool) -> Result<()> {
        use daemonize::Daemonize;
        if !run_in_foreground {
            let dir = Dir::Data.get_or_create()?;
            let stdout = File::create(dir.clone().join("ctrl.out.log"))?;
            let stderr = File::create(dir.clone().join("ctrl.err.log"))?;
            let daemonize = Daemonize::new()
                // .pid_file(Dir::Runtime.get_or_create()?.join("codchi.pid")) // Every method except `new` and `start`
                .stdout(stdout) // Redirect stdout to `/tmp/daemon.out`.
                .stderr(stderr); // Redirect stderr to `/tmp/daemon.err`.

            daemonize.start().context("Failed daemonizing controller")?;
        }
        Ok(())
    }
}
