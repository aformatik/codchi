use super::{ControllerServiceClient, RUNTIME};
use crate::consts::Dir;
use anyhow::{anyhow, Result};
use futures::Future;
use parity_tokio_ipc::Endpoint;
use parity_tokio_ipc::SecurityAttributes;
use std::fs;
use std::io;
use tarpc::{
    serde_transport, tokio_serde::formats::Bincode,
    tokio_util::codec::length_delimited::LengthDelimitedCodec,
};

pub fn get_socket_path() -> Result<String> {
    if cfg!(target_family = "unix") {
        let path = Dir::Runtime.get_or_create()?.join("codchi.sock");
        Ok(path.to_string_lossy().to_string())
    } else if cfg!(target_family = "windows") {
        Ok(r"\\.\pipe\codchi.sock".to_string())
    } else {
        Err(anyhow!("Unsupported OS."))
    }
}

pub fn bind_server() -> Result<Endpoint> {
    let path = get_socket_path()?;
    if fs::metadata(&path).is_ok() {
        drop(fs::remove_file(path.clone())); // ignore result
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

pub fn with_client<Fut, Res>(f: impl FnOnce(Option<ControllerServiceClient>) -> Fut) -> Result<Res>
where
    Res: Send + 'static,
    Fut: Future<Output = Result<Res>> + Send + 'static,
{
    RUNTIME.block_on(async {
        let client = connect_client_async().await?;
        f(client).await
    })
}

pub fn with_client_connected<Fut, Res>(
    f: impl FnOnce(ControllerServiceClient) -> Fut,
) -> Result<Res>
where
    Res: Send + 'static,
    Fut: Future<Output = Result<Res>> + Send + 'static,
{
    RUNTIME.block_on(async {
        let client = connect_client_async()
            .await?
            .ok_or(super::Error::NotRunning)?;
        f(client).await
    })
}
