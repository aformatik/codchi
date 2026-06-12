//! Narrow platform-independent store driver boundary.

use codchi_shared::CommandError;
use tokio::io::{AsyncBufReadExt, BufReader, Lines};
use tokio::process::{Child, ChildStderr, ChildStdout};

/// Store state as observed from the host platform.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum StorePlatformStatus {
    #[default]
    NotInstalled,
    Stopped,
    Running,
}

/// A store-driver failure suitable for lifecycle and health reporting.
///
/// Two shapes only: a `Message` for protocol/logic problems the driver detects
/// itself (an impossible state string, a missing image name), and `Command` for
/// any subprocess failure, which the shared [`CommandExt`](codchi_shared::CommandExt)
/// already models in full (spawn, non-zero exit with captured output, decode).
#[derive(Debug, thiserror::Error)]
pub enum StoreError {
    #[error("{0}")]
    Message(String),
    #[error(transparent)]
    Command(#[from] CommandError),
}

impl StoreError {
    pub fn new(message: impl Into<String>) -> Self {
        Self::Message(message.into())
    }
}

/// Platform operations needed to own the store lifecycle.
///
/// Most methods are synchronous because they wrap one-shot host subprocesses;
/// the [`StoreSupervisor`](crate::StoreSupervisor) runs them on Tokio's blocking pool.
/// [`attach`](Store::attach) is the exception: it opens a *long-lived* follower
/// over the store's output and is consumed asynchronously.
pub trait Store: Send + Sync + 'static {
    fn status(&self) -> Result<StorePlatformStatus, StoreError>;
    fn register(&self) -> Result<(), StoreError>;
    fn start(&self) -> Result<(), StoreError>;
    fn probe_health(&self) -> Result<(), StoreError>;
    fn stop(&self) -> Result<(), StoreError>;

    /// Attach to the running store and stream its combined stdout/stderr, line
    /// by line ([`StoreLogStream`]). Called once, after a successful start, to
    /// feed the `Store` source log (C7).
    ///
    /// The returned handle is a **supervision** handle, not merely a log tap:
    /// dropping it detaches. For rootless Podman that is harmless (the detached
    /// container keeps running; [`stop`](Store::stop) is what stops it); for the
    /// future WSL backend the same handle *is* the store process, so dropping it
    /// terminates the store. The stream ends when the store process/container
    /// exits — observe-only (P6): the sentinel, not this stream, owns lifecycle.
    fn attach(&self) -> Result<StoreLogStream, StoreError>;
}

/// A live, line-buffered capture of the running store's output.
///
/// [`StoreLogStream::Process`] owns the follower child (`kill_on_drop`), so the
/// drop semantics above hold. [`StoreLogStream::Empty`] is an immediately-ended
/// stream for drivers/tests with no process to follow.
pub enum StoreLogStream {
    Process(Box<ProcessLogStream>),
    Empty,
}

impl StoreLogStream {
    /// Wrap a spawned follower child, taking ownership of both pipes.
    pub fn from_child(mut child: Child) -> Result<Self, StoreError> {
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| StoreError::new("store follower child has no stdout pipe"))?;
        let stderr = child
            .stderr
            .take()
            .ok_or_else(|| StoreError::new("store follower child has no stderr pipe"))?;
        Ok(StoreLogStream::Process(Box::new(ProcessLogStream {
            _child: child,
            stdout: BufReader::new(stdout).lines(),
            stderr: BufReader::new(stderr).lines(),
            stdout_done: false,
            stderr_done: false,
        })))
    }

    /// The next output line, or `None` once the store's output is exhausted.
    pub async fn next_line(&mut self) -> Option<String> {
        match self {
            StoreLogStream::Process(process) => process.next_line().await,
            StoreLogStream::Empty => None,
        }
    }
}

/// A [`StoreLogStream`] backed by a child process whose stdout and stderr are
/// merged, in arrival order, into one line stream.
pub struct ProcessLogStream {
    // Held only to keep the follower alive and `kill_on_drop` it; never read
    // directly — its pipes were moved into the line readers below.
    _child: Child,
    stdout: Lines<BufReader<ChildStdout>>,
    stderr: Lines<BufReader<ChildStderr>>,
    stdout_done: bool,
    stderr_done: bool,
}

impl ProcessLogStream {
    async fn next_line(&mut self) -> Option<String> {
        loop {
            if self.stdout_done && self.stderr_done {
                return None;
            }
            tokio::select! {
                line = self.stdout.next_line(), if !self.stdout_done => match line {
                    Ok(Some(line)) => return Some(line),
                    // EOF or read error: that pipe is finished, keep the other.
                    _ => self.stdout_done = true,
                },
                line = self.stderr.next_line(), if !self.stderr_done => match line {
                    Ok(Some(line)) => return Some(line),
                    _ => self.stderr_done = true,
                },
            }
        }
    }
}
