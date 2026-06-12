//! Server-owned source logs (C7 / D9).
//!
//! Phase 1 owns two real log sources — `Server` (the daemon's own narrative)
//! and `Store` (the store container's output). `Machine` stays on the mock
//! service until Phase 7, so the router only routes `Server`/`Store` here.
//!
//! Each source is the same small machine:
//! - a monotonic per-source `seq` (per *run*; resume across a daemon restart is
//!   not a thing — R14 dropped `since_seq`, and the socket drops on restart);
//! - an in-memory **ring** of the most recent events, which answers `tail`
//!   backfill and seeds a follower;
//! - a `broadcast` channel fanning live events to any number of followers;
//! - an append-only **JSONL** file under [`codchi_shared::logs_dir`] — durable
//!   for offline inspection (`cat store.jsonl` after a reboot). Phase 1 does not
//!   prune it; SQLite-indexed tiering + retention are Phase 9 (D14). The
//!   `stream_logs` replay path is the ring, not this file.
//!
//! The `Server` source is fed by a [`tracing`] layer ([`ServerLogLayer`]) so the
//! existing `info!`/`warn!`/`error!` call sites feed both stderr and the log
//! with no duplicate emit. The `Store` source is fed by the store-output capture
//! task ([`capture_store_logs`]).

use std::collections::VecDeque;
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write};
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use chrono::Utc;
use codchi_api::events::DEFAULT_TAIL;
use codchi_api::{ApiError, Event, EventSeq, EventStream, EventStreamOpts, LogLevel, LogSource};
use futures::StreamExt;
use tokio::sync::broadcast;
use tracing::field::{Field, Visit};
use tracing::{Level, Subscriber};
use tracing_subscriber::layer::{Context, Layer};

/// How many recent events each source keeps for `tail` backfill and follower
/// seeding. These are low-rate lifecycle logs, not a build firehose.
const RING_CAPACITY: usize = 1024;

/// Live-follower fan-out buffer. A follower that lags past this drops the gap
/// (logged as such) rather than stalling the emitter.
const BROADCAST_CAPACITY: usize = 256;

/// One server-owned source's in-memory + on-disk log state.
struct Source {
    seq: AtomicU64,
    ring: Mutex<VecDeque<Event>>,
    tx: broadcast::Sender<Event>,
    /// `None` if the JSONL could not be opened — the source still works fully
    /// from memory, it just isn't durable this run.
    file: Mutex<Option<BufWriter<File>>>,
}

impl Source {
    fn new(file: Option<BufWriter<File>>) -> Arc<Self> {
        let (tx, _) = broadcast::channel(BROADCAST_CAPACITY);
        Arc::new(Self {
            seq: AtomicU64::new(0),
            ring: Mutex::new(VecDeque::with_capacity(RING_CAPACITY)),
            tx,
            file: Mutex::new(file),
        })
    }

    fn append(&self, level: LogLevel, topic: String, message: String) {
        let seq = EventSeq(self.seq.fetch_add(1, Ordering::Relaxed) + 1);
        let event = Event::Log {
            seq,
            ts: Utc::now(),
            level,
            topic,
            message,
        };

        {
            let mut ring = self.ring.lock().expect("log ring poisoned");
            if ring.len() == RING_CAPACITY {
                ring.pop_front();
            }
            ring.push_back(event.clone());
        }

        if let Ok(mut guard) = self.file.lock()
            && let Some(writer) = guard.as_mut()
        {
            let wrote = serde_json::to_writer(&mut *writer, &event)
                .map_err(std::io::Error::from)
                .and_then(|()| writer.write_all(b"\n"))
                .and_then(|()| writer.flush());
            if wrote.is_err() {
                // Best-effort durability: drop the writer so we stop trying,
                // memory-only from here. (Do not recurse into tracing here.)
                *guard = None;
            }
        }

        // Ignore the no-active-followers case.
        let _ = self.tx.send(event);
    }

    fn stream(&self, opts: EventStreamOpts) -> EventStream {
        let tail = opts.tail.unwrap_or(DEFAULT_TAIL) as usize;

        // Subscribe *before* snapshotting the ring so an event emitted between
        // the snapshot and the subscription can't slip through the gap; the
        // dedup below drops anything the snapshot already carried.
        let receiver = opts.follow.then(|| self.tx.subscribe());

        let snapshot: Vec<Event> = {
            let ring = self.ring.lock().expect("log ring poisoned");
            let skip = ring.len().saturating_sub(tail);
            ring.iter().skip(skip).cloned().collect()
        };
        let last_seq = snapshot.last().map(Event::seq);

        let backfill = futures::stream::iter(snapshot.into_iter().map(Ok));
        match receiver {
            None => backfill.boxed(),
            Some(receiver) => backfill.chain(follow(receiver, last_seq)).boxed(),
        }
    }
}

/// Turn a broadcast subscription into the live tail of a `stream`, skipping
/// anything already delivered by the backfill (`seq <= after`).
fn follow(
    receiver: broadcast::Receiver<Event>,
    after: Option<EventSeq>,
) -> impl futures::Stream<Item = Result<Event, ApiError>> {
    futures::stream::unfold((receiver, after), |(mut receiver, after)| async move {
        loop {
            match receiver.recv().await {
                Ok(event) => {
                    if after.is_some_and(|after| event.seq().0 <= after.0) {
                        continue;
                    }
                    return Some((Ok(event), (receiver, after)));
                }
                // Slow follower: skip the gap and keep going (R2: informational).
                Err(broadcast::error::RecvError::Lagged(_)) => continue,
                // Emitter gone (server shutting down): end the stream.
                Err(broadcast::error::RecvError::Closed) => return None,
            }
        }
    })
}

/// The server's source-keyed log store. Cheap to [`Clone`] (shared `Arc`s).
#[derive(Clone)]
pub struct LogStore {
    server: Arc<Source>,
    store: Arc<Source>,
}

impl LogStore {
    /// Build a store whose `Server`/`Store` JSONL lives under `dir` (created if
    /// absent). On any filesystem error the affected source degrades to
    /// memory-only — logging never blocks the daemon from starting.
    pub fn new(dir: &Path) -> Self {
        Self {
            server: Source::new(open_jsonl(dir, "server.jsonl")),
            store: Source::new(open_jsonl(dir, "store.jsonl")),
        }
    }

    /// A memory-only store (no JSONL), for the mock daemon and tests.
    pub fn memory() -> Self {
        Self {
            server: Source::new(None),
            store: Source::new(None),
        }
    }

    fn source(&self, source: &LogSource) -> Option<&Arc<Source>> {
        match source {
            LogSource::Server => Some(&self.server),
            LogSource::Store => Some(&self.store),
            // Machine logs are not server-owned in Phase 1 (Phase 7).
            LogSource::Machine(_) => None,
        }
    }

    /// Append a log line to a server-owned source. A non-server-owned source
    /// (`Machine`) is silently ignored — the router never sends those here.
    pub fn append(
        &self,
        source: LogSource,
        level: LogLevel,
        topic: impl Into<String>,
        message: impl Into<String>,
    ) {
        if let Some(src) = self.source(&source) {
            src.append(level, topic.into(), message.into());
        }
    }

    /// Stream a server-owned source per [`EventStreamOpts`] (`tail` backfill,
    /// then live `follow`). Errors only for a non-server-owned source.
    pub fn stream(
        &self,
        source: LogSource,
        opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError> {
        match self.source(&source) {
            Some(src) => Ok(src.stream(opts)),
            None => Err(ApiError::internal(format!(
                "log source `{source}` is not server-owned in this phase"
            ))),
        }
    }
}

fn open_jsonl(dir: &Path, name: &str) -> Option<BufWriter<File>> {
    if let Err(error) = std::fs::create_dir_all(dir) {
        eprintln!(
            "codchi-server: cannot create log dir {}: {error}; logs are memory-only",
            dir.display()
        );
        return None;
    }
    match OpenOptions::new()
        .create(true)
        .append(true)
        .open(dir.join(name))
    {
        Ok(file) => Some(BufWriter::new(file)),
        Err(error) => {
            eprintln!(
                "codchi-server: cannot open log file {}: {error}; logs are memory-only",
                dir.join(name).display()
            );
            None
        }
    }
}

/// A [`tracing`] layer that fans the daemon's own events into the `Server`
/// source log. Installed by [`crate::logging::init`] with a filter so only
/// `codchi_server` events at `INFO`+ are captured — independent of console
/// verbosity, so the durable `Server` log is stable regardless of `RUST_LOG`.
pub struct ServerLogLayer {
    logs: LogStore,
}

impl ServerLogLayer {
    pub fn new(logs: LogStore) -> Self {
        Self { logs }
    }
}

impl<S: Subscriber> Layer<S> for ServerLogLayer {
    fn on_event(&self, event: &tracing::Event<'_>, _ctx: Context<'_, S>) {
        let meta = event.metadata();
        let level = match *meta.level() {
            Level::ERROR => LogLevel::Error,
            Level::WARN => LogLevel::Warn,
            Level::INFO => LogLevel::Info,
            Level::DEBUG => LogLevel::Debug,
            Level::TRACE => LogLevel::Trace,
        };
        // Topic = the emitting module's last segment (`supervisor`, `main`,
        // …) — a coarse but useful grouping until jobs carry explicit topics.
        let topic = meta
            .target()
            .rsplit("::")
            .next()
            .unwrap_or(meta.target())
            .to_owned();

        let mut visitor = MessageVisitor::default();
        event.record(&mut visitor);
        self.logs
            .append(LogSource::Server, level, topic, visitor.finish());
    }
}

/// Flattens a tracing event's fields into one message string: the `message`
/// field verbatim, then ` key=value` for any structured fields (e.g. `%error`).
#[derive(Default)]
struct MessageVisitor {
    message: String,
    fields: String,
}

impl MessageVisitor {
    fn finish(self) -> String {
        let mut out = self.message;
        out.push_str(&self.fields);
        out
    }
}

impl Visit for MessageVisitor {
    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        use std::fmt::Write;
        if field.name() == "message" {
            let _ = write!(self.message, "{value:?}");
        } else {
            let _ = write!(self.fields, " {}={value:?}", field.name());
        }
    }
}

/// Drain the store's output stream into the `Store` source log until the store
/// process/container exits. P6: this is observe-only — the stream ending is
/// noted but the sentinel, not this task, drives lifecycle (no auto-restart).
pub async fn capture_store_logs(mut stream: crate::platform::StoreLogStream, logs: LogStore) {
    while let Some(line) = stream.next_line().await {
        let (level, topic, message) = classify_store_line(line);
        logs.append(LogSource::Store, level, topic, message);
    }
    // Lands on the Server log (and stderr) via the tracing layer.
    tracing::info!("store output stream ended");
}

/// Lightweight classification of one store-container output line — the v1
/// successor to beta's `parse_container_log`. Full structured nix-JSON parsing
/// (the `@nix {…}` internal-json protocol → typed build/progress events) is a
/// build-pipeline concern deferred to Phase 6; Phase 1 only needs readable
/// `Store` log lines with a sensible level/topic.
fn classify_store_line(line: String) -> (LogLevel, String, String) {
    if let Some(rest) = line.strip_prefix("@nix ") {
        // Keep the raw JSON as the message for now; topic marks its origin.
        return (LogLevel::Info, "nix".to_owned(), rest.to_owned());
    }
    let lower = line.to_ascii_lowercase();
    let level = if lower.contains("error:") || lower.starts_with("error") {
        LogLevel::Error
    } else if lower.contains("warning:") || lower.starts_with("warn") {
        LogLevel::Warn
    } else {
        LogLevel::Info
    };
    (level, "store".to_owned(), line)
}
