//! C7 source-log semantics: `tail` backfill/cap, live `follow`, per-source seq,
//! and the Phase-1 rule that only `Server`/`Store` are server-owned.

use std::time::Duration;

use codchi_api::events::EventStreamOpts;
use codchi_api::{ApiError, Event, LogLevel, LogSource, MachineId};
use codchi_server::LogStore;
use futures::StreamExt;

fn message(event: &Event) -> &str {
    match event {
        Event::Log { message, .. } => message,
        other => panic!("expected a log event, got {other:?}"),
    }
}

fn seq(event: &Event) -> u64 {
    event.seq().0
}

/// `follow: false` is a bounded replay: it yields the current tail and ends, so
/// the stream can be fully collected.
async fn drain(store: &LogStore, source: LogSource, opts: EventStreamOpts) -> Vec<Event> {
    store
        .stream(source, opts)
        .expect("server-owned source")
        .map(|item| item.expect("no stream error"))
        .collect()
        .await
}

#[tokio::test]
async fn tail_backfills_in_order_with_monotonic_seq() {
    let store = LogStore::memory();
    for n in 1..=3 {
        store.logs_append(LogSource::Store, format!("line {n}"));
    }

    let events = drain(
        &store,
        LogSource::Store,
        EventStreamOpts {
            tail: None,
            follow: false,
        },
    )
    .await;

    let messages: Vec<&str> = events.iter().map(message).collect();
    assert_eq!(messages, ["line 1", "line 2", "line 3"]);
    let seqs: Vec<u64> = events.iter().map(seq).collect();
    assert_eq!(
        seqs,
        [1, 2, 3],
        "per-source seq starts at 1 and is monotonic"
    );
}

#[tokio::test]
async fn tail_caps_to_the_last_n() {
    let store = LogStore::memory();
    for n in 1..=5 {
        store.logs_append(LogSource::Store, format!("line {n}"));
    }

    let events = drain(
        &store,
        LogSource::Store,
        EventStreamOpts {
            tail: Some(2),
            follow: false,
        },
    )
    .await;

    let messages: Vec<&str> = events.iter().map(message).collect();
    assert_eq!(messages, ["line 4", "line 5"]);
}

#[tokio::test]
async fn follow_delivers_live_events_after_the_backfill() {
    let store = LogStore::memory();
    store.logs_append(LogSource::Store, "before".to_owned());

    // Open a follower, then emit; the backfill ("before") arrives first, then
    // the live event ("after") with no duplicate.
    let mut stream = store
        .stream(
            LogSource::Store,
            EventStreamOpts {
                tail: None,
                follow: true,
            },
        )
        .expect("server-owned source");

    let backfill = stream.next().await.unwrap().unwrap();
    assert_eq!(message(&backfill), "before");

    store.logs_append(LogSource::Store, "after".to_owned());

    let live = tokio::time::timeout(Duration::from_secs(1), stream.next())
        .await
        .expect("a live event within the timeout")
        .unwrap()
        .unwrap();
    assert_eq!(message(&live), "after");
    assert_eq!(seq(&live), 2, "live event keeps the monotonic seq");
}

#[tokio::test]
async fn server_and_store_have_independent_seq_spaces() {
    let store = LogStore::memory();
    store.logs_append(LogSource::Server, "s1".to_owned());
    store.logs_append(LogSource::Store, "t1".to_owned());

    let server = drain(
        &store,
        LogSource::Server,
        EventStreamOpts {
            tail: None,
            follow: false,
        },
    )
    .await;
    let store_events = drain(
        &store,
        LogSource::Store,
        EventStreamOpts {
            tail: None,
            follow: false,
        },
    )
    .await;

    assert_eq!(seq(&server[0]), 1);
    assert_eq!(seq(&store_events[0]), 1, "each source counts from 1");
}

#[tokio::test]
async fn machine_source_is_not_server_owned_in_phase_1() {
    let store = LogStore::memory();
    let result = store.stream(
        LogSource::Machine(MachineId("demo".to_owned())),
        EventStreamOpts::default(),
    );
    // Surfaces as an internal error (the router sends machine logs to the mock
    // service instead, so this branch is never hit in production).
    match result {
        Ok(_) => panic!("machine logs are not server-owned yet"),
        Err(err) => assert!(matches!(err, ApiError::Internal { .. })),
    }
}

/// Test-only convenience extension so the cases above can append a single
/// info-level `Store`/`Server` line without repeating the level/topic.
trait LogStoreTestExt {
    fn logs_append(&self, source: LogSource, message: String);
}

impl LogStoreTestExt for LogStore {
    fn logs_append(&self, source: LogSource, message: String) {
        self.append(source, LogLevel::Info, "test", message);
    }
}
