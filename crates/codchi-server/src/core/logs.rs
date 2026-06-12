//! Logs domain — the `Server`/`Store` vs `Machine` source branch (SC1).
//!
//! The Phase-1 `StreamLogsEp` hand-branch moves here: `Server`/`Store` are real
//! server-owned sources backed by the [`LogStore`](crate::logs::LogStore);
//! `Machine` stays on the mock until Phase 7. The router handler is now a
//! one-liner.

use codchi_api::dto::LogSource;
use codchi_api::error::ApiError;
use codchi_api::events::EventStreamOpts;
use codchi_api::service::{CodchiService, EventStream};

use super::ServerCore;

impl ServerCore {
    pub(crate) async fn stream_logs(
        &self,
        source: LogSource,
        opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError> {
        if matches!(source, LogSource::Machine(_)) {
            self.mock.stream_logs(source, opts).await
        } else {
            self.logs.stream(source, opts)
        }
    }
}
