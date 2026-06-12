//! Jobs domain (SC3). Phase 2: delegates to the mock; the real job system —
//! the task+channel row of the concurrency taxonomy — lands in Phase 5, at which
//! point store start/recover also become `Store`-subject jobs (SC-C).

use codchi_api::dto::{JobFilter, JobView};
use codchi_api::error::ApiError;
use codchi_api::events::EventStreamOpts;
use codchi_api::ids::JobId;
use codchi_api::service::{CodchiService, EventStream};

use super::ServerCore;

impl ServerCore {
    pub(crate) async fn list_jobs(&self, filter: JobFilter) -> Result<Vec<JobView>, ApiError> {
        self.mock.list_jobs(filter).await
    }

    pub(crate) async fn get_job(&self, id: &JobId) -> Result<JobView, ApiError> {
        self.mock.get_job(id).await
    }

    pub(crate) async fn cancel_job(&self, id: &JobId) -> Result<(), ApiError> {
        self.mock.cancel_job(id).await
    }

    pub(crate) async fn stream_job_events(
        &self,
        id: &JobId,
        opts: EventStreamOpts,
    ) -> Result<EventStream, ApiError> {
        self.mock.stream_job_events(id, opts).await
    }
}
