//! Server-status domain — the SC5 projection-join.
//!
//! The Phase-1 `ServerStatusEp` overlay moves here verbatim (SC1): the mock
//! supplies the still-unbacked base (api/server version, `started_at`, schema —
//! Phase 3/4), and the store-condition projections overlay the real
//! `lifecycle`/`store`/`findings`/`startup_error`. Output is byte-identical to
//! Phase 1 (SC2). This is the singleton of the general
//! `durable ⋈ observed ⋈ in-flight` join that `MachineView` scales to.

use codchi_api::dto::{FindingsSummary, ServerStatus};
use codchi_api::error::ApiError;
use codchi_api::service::CodchiService;

use super::ServerCore;

impl ServerCore {
    pub(crate) async fn server_status(&self) -> Result<ServerStatus, ApiError> {
        let mut status = self.mock.server_status().await?;
        status.lifecycle = self.lifecycle();
        status.store = self.store_status();

        let summary = FindingsSummary::of(&self.store_findings());
        status.findings_summary.critical += summary.critical;
        status.findings_summary.error += summary.error;
        status.findings_summary.warning += summary.warning;
        status.findings_summary.info += summary.info;

        status.startup_error = self.startup_error();
        Ok(status)
    }
}
