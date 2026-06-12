//! Doctor domain — read/scan/fix, with the store-finding overlay (SC1).
//!
//! The Phase-1 `DoctorEp` overlay moves here: the mock report is extended with
//! the projected store findings and stamped `generated_at`. `doctor_scan`/
//! `doctor_fix` delegate to the mock until the recovery-findings phase
//! (SC-B, Phase 10).

use chrono::Utc;
use codchi_api::dto::{DoctorOpts, DoctorReport, JobView};
use codchi_api::error::ApiError;
use codchi_api::ids::FindingId;
use codchi_api::service::CodchiService;

use super::ServerCore;

impl ServerCore {
    pub(crate) async fn doctor(&self, opts: DoctorOpts) -> Result<DoctorReport, ApiError> {
        let mut report = self.mock.doctor(opts).await?;
        report.findings.extend(self.store_findings());
        report.generated_at = Utc::now();
        Ok(report)
    }

    pub(crate) async fn doctor_scan(
        &self,
        opts: DoctorOpts,
    ) -> Result<JobView<DoctorReport>, ApiError> {
        self.mock.doctor_scan(opts).await
    }

    pub(crate) async fn doctor_fix(&self, finding: FindingId) -> Result<JobView<()>, ApiError> {
        self.mock.doctor_fix(finding).await
    }
}
