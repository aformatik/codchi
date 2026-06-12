//! Migration domain (SC3). Phase 2: delegates to the mock; the real beta→v1
//! migration lands in Phase 11.

use codchi_api::dto::{JobView, MigrationOpts, MigrationPlan, MigrationSummary};
use codchi_api::error::ApiError;
use codchi_api::service::CodchiService;

use super::ServerCore;

impl ServerCore {
    pub(crate) async fn migration_plan(&self) -> Result<MigrationPlan, ApiError> {
        self.mock.migration_plan().await
    }

    pub(crate) async fn migration_run(
        &self,
        opts: MigrationOpts,
    ) -> Result<JobView<MigrationSummary>, ApiError> {
        self.mock.migration_run(opts).await
    }
}
