//! Exec domain (R7, SC3). Phase 2: delegates to the mock; the real exec/session
//! model lands in Phase 8.

use codchi_api::dto::{ExecPlan, JobView, PrepareExecRequest};
use codchi_api::error::ApiError;
use codchi_api::ids::MachineId;
use codchi_api::service::CodchiService;

use super::ServerCore;

impl ServerCore {
    pub(crate) async fn prepare_exec(
        &self,
        id: &MachineId,
        req: PrepareExecRequest,
    ) -> Result<JobView<ExecPlan>, ApiError> {
        self.mock.prepare_exec(id, req).await
    }
}
