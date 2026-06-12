//! Build / update / activation domain and config resolution (SC3). Phase 2:
//! delegates to the mock; real generations land with the build-model phase
//! (Phase 6), backed by jobs (Phase 5).

use codchi_api::dto::{
    ConfigResolution, GenerationView, JobView, Rebuilt, ResolveConfigRequest, Updated,
};
use codchi_api::error::ApiError;
use codchi_api::ids::{GenerationId, MachineId};
use codchi_api::service::CodchiService;

use super::ServerCore;

impl ServerCore {
    pub(crate) async fn rebuild(&self, id: &MachineId) -> Result<JobView<Rebuilt>, ApiError> {
        self.mock.rebuild(id).await
    }

    pub(crate) async fn update(&self, id: &MachineId) -> Result<JobView<Updated>, ApiError> {
        self.mock.update(id).await
    }

    pub(crate) async fn list_generations(
        &self,
        id: &MachineId,
    ) -> Result<Vec<GenerationView>, ApiError> {
        self.mock.list_generations(id).await
    }

    pub(crate) async fn activate_generation(
        &self,
        id: &MachineId,
        generation: GenerationId,
    ) -> Result<JobView<()>, ApiError> {
        self.mock.activate_generation(id, generation).await
    }

    pub(crate) async fn resolve_config(
        &self,
        req: ResolveConfigRequest,
    ) -> Result<JobView<ConfigResolution>, ApiError> {
        self.mock.resolve_config(req).await
    }
}
