//! Machines domain (SC3). Phase 2: every method delegates to the quarantined
//! mock; real machine state arrives with SQLite in Phase 4. Each method is the
//! named seam that phase fills in.

use codchi_api::dto::{
    CloneMachineRequest, CreateMachineRequest, JobView, MachineDetail, MachineView,
    SetModulesRequest,
};
use codchi_api::error::ApiError;
use codchi_api::ids::MachineId;
use codchi_api::service::CodchiService;

use super::ServerCore;

impl ServerCore {
    pub(crate) async fn list_machines(&self) -> Result<Vec<MachineView>, ApiError> {
        self.mock.list_machines().await
    }

    pub(crate) async fn get_machine(&self, id: &MachineId) -> Result<MachineDetail, ApiError> {
        self.mock.get_machine(id).await
    }

    pub(crate) async fn create_machine(
        &self,
        req: CreateMachineRequest,
    ) -> Result<JobView<()>, ApiError> {
        self.mock.create_machine(req).await
    }

    pub(crate) async fn clone_machine(
        &self,
        source: &MachineId,
        req: CloneMachineRequest,
    ) -> Result<JobView<()>, ApiError> {
        self.mock.clone_machine(source, req).await
    }

    pub(crate) async fn delete_machine(&self, id: &MachineId) -> Result<JobView<()>, ApiError> {
        self.mock.delete_machine(id).await
    }

    pub(crate) async fn set_modules(
        &self,
        id: &MachineId,
        req: SetModulesRequest,
    ) -> Result<(), ApiError> {
        self.mock.set_modules(id, req).await
    }
}
