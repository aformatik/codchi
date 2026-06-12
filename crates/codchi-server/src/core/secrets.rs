//! Secrets domain (SC3). Phase 2: delegates to the mock; real secret storage
//! arrives with the machine/secret phases.

use codchi_api::dto::{SecretKey, SecretName};
use codchi_api::error::ApiError;
use codchi_api::ids::MachineId;
use codchi_api::service::CodchiService;

use super::ServerCore;

impl ServerCore {
    pub(crate) async fn set_secret(
        &self,
        id: &MachineId,
        key: SecretName,
        value: String,
    ) -> Result<(), ApiError> {
        self.mock.set_secret(id, key, value).await
    }

    pub(crate) async fn get_secret(
        &self,
        id: &MachineId,
        key: SecretName,
    ) -> Result<String, ApiError> {
        self.mock.get_secret(id, key).await
    }

    pub(crate) async fn list_secrets(&self, id: &MachineId) -> Result<Vec<SecretKey>, ApiError> {
        self.mock.list_secrets(id).await
    }

    pub(crate) async fn delete_secret(
        &self,
        id: &MachineId,
        key: SecretName,
    ) -> Result<(), ApiError> {
        self.mock.delete_secret(id, key).await
    }
}
