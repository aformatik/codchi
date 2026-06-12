//! Store domain — the reader half of the condition (SC7) and store generations.
//!
//! The supervisor is the sole *writer* of the [`StoreCondition`] (SC6); this
//! module only borrows the `watch::Receiver` and **projects** it on read, so the
//! SC4 layer boundary holds. The projections are the pure functions in
//! [`super::condition`]; these methods just feed them the current condition and
//! the shutdown flag.

use codchi_api::dto::{Finding, ServerLifecycle, StoreGenerationView, StoreStatus};
use codchi_api::error::ApiError;
use codchi_api::service::CodchiService;

use super::ServerCore;
use super::condition::{self, StoreCondition};

impl ServerCore {
    /// A snapshot of the current store condition.
    pub(crate) fn store_condition(&self) -> StoreCondition {
        self.condition.borrow().clone()
    }

    /// Project the headline lifecycle (SC5/SC8): `Stopping` when shutting down,
    /// otherwise the store-condition projection.
    pub(crate) fn lifecycle(&self) -> ServerLifecycle {
        condition::lifecycle(self.shutdown.is_cancelled(), &self.store_condition())
    }

    /// Project the wire [`StoreStatus`].
    pub(crate) fn store_status(&self) -> StoreStatus {
        condition::store_status(&self.store_condition())
    }

    /// Project the active findings (just `store.unavailable` in Phase 2).
    pub(crate) fn store_findings(&self) -> Vec<Finding> {
        condition::store_findings(&self.store_condition())
    }

    /// Project the recoverable startup error, if any.
    pub(crate) fn startup_error(&self) -> Option<ApiError> {
        condition::startup_error(&self.store_condition())
    }

    pub(crate) async fn list_store_generations(
        &self,
    ) -> Result<Vec<StoreGenerationView>, ApiError> {
        self.mock.list_store_generations().await
    }
}
