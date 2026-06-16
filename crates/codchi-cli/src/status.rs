//! The `codchi status` command (C5 / D7).
//!
//! Reads the daemon's `ServerStatus` (lifecycle + store state, overlaid with the
//! real lifecycle server-side per D7) and the machine list, then renders them as
//! human text or JSON. Read-only: no probing, just the snapshot the daemon
//! already holds (P6).

use codchi_api::dto::{
    ConfigurationStatus, Lifecycle, MachineView, ServerLifecycle, ServerStatus, StoreState,
};
use codchi_api::{ApiError, CodchiService};

use crate::client::HttpClient;

/// Fetch and render daemon + store + machine status.
pub async fn run(client: &HttpClient, json: bool) -> Result<(), ApiError> {
    let status = client.server_status().await?;
    let machines = client.list_machines().await?;

    if json {
        let out = serde_json::json!({ "server": status, "machines": machines });
        let text = serde_json::to_string_pretty(&out)
            .map_err(|e| ApiError::internal(format!("could not encode status JSON: {e}")))?;
        println!("{text}");
    } else {
        print_text(&status, &machines);
    }
    Ok(())
}

fn print_text(status: &ServerStatus, machines: &[MachineView]) {
    println!(
        "codchi-server   {:<10} (api v{} · server {})",
        lifecycle_str(status.lifecycle),
        status.api_version,
        status.server_version,
    );
    println!("store           {}", store_str(status.store.state));
    if let Some(err) = &status.startup_error {
        println!("startup error   {err}");
    }
    if let Some(reason) = &status.store.last_error {
        println!("store error     {reason}");
    }

    if machines.is_empty() {
        println!("machines        none");
        return;
    }
    println!("machines ({})", machines.len());
    let width = machines
        .iter()
        .map(|m| m.id.0.len())
        .max()
        .unwrap_or(0)
        .max(4);
    for m in machines {
        let busy = if m.busy_with.is_some() {
            "  (busy)"
        } else {
            ""
        };
        println!(
            "  {:<width$}  {:<8} {}{busy}",
            m.id.0,
            run_str(m.lifecycle),
            config_str(m.configuration_status),
            width = width,
        );
    }
}

fn lifecycle_str(lifecycle: ServerLifecycle) -> &'static str {
    match lifecycle {
        ServerLifecycle::Starting => "starting",
        ServerLifecycle::Healthcheck => "healthcheck",
        ServerLifecycle::Ready => "ready",
        ServerLifecycle::Degraded => "degraded",
        ServerLifecycle::Stopping => "stopping",
    }
}

fn store_str(state: StoreState) -> &'static str {
    match state {
        StoreState::Up => "up",
        StoreState::Down => "down",
        StoreState::Recovering => "recovering",
        StoreState::Unknown => "unknown",
    }
}

fn run_str(lifecycle: Lifecycle) -> &'static str {
    match lifecycle {
        Lifecycle::Creating => "creating",
        Lifecycle::Reconciling => "reconciling",
        Lifecycle::Absent => "absent",
        Lifecycle::Stopped => "stopped",
        Lifecycle::Running => "running",
    }
}

fn config_str(status: ConfigurationStatus) -> &'static str {
    match status {
        ConfigurationStatus::Unbuilt => "unbuilt",
        ConfigurationStatus::Applied => "applied",
        ConfigurationStatus::NeedsRebuild => "needs-rebuild",
    }
}
