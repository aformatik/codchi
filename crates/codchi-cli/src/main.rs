//! `codchi` binary entry point.
//!
//! Phase 1 (C4) ships the typed [`HttpClient`](codchi_cli::HttpClient) but not
//! yet the command surface: client-initiated daemon spawn and `codchi status`
//! are C5. For now the binary reports where it would dial so the wiring is
//! observable end to end.

use codchi_cli::HttpClient;

fn main() {
    let client = HttpClient::connect_default();
    eprintln!(
        "codchi {} — would dial codchi-server at {} (commands land in Phase 1 C5)",
        codchi_api::API_VERSION,
        client.socket().display(),
    );
}
