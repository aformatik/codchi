//! Emit the committed `openapi.json` snapshot.
//!
//! ```sh
//! cargo run -p codchi-api --bin gen-openapi > crates/codchi-api/openapi.json
//! ```
//!
//! CI regenerates and diffs this on every PR; uncommitted drift fails CI (Q5).

fn main() {
    println!("{}", codchi_api::openapi::openapi_json());
}
