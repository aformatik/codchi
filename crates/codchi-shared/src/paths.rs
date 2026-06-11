//! Host-side filesystem paths shared by the server and its clients.
//!
//! Phase 1 (D6) serves the HTTP/JSON API over a per-user Unix domain socket. The
//! server binds it and clients dial it, so its location must be agreed in one
//! place — here. The default lives under `$XDG_RUNTIME_DIR/codchi/` (a per-user,
//! tmpfs-backed directory with `0700` perms), falling back to the system temp
//! directory on platforms/sessions without `XDG_RUNTIME_DIR`.

use std::path::PathBuf;

/// Environment override for the full socket path. Used by tests (each gets an
/// isolated socket) and by future deployment knobs; takes precedence over the
/// computed default.
const SOCKET_ENV: &str = "CODCHI_SOCKET";

/// Environment override for the persistent data directory. Used by tests (each
/// gets an isolated tree) and deployment knobs; takes precedence over the XDG
/// default.
const DATA_DIR_ENV: &str = "CODCHI_DATA_DIR";

/// Fixed v1 Podman resource names. Machine resources use their own
/// `codchi-machine-*` namespace at the platform boundary.
pub const STORE_CONTAINER_NAME: &str = "codchi-store";
pub const STORE_NIX_VOLUME_NAME: &str = "codchi-store-nix";

/// The store's shared Nix store, the one Podman named volume that must survive
/// container recreation (`v1/phases/01-podman-store.md` S4). Must match
/// `nix/container/consts.nix`.
pub const STORE_NIX_DIR: &str = "/nix";

/// The per-user Codchi runtime directory (`$XDG_RUNTIME_DIR/codchi`, else
/// `<tmp>/codchi`). The server is responsible for creating it.
pub fn runtime_dir() -> PathBuf {
    std::env::var_os("XDG_RUNTIME_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(std::env::temp_dir)
        .join("codchi")
}

/// The per-user `codchi-server` socket path. Honors the `CODCHI_SOCKET`
/// override (an absolute path) before falling back to
/// [`runtime_dir`]`/server.sock`.
pub fn server_socket_path() -> PathBuf {
    if let Some(path) = std::env::var_os(SOCKET_ENV) {
        return PathBuf::from(path);
    }
    runtime_dir().join("server.sock")
}

/// The per-user persistent data directory (`$XDG_DATA_HOME/codchi`, else
/// `~/.local/share/codchi`), honoring the `CODCHI_DATA_DIR` override. Unlike
/// [`runtime_dir`] this survives reboot, so it holds state the daemon wants to
/// keep across restarts — the source-log JSONL (C7) being the first user.
pub fn data_dir() -> PathBuf {
    if let Some(path) = std::env::var_os(DATA_DIR_ENV) {
        return PathBuf::from(path);
    }
    std::env::var_os("XDG_DATA_HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".local/share")))
        .unwrap_or_else(std::env::temp_dir)
        .join("codchi")
}

/// Where the durable source-log JSONL lives ([`data_dir`]`/logs`). The server
/// creates it on demand. Phase 1 appends here without pruning; SQLite-indexed
/// tiering and retention land in Phase 9 (D14).
pub fn logs_dir() -> PathBuf {
    data_dir().join("logs")
}
