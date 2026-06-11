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
