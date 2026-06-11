#![cfg(unix)]

use std::fs;
use std::os::unix::fs::PermissionsExt;

use codchi_server::{PodmanStore, Store, StorePlatformStatus};
use tempfile::tempdir;

#[test]
fn podman_store_registers_starts_and_probes() {
    let temp = tempdir().unwrap();
    let script = temp.path().join("podman");
    let state = temp.path().join("state");
    let calls = temp.path().join("calls");
    let script_body = format!(
        r#"#!/bin/sh
printf '%s\n' "$*" >> "{calls}"
case "$1 $2" in
  "container exists")
    test -f "{state}"
    ;;
  "container inspect")
    test "$(cat "{state}")" = running && printf 'true\n' || printf 'false\n'
    ;;
  "load -q")
    printf 'Loaded image: codchi-store:test\n'
    ;;
  "create --name")
    printf 'stopped\n' > "{state}"
    ;;
  "start codchi-store")
    printf 'running\n' > "{state}"
    ;;
  "exec codchi-store")
    test "$(cat "{state}")" = running
    ;;
  "stop codchi-store")
    printf 'stopped\n' > "{state}"
    ;;
  *)
    printf 'unexpected arguments: %s\n' "$*" >&2
    exit 2
    ;;
esac
"#,
        calls = calls.display(),
        state = state.display(),
    );
    fs::write(&script, script_body).unwrap();
    let mut permissions = fs::metadata(&script).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&script, permissions).unwrap();

    let image = temp.path().join("store-image.tar");
    fs::write(&image, b"image").unwrap();
    let store = PodmanStore::new(script, image);

    assert_eq!(store.status().unwrap(), StorePlatformStatus::NotInstalled);
    store.register().unwrap();
    assert_eq!(store.status().unwrap(), StorePlatformStatus::Stopped);
    store.start().unwrap();
    assert_eq!(store.status().unwrap(), StorePlatformStatus::Running);
    store.probe_health().unwrap();

    let calls = fs::read_to_string(calls).unwrap();
    assert!(calls.contains("load -q -i"));
    assert!(calls.contains("create --name codchi-store"));
    // v1 (S4/S6): only the `/nix` named volume — no host /config or /data binds.
    assert!(calls.contains("--volume codchi-store-nix:/nix"));
    assert!(!calls.contains(":/config"));
    assert!(!calls.contains(":/data"));
    assert!(calls.contains("exec codchi-store run nix store ping --store daemon"));
}
