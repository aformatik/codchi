# Testing

This is not a general testing policy. These are the highest-impact tests with
acceptable effort for v1.

## 1. Windows WSL Upgrade Test

The most important test is a real Windows WSL upgrade test from the previous
Codchi release to the candidate build.

Flow:

1. install the previous Codchi release
2. create the store
3. create one base machine
4. write a sentinel file inside the machine
5. verify `codchi status` and `codchi exec`
6. install the candidate build
7. run automatic migration
8. verify `codchi status`, `codchi exec`, and the sentinel file again

This should be release-blocking because it protects existing Windows WSL users.

## 2. Windows WSL Fresh-Install Smoke Test

Keep a direct current-version WSL test for the candidate build, separate from
upgrade and migration.

Flow:

1. install the candidate build
2. create the store
3. create one base machine
4. run `codchi exec <machine> env`
5. verify simple file persistence inside the machine

This catches WSL/runtime breakage that is unrelated to migration.

## 3. Scheduled WSL Runs

Run the real WSL suites on a schedule in addition to release or PR gates.

WSL changes independently of Codchi. Scheduled failures are useful even when the
Codchi source tree has not changed.

The scheduled logs should record:

- Codchi version
- Windows runner image/version
- `wsl.exe --version`
- package source

## 4. Code-Only Migration Tests

Add fast migration tests that do not require WSL.

These should use old-state fixtures and cover:

- migration planning
- dry-run JSON
- idempotency
- backup preservation
- malformed or missing config files
- no destructive operation on failure

These tests do not replace real WSL migration tests. They make migration failures
cheaper to diagnose.

## 5. Podman Fresh-Install Smoke Test

Add a real Linux Podman smoke test for the v1 Linux target.

Flow:

1. install the candidate build
2. create/start the store
3. create one machine
4. run `codchi exec <machine> env`
5. verify simple file persistence
6. rebuild the machine
7. delete the machine and verify expected cleanup

This gives confidence in the Linux v1 platform and exercises shared daemon/job
behavior, but it is lower priority than Windows WSL migration because it does not
protect existing users yet.
