# Codchifile

Idea: A single source of truth for a codchi module which exist in a repository.
This allows the `codchi add` command to only take the url to the repository and
figure everything out from there.

## Implementation

- Nix Flake with nixosModules.NAME / codchiModules.NAME
    - Plain Old NixOS Module inside flake.nix
    - Pros:
        - Compatible with Nix(OS)
        - Flake: Required for pinning nixpkgs
    - Cons: 
        - Tooling not obvious from filename: "flake" != "codchi"
        - Requires manual updating of flake.lock
            => Maybe add `codchi lock update MACHINE MODULE`, autocommit with saved credentials
- Goals:
    - Dont invent new file format
        => Move Codchi config into NixOS modules (e.g. secrets, capabilities)
    - provide `codchi.addNixpkgs`, `codchi.injectInputs` as special arg to allow extending
        => User may provide custom flake inputs directly inside his
           flake.nix which doesn't need additional magic.

## `codchi init NAME MODULE_URL`

- Alternative: `codchi init --empty`

## `codchi add NAME MODULE_URL`
