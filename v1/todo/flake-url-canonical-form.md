# Canonical module URL: format + parser choice

`ModuleSpec.url` (MS8) is the durable module identity, the `resolve_config`
canonicalization output (MS14), and the source of the injective generated
flake-input alias (MS8). Its exact format and the parser/normalizer behind it are
not yet locked. This note records the research and the intended direction; the
concrete normalizer is a Phase-6 (generated `flake.nix`) implementation detail.

## Beta's handling (`crates/beta/beta-codchi/src/config/flake.rs`)

Beta rolls its own `FlakeUrl<W>` = `FlakeLocation` (`Remote { scheme, host, repo,
auth }` | `Local { path }`) + `commit`/`ref` + typestate `flake_attr`, with **two
serializations**:

- a bespoke `Display`/`FromStr` form
  `<scheme>://<host>/<repo>?token=…&commit=…&ref=…#<attr>` (scheme ∈
  github/gitlab/sourcehut/http/https/ssh) — **not** a Nix flake ref; and
- `to_nix_url()` which converts to a real Nix flake ref (`github:owner/repo?host=…`,
  `git+https://…@host/repo?…`, `git+file://…`).

Beta also already depends on `git-url-parse` for parsing the *input* git URL.

Problems for v1: (1) **dual representation** (internal form ≠ what Nix consumes);
(2) **`token=` auth embedded in the serialized URL** — a secret baked into the
durable identity that would also land in SQLite, events, and logs; (3) a
hand-maintained bespoke grammar.

## Crate landscape

- **`nix-uri`** ([docs.rs](https://docs.rs/nix-uri/latest/nix_uri/),
  [github.com/a-kenji/nix-uri](https://github.com/a-kenji/nix-uri)) — the only
  real flake-ref crate. Models flakerefs properly (GitHub/GitLab/Sourcehut
  forges; `git+`/`hg+`/`file`/`tarball`; indirect; path) and round-trips. **But**
  v0.2.0 (May 2026), 4 stars / single maintainer, self-described "early WIP, not
  all cases covered, error handling not properly implemented." `#attr` fragment
  not clearly handled (correctly — it's outside the flakeref grammar).
- **`git-url-parse`** — parses git URLs, not flake refs; cannot be the canonical
  authority (already used by beta for input only).

## Decision direction

1. **Format:** adopt the **standard Nix flake-reference format** as the canonical
   `ModuleSpec.url` — drop beta's bespoke dual format. The canonical URL then *is*
   what gets written into generated `flake.nix` inputs (no conversion), which is
   what MS14 canonicalization and MS8 injectivity want. Handle `#attr`
   separately (split on `#`, as both beta and Nix do).
2. **Parser:** do **not** hard-depend on `nix-uri` as the canonical authority
   yet — canonicalization is core correctness (injective alias, validation gate)
   and the crate is WIP with weak error handling. Codchi owns a **lean normalizer
   over its supported subset** (the beta scheme set), using `url` for the
   `git+https` transport part and hand-rolled forge shorthands, well-tested.
   **Re-evaluate adopting/contributing to `nix-uri`** once it stabilizes.
3. **Security:** auth leaves the URL. v1 must not store tokens in
   `ModuleSpec.url`; auth moves out-of-band (netrc / Nix `access-tokens`).

## When picked up

Lock the format + auth-exclusion + `#attr` rule into MS8/MS14 and add a CONTEXT
glossary entry for *canonical module URL*; implement the normalizer in Phase 6
with a round-trip + injectivity test suite (distinct sources → distinct aliases).
