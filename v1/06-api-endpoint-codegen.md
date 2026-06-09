# 06 — API Endpoint Catalog & Client/Server Wiring

How the HTTP layer is described once and reused by the server router, the typed
client, and OpenAPI — without an external code generator and without
stringly-typed dispatch.

This note records a decision that the earlier docs only gestured at
([01-architecture.md](01-architecture.md) §"API Source of Truth": *"a semantic
service trait used by server and typed clients"*, *"URL mapping is hand-written
once and reused by both the axum router and the typed client builder"*). It does
not change any locked Phase 0 contract decision; it specifies the **mechanism**.

## What was already decided

- HTTP/JSON, `axum` server, one shared `AppState { core: Arc<ServerCore> }`,
  thin handlers (`01-architecture.md`).
- One semantic trait, [`CodchiService`], implemented **directly** by
  `ServerCore` and **over the wire** by the HTTP client; the mock implements it
  for tests.
- `codchi-api` is the source of truth and **generates OpenAPI from the Rust
  types** (we do not generate Rust from OpenAPI).
- Transport (Unix socket vs. TCP, auth) is deliberately open (Phase 1/17).

## The governing constraint

`codchi-api` must not depend on `axum` or `reqwest` (locked crate rule). So the
per-endpoint facts live in `codchi-api`, while the actual `axum::Router` and the
`reqwest`/`hyper` client live in the downstream crates. Any "describe once"
scheme must therefore expose **neutral, typed metadata** from `codchi-api` that
downstream **generic** code consumes — a macro in `codchi-api` cannot emit axum
code directly.

## Decision: a typed `Endpoint` catalog

Each route is a zero-sized marker type implementing one trait, declared once in
the `endpoints! { … }` block in [`codchi-api/src/endpoints.rs`]:

```rust
pub trait Endpoint {
    const METHOD: Method;
    const PATH: &'static str;       // full path incl. /v1, with {param} holes
    const OPERATION_ID: &'static str;
    const SUMMARY: &'static str;
    const SUCCESS_STATUS: u16;       // 200 / 204
    const RESPONSE: ResponseShape;   // Json | Ndjson | Empty
    const ROUTE: Route;              // derived cheap metadata

    type Body;       // request DTO; () ⇒ no body
    type Query;      // query struct; () ⇒ none; fields become query params
    type Response;   // success DTO; () ⇒ empty; element type for NDJSON
}
```

One declaration per route yields, with no duplication:

- **`ROUTES: &[Route]`** — cheap `const` metadata for routing and `route(op_id)`
  lookup.
- **OpenAPI** — `openapi.rs` iterates `endpoints::build_operations(...)`; bodies,
  query params, and responses come from the **same tokens** as each `Endpoint`
  impl, so the spec cannot drift from the types. Query parameters are
  introspected from `Endpoint::Query`'s `JsonSchema` (one param per field), so
  even those can't drift.
- **Phase 1 server/client** — generic over `E: Endpoint` (below).

Endpoints are matched by **type**, not by `match operation_id` over `&str`.
Adding a route is one entry; forgetting to wire its request/response is a
**compile error**, not a runtime panic. This replaces the previous
string-keyed `wire_operation`/`summary` matches.

### Why not the alternatives

- **External OpenAPI codegen** (progenitor / openapi-generator): generates Rust
  *from* JSON generated *from* Rust — a circular, lossy round-trip that loses our
  newtype IDs and the generic `JobView<O>` typing, and adds a build-time
  toolchain. Rejected.
- **`aide`'s axum integration** deriving OpenAPI from handlers: makes handlers
  the OpenAPI source, contradicting locked **P1** (catalog is the source).
  Rejected; aide's axum feature stays off.
- **A macro that also generates the trait + dispatch + client** (tarpc-style):
  maximal DRY but high magic; the trait stops being hand-readable and the mock
  lives downstream of a macro. Not worth it for ~26 internal routes.

## Phase 1 plan — generic router and client

The payoff of the typed catalog: the server and client are written **once**,
generic over the endpoint.

**Server (`codchi-server`, axum).** A generic registration helper per response
shape, e.g.:

```rust
fn mount<E: Endpoint>(router: Router, handler: H) -> Router
where E::Body: DeserializeOwned, E::Response: Serialize { … }
```

It builds the route at `E::PATH` for `E::METHOD`, extracts the path params, the
query (`E::Query`), and the JSON body (`E::Body`), calls the matching
`ServerCore`/`CodchiService` method, and serialises `E::Response` per
`E::RESPONSE` (JSON 200 / NDJSON stream / empty 204). A test asserts the router
covers every entry in `ROUTES` (same trick as the OpenAPI-coverage test today).

**Client (CLI/tray crate, reqwest/hyper).** A generic call helper:

```rust
async fn call<E: Endpoint>(&self, path_args: …, query: E::Query, body: E::Body)
    -> Result<E::Response, ApiError> { … }
```

behind a hand-written `impl CodchiService for HttpClient` whose methods are
one-liners: `self.call::<CloneMachineEp>(…)`. The `ApiError` default-response
shape and the `JobView<O>` typing flow through unchanged.

**`CodchiService` and the mock stay hand-written.** They are the readable
semantic surface and the test double; nothing generates them.

### What the catalog still needs for Phase 1

The Phase 0 catalog models `Body`, `Query`, `Response`. To fully drive transport
generically, Phase 1 must also model **path parameters** as a type (today they
are derived from the `{name}` segments of `PATH` as untyped strings):

```rust
type Path;   // e.g. (MachineId,) or (MachineId, SecretKey) or (JobId,)
```

with a small trait to render the tuple into the path template and parse it back
out of axum's matched params. This is the one piece of additional machinery D
needs beyond the Phase 0 surface.

### The residual glue (and why it stays)

`Endpoint` gives generic **plumbing**, but it cannot auto-derive the
`CodchiService` method **bodies**: Rust can't introspect a trait method's
argument list. So each method keeps a one-line mapping from its arguments to
`call::<SomeEp>(path_args, query, body)`. Eliminating even that would require the
trait-generating macro rejected above. The glue is small, explicit, and
debuggable; a `ROUTES`-coverage test keeps it honest.

## Status

- **Implemented in Phase 0:** the `Endpoint` trait + marker catalog, `ROUTES`,
  `route()`, OpenAPI generation driven off the catalog, query-param
  introspection, and removal of `operation_id`-string dispatch.
- **Deferred to Phase 1:** `type Path` + the render/parse trait, the generic
  `mount<E>` / `call<E>` helpers, and the `HttpClient` `CodchiService` impl. See
  [PLAN.md](PLAN.md) Phase 1.
