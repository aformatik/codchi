//! OpenAPI generation (P1).
//!
//! Built from the [`crate::endpoints`] catalog plus `schemars`-derived DTO
//! schemas. The catalog is the single source of truth; this module only holds
//! the `aide` assembly helpers and the document shell. `codchi-server` `axum`
//! handlers consume the same catalog rather than re-describing routes.
//!
//! Produced document is OpenAPI 3.1 with component schemas under
//! `#/components/schemas/`. Use the `gen-openapi` bin to emit the committed
//! `openapi.json` snapshot.

use std::borrow::Cow;

use aide::openapi::{
    Components, Info, MediaType, OpenApi, Parameter, ParameterData, ParameterSchemaOrContent,
    PathItem, Paths, ReferenceOr, RequestBody, Response, Responses, SchemaObject, StatusCode,
};
use indexmap::IndexMap;
use schemars::JsonSchema;
use schemars::r#gen::{SchemaGenerator, SchemaSettings};
use schemars::schema::Schema;

use crate::endpoints::{Method, build_operations};
use crate::error::ApiError;
use crate::version::API_VERSION;

/// Generate the full OpenAPI document for the v1 contract.
pub fn openapi() -> OpenApi {
    let mut g = SchemaSettings::openapi3().into_generator();

    // Register the cross-cutting schemas once and reuse the $ref / clone.
    let err_schema = schema_object(g.subschema_for::<ApiError>());
    let string_schema = schema_object(g.subschema_for::<String>());

    // Operations come from the typed endpoint catalog (single source of truth).
    let mut paths: IndexMap<String, ReferenceOr<PathItem>> = IndexMap::new();
    for entry in build_operations(&mut g, &err_schema, &string_schema) {
        let item = paths
            .entry(entry.path.to_owned())
            .or_insert_with(|| ReferenceOr::Item(PathItem::default()));
        if let ReferenceOr::Item(item) = item {
            match entry.method {
                Method::Get => item.get = Some(entry.operation),
                Method::Post => item.post = Some(entry.operation),
                Method::Delete => item.delete = Some(entry.operation),
            }
        }
    }

    // Component schemas, in deterministic (sorted) order from the generator.
    let mut schemas: IndexMap<String, SchemaObject> = IndexMap::new();
    for (name, schema) in g.definitions() {
        schemas.insert(name.clone(), schema_object(schema.clone()));
    }

    OpenApi {
        openapi: Cow::Borrowed("3.1.0"),
        info: Info {
            title: "Codchi API".to_owned(),
            description: Some(
                "Per-user codchi-server v1 contract. See \
                 v1/phases/00-contract-decisions.md."
                    .to_owned(),
            ),
            version: API_VERSION.to_string(),
            ..Default::default()
        },
        paths: Some(Paths {
            paths,
            extensions: IndexMap::new(),
        }),
        components: Some(Components {
            schemas,
            ..Default::default()
        }),
        ..Default::default()
    }
}

/// The OpenAPI document as pretty-printed JSON (the committed snapshot form).
pub fn openapi_json() -> String {
    serde_json::to_string_pretty(&openapi()).expect("OpenApi serializes")
}

// ---- helpers (shared with the endpoints! catalog) ----

pub(crate) fn schema_object(json_schema: Schema) -> SchemaObject {
    SchemaObject {
        json_schema,
        external_docs: None,
        example: None,
    }
}

fn json_content(schema: SchemaObject) -> IndexMap<String, MediaType> {
    let mut content = IndexMap::new();
    content.insert(
        "application/json".to_owned(),
        MediaType {
            schema: Some(schema),
            ..Default::default()
        },
    );
    content
}

pub(crate) fn json_body<T: JsonSchema>(g: &mut SchemaGenerator) -> ReferenceOr<RequestBody> {
    let schema = schema_object(g.subschema_for::<T>());
    ReferenceOr::Item(RequestBody {
        content: json_content(schema),
        required: true,
        ..Default::default()
    })
}

pub(crate) fn ok_json<T: JsonSchema>(g: &mut SchemaGenerator, desc: &str) -> Response {
    let schema = schema_object(g.subschema_for::<T>());
    Response {
        description: desc.to_owned(),
        content: json_content(schema),
        ..Default::default()
    }
}

/// NDJSON stream of `T` objects, one per line (Q2).
pub(crate) fn ok_ndjson<T: JsonSchema>(g: &mut SchemaGenerator, desc: &str) -> Response {
    let schema = schema_object(g.subschema_for::<T>());
    let mut content = IndexMap::new();
    content.insert(
        "application/x-ndjson".to_owned(),
        MediaType {
            schema: Some(schema),
            ..Default::default()
        },
    );
    Response {
        description: desc.to_owned(),
        content,
        ..Default::default()
    }
}

pub(crate) fn ok_empty(desc: &str) -> Response {
    Response {
        description: desc.to_owned(),
        ..Default::default()
    }
}

pub(crate) fn responses(ok_code: u16, ok: Response, err_schema: &SchemaObject) -> Responses {
    let mut map: IndexMap<StatusCode, ReferenceOr<Response>> = IndexMap::new();
    map.insert(StatusCode::Code(ok_code), ReferenceOr::Item(ok));
    let err = Response {
        description: "Typed API error (ApiError)".to_owned(),
        content: json_content(err_schema.clone()),
        ..Default::default()
    };
    Responses {
        default: Some(ReferenceOr::Item(err)),
        responses: map,
        extensions: IndexMap::new(),
    }
}

fn param_data(name: &str, schema: SchemaObject, required: bool) -> ParameterData {
    ParameterData {
        name: name.to_owned(),
        description: None,
        required,
        deprecated: None,
        format: ParameterSchemaOrContent::Schema(schema),
        example: None,
        examples: IndexMap::new(),
        explode: None,
        extensions: IndexMap::new(),
    }
}

pub(crate) fn path_param(name: &str, schema: SchemaObject) -> Parameter {
    Parameter::Path {
        parameter_data: param_data(name, schema, true),
        style: Default::default(),
    }
}

fn query_param(name: &str, schema: SchemaObject, required: bool) -> Parameter {
    Parameter::Query {
        parameter_data: param_data(name, schema, required),
        allow_reserved: false,
        style: Default::default(),
        allow_empty_value: None,
    }
}

/// Extract `{name}` segments from a path template.
pub(crate) fn path_params(path: &str) -> Vec<&str> {
    path.split('/')
        .filter_map(|seg| seg.strip_prefix('{').and_then(|s| s.strip_suffix('}')))
        .collect()
}

/// Expose each field of a query struct `Q` as an individual query parameter.
/// Drives `stream_job_events` (`EventStreamOpts`) and `doctor` (`DoctorOpts`)
/// off their `Endpoint::Query` type, so the params can never drift from it.
pub(crate) fn query_params<Q: JsonSchema>(g: &mut SchemaGenerator) -> Vec<Parameter> {
    let root = g.subschema_for::<Q>();
    let obj = resolve_object(g, &root);
    let mut out = Vec::new();
    if let Some(ov) = obj.object {
        let required = ov.required;
        for (name, schema) in ov.properties {
            let is_required = required.contains(&name);
            out.push(query_param(&name, schema_object(schema), is_required));
        }
    }
    out
}

/// Resolve a (possibly `$ref`) schema to its concrete object schema, following
/// one level of component reference via the generator's definitions.
fn resolve_object(g: &SchemaGenerator, schema: &Schema) -> schemars::schema::SchemaObject {
    if let Schema::Object(so) = schema {
        if let Some(reference) = &so.reference
            && let Some(name) = reference.rsplit('/').next()
            && let Some(def) = g.definitions().get(name)
        {
            return resolve_object(g, def);
        }
        return so.clone();
    }
    schemars::schema::SchemaObject::default()
}
