use std::path::PathBuf;

use cel_core::Env;
use cel_core_lsp::settings::{build_env_with_protos, load_proto_registry, load_settings};
use cel_core_lsp::{
    proto_to_diagnostics, to_diagnostics, DocumentState, LineIndex, ProtoDocumentState,
};
use expect_test::expect;
use tower_lsp::lsp_types::Diagnostic;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Format diagnostics into a deterministic, human-readable string.
///
/// Each diagnostic becomes one line:
///   <start_line>:<start_col>-<end_line>:<end_col> <severity> [<code>]: <message>
///
/// Lines are sorted for determinism since HashMap-based variable order is not
/// guaranteed.
fn format_diagnostics(diagnostics: &[Diagnostic]) -> String {
    if diagnostics.is_empty() {
        return "OK (no diagnostics)".to_string();
    }

    let mut lines: Vec<String> = diagnostics
        .iter()
        .map(|d| {
            let range = &d.range;
            let severity = match d.severity {
                Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR) => "error",
                Some(tower_lsp::lsp_types::DiagnosticSeverity::WARNING) => "warning",
                Some(tower_lsp::lsp_types::DiagnosticSeverity::INFORMATION) => "info",
                Some(tower_lsp::lsp_types::DiagnosticSeverity::HINT) => "hint",
                _ => "unknown",
            };
            let code = match &d.code {
                Some(tower_lsp::lsp_types::NumberOrString::String(s)) => format!(" [{}]", s),
                Some(tower_lsp::lsp_types::NumberOrString::Number(n)) => format!(" [{}]", n),
                None => String::new(),
            };
            format!(
                "{}:{}-{}:{} {}{}: {}",
                range.start.line,
                range.start.character,
                range.end.line,
                range.end.character,
                severity,
                code,
                d.message,
            )
        })
        .collect();

    lines.sort();
    lines.join("\n")
}

/// Build an Env from a fixture directory's settings.toml, then parse + typecheck
/// the given CEL expression, returning formatted diagnostics.
fn check_cel(fixture_dir: &str, source: &str) -> String {
    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(fixture_dir);
    let settings = load_settings(&fixture_path.join("settings.toml"));
    let env = build_env_with_protos(&settings, &fixture_path);

    let state = DocumentState::with_env(source.to_string(), 0, &env);
    let line_index = LineIndex::new(source.to_string());
    let diagnostics = to_diagnostics(&state.errors, state.check_errors(), &line_index);

    format_diagnostics(&diagnostics)
}

/// Parse + typecheck with the default environment (standard library + all extensions).
fn check_cel_default(source: &str) -> String {
    let env = Env::with_standard_library().with_all_extensions();
    let state = DocumentState::with_env(source.to_string(), 0, &env);
    let line_index = LineIndex::new(source.to_string());
    let diagnostics = to_diagnostics(&state.errors, state.check_errors(), &line_index);

    format_diagnostics(&diagnostics)
}

// ---------------------------------------------------------------------------
// Tests — valid expressions (no diagnostics)
// ---------------------------------------------------------------------------

#[test]
fn valid_arithmetic() {
    let actual = check_cel_default("1 + 2 * 3");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn valid_string_operations() {
    let actual = check_cel_default("'hello'.size() > 0");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn valid_with_declared_variables() {
    let actual = check_cel("basic", "x > 10 && name.startsWith('test')");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn valid_ternary() {
    let actual = check_cel("basic", "flag ? x : 0");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

// ---------------------------------------------------------------------------
// Tests — error diagnostics
// ---------------------------------------------------------------------------

#[test]
fn undeclared_variable() {
    let actual = check_cel_default("unknown_var + 1");
    let expected = expect![[r#"0:0-0:11 error [undeclared-reference]: undeclared reference to 'unknown_var'"#]];
    expected.assert_eq(&actual);
}

#[test]
fn undeclared_variable_with_settings() {
    let actual = check_cel("basic", "x + y");
    let expected = expect![[r#"0:4-0:5 error [undeclared-reference]: undeclared reference to 'y'"#]];
    expected.assert_eq(&actual);
}

#[test]
fn type_mismatch_addition() {
    let actual = check_cel("basic", "x + name");
    let expected = expect![[r#"0:0-0:8 error [no-matching-overload]: no matching overload for '_+_' with argument types (int, string)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn type_mismatch_comparison() {
    let actual = check_cel("basic", "x > name");
    let expected = expect![[r#"0:0-0:8 error [no-matching-overload]: no matching overload for '_>_' with argument types (int, string)"#]];
    expected.assert_eq(&actual);
}

// ---------------------------------------------------------------------------
// Tests — extensions
// ---------------------------------------------------------------------------

#[test]
fn string_extension() {
    let actual = check_cel("extensions", "msg.charAt(0)");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn math_extension() {
    let actual = check_cel("extensions", "math.greatest(val, 0.0)");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

// ---------------------------------------------------------------------------
// Tests — proto types
// ---------------------------------------------------------------------------

#[test]
fn proto_field_access() {
    let actual = check_cel("proto", "user.name");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn proto_undefined_field() {
    let actual = check_cel("proto", "user.nonexistent");
    let expected = expect![[r#"0:0-0:16 error [undefined-field]: undefined field 'nonexistent' on type 'test.User'"#]];
    expected.assert_eq(&actual);
}

#[test]
fn proto_nested_access() {
    let actual = check_cel("proto", "user.address.city");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

// ---------------------------------------------------------------------------
// Tests — protovalidate `this` type resolution
// ---------------------------------------------------------------------------

/// Build a proto source with a message-level protovalidate CEL expression,
/// parse it as a ProtoDocumentState with the proto fixture registry,
/// and return formatted diagnostics.
fn check_protovalidate_message(message_name: &str, cel_expr: &str) -> String {
    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/proto");
    let settings = load_settings(&fixture_path.join("settings.toml"));
    let registry = load_proto_registry(&settings, &fixture_path);

    let proto_source = format!(
        r#"syntax = "proto3";
package test;

message {} {{
    option (buf.validate.message).cel = {{
        expression: "{}"
    }};
}}"#,
        message_name, cel_expr
    );

    let state = ProtoDocumentState::new(proto_source, 0, registry.as_ref());
    let diagnostics = proto_to_diagnostics(&state);
    format_diagnostics(&diagnostics)
}

#[test]
fn protovalidate_this_field_access() {
    let actual = check_protovalidate_message("User", "this.name");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn protovalidate_this_undefined_field() {
    let actual = check_protovalidate_message("User", "this.nonexistent");
    let expected = expect![[r#"5:21-5:37 error [undefined-field]: undefined field 'nonexistent' on type 'test.User'"#]];
    expected.assert_eq(&actual);
}

#[test]
fn protovalidate_this_nested_field_access() {
    let actual = check_protovalidate_message("User", "this.address.city");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

// ---------------------------------------------------------------------------
// Tests — protovalidate field-level CEL expressions
// ---------------------------------------------------------------------------

/// Build a proto source with a field-level protovalidate CEL expression,
/// parse it as a ProtoDocumentState with the proto fixture registry,
/// and return formatted diagnostics.
fn check_protovalidate_field(field_type: &str, field_name: &str, cel_expr: &str) -> String {
    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/proto");
    let settings = load_settings(&fixture_path.join("settings.toml"));
    let registry = load_proto_registry(&settings, &fixture_path);

    let proto_source = format!(
        r#"syntax = "proto3";
package test;

message TestMessage {{
    {field_type} {field_name} = 1 [(buf.validate.field).cel = {{
        expression: "{cel_expr}"
    }}];
}}"#
    );

    let state = ProtoDocumentState::new(proto_source, 0, registry.as_ref());
    let diagnostics = proto_to_diagnostics(&state);
    format_diagnostics(&diagnostics)
}

#[test]
fn protovalidate_field_string_is_email() {
    let actual = check_protovalidate_field("string", "email", "this.isEmail()");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn protovalidate_field_int_comparison() {
    let actual = check_protovalidate_field("int32", "age", "this > 0 && this < 150");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn protovalidate_field_string_size() {
    let actual = check_protovalidate_field("string", "name", "this.size() > 0");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}
