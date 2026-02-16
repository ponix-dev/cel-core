use std::path::PathBuf;
use std::sync::Arc;

use cel_core::Env;
use cel_core_lsp::{build_env_with_protos, discover_settings, load_proto_registry, load_settings};
use cel_core_lsp::{
    completion_at_position_proto, proto_to_diagnostics, to_diagnostics, DocumentState, LineIndex,
    ProtoDocumentState,
};
use expect_test::expect;
use tower_lsp::lsp_types::{CompletionResponse, Diagnostic, Position};

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

    let state = DocumentState::with_env(source.to_string(), 0, Arc::new(env));
    let line_index = LineIndex::new(source.to_string());
    let diagnostics = to_diagnostics(&state.errors, state.check_errors(), &line_index);

    format_diagnostics(&diagnostics)
}

/// Parse + typecheck with the default environment (standard library + all extensions).
fn check_cel_default(source: &str) -> String {
    let env = Env::with_standard_library().with_all_extensions();
    let state = DocumentState::with_env(source.to_string(), 0, Arc::new(env));
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
    let expected = expect![[
        r#"0:0-0:11 error [undeclared-reference]: undeclared reference to 'unknown_var'"#
    ]];
    expected.assert_eq(&actual);
}

#[test]
fn undeclared_variable_with_settings() {
    let actual = check_cel("basic", "x + y");
    let expected =
        expect![[r#"0:4-0:5 error [undeclared-reference]: undeclared reference to 'y'"#]];
    expected.assert_eq(&actual);
}

#[test]
fn type_mismatch_addition() {
    let actual = check_cel("basic", "x + name");
    let expected = expect![[
        r#"0:0-0:8 error [no-matching-overload]: no matching overload for '_+_' with argument types (int, string)"#
    ]];
    expected.assert_eq(&actual);
}

#[test]
fn type_mismatch_comparison() {
    let actual = check_cel("basic", "x > name");
    let expected = expect![[
        r#"0:0-0:8 error [no-matching-overload]: no matching overload for '_>_' with argument types (int, string)"#
    ]];
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
    let expected = expect![[
        r#"0:0-0:16 error [undefined-field]: undefined field 'nonexistent' on type 'test.User'"#
    ]];
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
    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/proto");
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
    let expected = expect![[
        r#"5:21-5:37 error [undefined-field]: undefined field 'nonexistent' on type 'test.User'"#
    ]];
    expected.assert_eq(&actual);
}

#[test]
fn protovalidate_this_nested_field_access() {
    let actual = check_protovalidate_message("User", "this.address.city");
    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);
}

#[test]
fn protovalidate_has_undefined_field() {
    let actual = check_protovalidate_message("User", "has(this.nonexistent)");
    let expected = expect![[
        r#"5:21-5:42 error [undefined-field]: undefined field 'nonexistent' on type 'test.User'"#
    ]];
    expected.assert_eq(&actual);
}

// ---------------------------------------------------------------------------
// Tests — protovalidate field-level CEL expressions
// ---------------------------------------------------------------------------

/// Build a proto source with a field-level protovalidate CEL expression,
/// parse it as a ProtoDocumentState with the proto fixture registry,
/// and return formatted diagnostics.
fn check_protovalidate_field(field_type: &str, field_name: &str, cel_expr: &str) -> String {
    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/proto");
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

// ---------------------------------------------------------------------------
// Tests — proto completion
// ---------------------------------------------------------------------------

/// Helper: build a proto file with a field-level CEL expression containing
/// the cursor at the given position, then return completion labels.
fn get_proto_completions(
    field_type: &str,
    field_name: &str,
    cel_expr: &str,
    cursor_col_within_cel: u32,
) -> Vec<String> {
    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/proto");
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

    // The expression string is on line 5 (0-indexed), starting after `expression: "`
    // Line 5 is: `        expression: "<cel_expr>"`
    //             0       8         18  21
    // The CEL content starts at column 21 (after 8 spaces + `expression: "`)
    let cel_start_col = 21u32;
    let position = Position::new(5, cel_start_col + cursor_col_within_cel);

    match completion_at_position_proto(&state, position) {
        Some(CompletionResponse::Array(items)) => items.into_iter().map(|i| i.label).collect(),
        _ => vec![],
    }
}

/// Helper: build a proto file with a message-level protovalidate CEL expression,
/// then return completion labels at the given cursor position within the CEL.
fn get_proto_message_completions(
    message_name: &str,
    cel_expr: &str,
    cursor_col_within_cel: u32,
) -> Vec<String> {
    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/proto");
    let settings = load_settings(&fixture_path.join("settings.toml"));
    let registry = load_proto_registry(&settings, &fixture_path);

    let proto_source = format!(
        r#"syntax = "proto3";
package test;

message {message_name} {{
    option (buf.validate.message).cel = {{
        expression: "{cel_expr}"
    }};
}}"#
    );

    let state = ProtoDocumentState::new(proto_source, 0, registry.as_ref());

    let cel_start_col = 21u32;
    let position = Position::new(5, cel_start_col + cursor_col_within_cel);

    match completion_at_position_proto(&state, position) {
        Some(CompletionResponse::Array(items)) => items.into_iter().map(|i| i.label).collect(),
        _ => vec![],
    }
}

#[test]
fn proto_completion_has_this_field_access() {
    // `has(this.)` with cursor after the dot (offset 9 in CEL)
    let labels = get_proto_message_completions("User", "has(this.)", 9);
    assert!(
        labels.contains(&"name".to_string()),
        "should suggest 'name' field inside has(this.): {:?}",
        labels
    );
    assert!(
        labels.contains(&"email".to_string()),
        "should suggest 'email' field inside has(this.): {:?}",
        labels
    );
    assert!(
        labels.contains(&"address".to_string()),
        "should suggest 'address' field inside has(this.): {:?}",
        labels
    );
}

#[test]
fn proto_completion_string_field_member_access() {
    // `this.` with cursor at end (offset 5 in CEL)
    let labels = get_proto_completions("string", "email", "this.", 5);
    assert!(
        labels.contains(&"isEmail".to_string()),
        "should suggest isEmail for string field: {:?}",
        labels
    );
    assert!(
        labels.contains(&"contains".to_string()),
        "should suggest contains for string field: {:?}",
        labels
    );
}

#[test]
fn proto_completion_string_field_mid_expression() {
    // `this.isEmail()` with cursor after dot (offset 5 in CEL)
    let labels = get_proto_completions("string", "email", "this.isEmail()", 5);
    assert!(
        labels.contains(&"isEmail".to_string()),
        "should suggest isEmail mid-expression: {:?}",
        labels
    );
}

#[test]
fn proto_completion_int_field_no_string_methods() {
    // `this.` with cursor at end (offset 5 in CEL) on int field
    let labels = get_proto_completions("int32", "age", "this.", 5);
    assert!(
        !labels.contains(&"isEmail".to_string()),
        "should NOT suggest isEmail for int field: {:?}",
        labels
    );
    assert!(
        !labels.contains(&"contains".to_string()),
        "should NOT suggest contains for int field: {:?}",
        labels
    );
}

// ---------------------------------------------------------------------------
// Tests — settings discovery
// ---------------------------------------------------------------------------

/// Use discover_settings from a subdirectory to find settings in the fixtures
/// parent, then verify .cel files get the configured variables.
#[test]
fn discover_settings_applies_to_cel_files() {
    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/basic");

    // Simulate discovering settings from a child directory
    let child = fixture_path.join("subdir");
    std::fs::create_dir_all(&child).ok();

    let (settings, settings_dir) = discover_settings(&child);
    assert_eq!(settings_dir, fixture_path);

    let env = Arc::new(build_env_with_protos(&settings, &settings_dir));

    // CEL expression using variables declared in basic/settings.toml should work
    let state = DocumentState::with_env("x > 10 && name.startsWith('test')".to_string(), 0, env);
    let line_index = LineIndex::new(state.source.clone());
    let diagnostics = to_diagnostics(&state.errors, state.check_errors(), &line_index);
    let actual = format_diagnostics(&diagnostics);

    let expected = expect![[r#"OK (no diagnostics)"#]];
    expected.assert_eq(&actual);

    // Clean up temp dir
    let _ = std::fs::remove_dir(&child);
}
