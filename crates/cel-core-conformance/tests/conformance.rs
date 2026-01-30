//! CEL conformance integration tests.
//!
//! These tests run the cel-parser, cel-checker, and cel-eval against official CEL conformance
//! test files from the cel-spec repository.
//!
//! - Parse tests: Verify expressions parse successfully
//! - Check tests: Verify type checker produces expected types
//! - Eval tests: Verify evaluation produces expected results

use cel_core_conformance::{
    load_test_file, Binding, CelConformanceService, ConformanceService, SimpleTest, TypeDecl,
};
use cel_core_proto::gen::cel::expr::conformance::test::simple_test::ResultMatcher;
use cel_core_proto::gen::cel::expr::conformance::test::{ErrorSetMatcher, UnknownSetMatcher};
use cel_core_proto::gen::cel::expr::decl::DeclKind;
use cel_core_proto::gen::cel::expr::r#type::TypeKind;
use cel_core_proto::gen::cel::expr::value::Kind as ValueKind;
use cel_core_proto::gen::cel::expr::Type as ProtoType;
use cel_core_proto::gen::cel::expr::{expr_value, ErrorSet, ExprValue, UnknownSet, Value as ProtoValue};
use cel_core_proto::cel_type_from_proto;
use std::path::Path;

const TESTDATA_PATH: &str = "cel-spec/tests/simple/testdata";

/// Compare two proto Types for equivalence.
///
/// - Dyn in expected matches any actual type
/// - Recursively compares nested types (list, map, abstract)
fn types_equivalent(actual: &ProtoType, expected: &ProtoType) -> bool {
    match (&actual.type_kind, &expected.type_kind) {
        // Dyn in expected matches anything
        (_, Some(TypeKind::Dyn(_))) => true,

        // Same primitive types
        (Some(TypeKind::Primitive(a)), Some(TypeKind::Primitive(e))) => a == e,

        // Same well-known types
        (Some(TypeKind::WellKnown(a)), Some(TypeKind::WellKnown(e))) => a == e,

        // Null types
        (Some(TypeKind::Null(_)), Some(TypeKind::Null(_))) => true,

        // List types - compare element types
        (Some(TypeKind::ListType(a)), Some(TypeKind::ListType(e))) => {
            match (a.elem_type.as_ref(), e.elem_type.as_ref()) {
                (Some(a_elem), Some(e_elem)) => types_equivalent(a_elem, e_elem),
                (None, None) => true,
                _ => false,
            }
        }

        // Map types - compare key and value types
        (Some(TypeKind::MapType(a)), Some(TypeKind::MapType(e))) => {
            let keys_match = match (a.key_type.as_ref(), e.key_type.as_ref()) {
                (Some(a_key), Some(e_key)) => types_equivalent(a_key, e_key),
                (None, None) => true,
                _ => false,
            };
            let values_match = match (a.value_type.as_ref(), e.value_type.as_ref()) {
                (Some(a_val), Some(e_val)) => types_equivalent(a_val, e_val),
                (None, None) => true,
                _ => false,
            };
            keys_match && values_match
        }

        // Message types - compare names
        (Some(TypeKind::MessageType(a)), Some(TypeKind::MessageType(e))) => a == e,

        // Type parameter names
        (Some(TypeKind::TypeParam(a)), Some(TypeKind::TypeParam(e))) => a == e,

        // Type of type - compare inner types
        (Some(TypeKind::Type(a)), Some(TypeKind::Type(e))) => types_equivalent(a, e),

        // Wrapper types
        (Some(TypeKind::Wrapper(a)), Some(TypeKind::Wrapper(e))) => a == e,

        // Abstract types - compare name and parameters
        (Some(TypeKind::AbstractType(a)), Some(TypeKind::AbstractType(e))) => {
            if a.name != e.name {
                return false;
            }
            if a.parameter_types.len() != e.parameter_types.len() {
                return false;
            }
            a.parameter_types
                .iter()
                .zip(e.parameter_types.iter())
                .all(|(a_param, e_param)| types_equivalent(a_param, e_param))
        }

        // Error types
        (Some(TypeKind::Error(_)), Some(TypeKind::Error(_))) => true,

        // Function types
        (Some(TypeKind::Function(a)), Some(TypeKind::Function(e))) => {
            if a.arg_types.len() != e.arg_types.len() {
                return false;
            }
            let args_match = a
                .arg_types
                .iter()
                .zip(e.arg_types.iter())
                .all(|(a_arg, e_arg)| types_equivalent(a_arg, e_arg));
            let result_match = match (a.result_type.as_ref(), e.result_type.as_ref()) {
                (Some(a_res), Some(e_res)) => types_equivalent(a_res, e_res),
                (None, None) => true,
                _ => false,
            };
            args_match && result_match
        }

        // None cases
        (None, None) => true,

        // Anything else doesn't match
        _ => false,
    }
}

/// Compare two proto Values for equivalence.
///
/// Handles:
/// - Primitives: null, bool, int64, uint64, double, string, bytes
/// - Float NaN: Both NaN should match
/// - Lists: Same length, recursive element comparison
/// - Maps: Same keys, recursive value comparison (order-agnostic)
/// - Type values: Compare type names
fn values_equivalent(actual: &ProtoValue, expected: &ProtoValue) -> bool {
    match (&actual.kind, &expected.kind) {
        // Null values
        (Some(ValueKind::NullValue(_)), Some(ValueKind::NullValue(_))) => true,

        // Boolean values
        (Some(ValueKind::BoolValue(a)), Some(ValueKind::BoolValue(e))) => a == e,

        // Integer values
        (Some(ValueKind::Int64Value(a)), Some(ValueKind::Int64Value(e))) => a == e,

        // Unsigned integer values
        (Some(ValueKind::Uint64Value(a)), Some(ValueKind::Uint64Value(e))) => a == e,

        // Double values - special case for NaN
        (Some(ValueKind::DoubleValue(a)), Some(ValueKind::DoubleValue(e))) => {
            if a.is_nan() && e.is_nan() {
                true
            } else {
                a == e
            }
        }

        // String values
        (Some(ValueKind::StringValue(a)), Some(ValueKind::StringValue(e))) => a == e,

        // Bytes values
        (Some(ValueKind::BytesValue(a)), Some(ValueKind::BytesValue(e))) => a == e,

        // List values - compare element by element
        (Some(ValueKind::ListValue(a)), Some(ValueKind::ListValue(e))) => {
            if a.values.len() != e.values.len() {
                return false;
            }
            a.values
                .iter()
                .zip(e.values.iter())
                .all(|(a_val, e_val)| values_equivalent(a_val, e_val))
        }

        // Map values - order-agnostic comparison
        (Some(ValueKind::MapValue(a)), Some(ValueKind::MapValue(e))) => {
            if a.entries.len() != e.entries.len() {
                return false;
            }
            // For each entry in expected, find a matching entry in actual
            for e_entry in &e.entries {
                let e_key = match &e_entry.key {
                    Some(k) => k,
                    None => return false,
                };
                let e_value = match &e_entry.value {
                    Some(v) => v,
                    None => return false,
                };

                // Find matching entry in actual
                let found = a.entries.iter().any(|a_entry| {
                    let a_key = match &a_entry.key {
                        Some(k) => k,
                        None => return false,
                    };
                    let a_value = match &a_entry.value {
                        Some(v) => v,
                        None => return false,
                    };
                    values_equivalent(a_key, e_key) && values_equivalent(a_value, e_value)
                });

                if !found {
                    return false;
                }
            }
            true
        }

        // Type values
        (Some(ValueKind::TypeValue(a)), Some(ValueKind::TypeValue(e))) => a == e,

        // Enum values
        (Some(ValueKind::EnumValue(a)), Some(ValueKind::EnumValue(e))) => {
            a.r#type == e.r#type && a.value == e.value
        }

        // Object values (proto messages) - compare Any type URLs and values
        (Some(ValueKind::ObjectValue(a)), Some(ValueKind::ObjectValue(e))) => {
            a.type_url == e.type_url && a.value == e.value
        }

        // None cases
        (None, None) => true,

        // Anything else doesn't match
        _ => false,
    }
}

/// Check if actual result matches expected error.
/// For now, any error matches any expected error (CEL spec doesn't require exact message matching).
fn matches_eval_error(actual: &ExprValue, _expected: &ErrorSet) -> bool {
    matches!(&actual.kind, Some(expr_value::Kind::Error(_)))
}

/// Check if actual result matches any of the expected errors.
fn matches_any_eval_errors(actual: &ExprValue, expected: &ErrorSetMatcher) -> bool {
    // If actual is an error, it matches if there's at least one expected error set
    if matches!(&actual.kind, Some(expr_value::Kind::Error(_))) {
        // Any error matches any expected error set
        !expected.errors.is_empty()
    } else {
        false
    }
}

/// A failure in an eval conformance test.
#[derive(Debug)]
struct EvalTestFailure {
    test_name: String,
    expr: String,
    reason: String,
}

/// Convert bindings from test data (ExprValue map) to Binding vec.
fn convert_bindings(
    bindings: &std::collections::HashMap<String, ExprValue>,
) -> Result<Vec<Binding>, String> {
    let mut result = Vec::new();
    for (name, expr_value) in bindings {
        match &expr_value.kind {
            Some(expr_value::Kind::Value(value)) => {
                result.push(Binding {
                    name: name.clone(),
                    value: value.clone(),
                });
            }
            Some(expr_value::Kind::Error(_)) => {
                return Err(format!("binding '{}' is an error value", name));
            }
            Some(expr_value::Kind::Unknown(_)) => {
                return Err(format!("binding '{}' is an unknown value", name));
            }
            None => {
                return Err(format!("binding '{}' has no value", name));
            }
        }
    }
    Ok(result)
}

/// Run evaluation tests in a file and collect failures.
fn run_eval_conformance_file(filename: &str) -> (usize, Vec<EvalTestFailure>) {
    let path = Path::new(TESTDATA_PATH).join(filename);
    let file = load_test_file(&path).expect(&format!("Failed to load {}", filename));

    let service = CelConformanceService::new();
    let mut failures = Vec::new();
    let mut total_tests = 0;

    for section in &file.section {
        for test in &section.test {
            let test_name = format!("{}/{}", section.name, test.name);

            // Skip only if check_only is true (evaluation explicitly disabled by test)
            if test.check_only {
                continue;
            }

            total_tests += 1;

            // Determine expected result type
            let expected_result = match &test.result_matcher {
                Some(ResultMatcher::Value(v)) => ExpectedResult::Value(v.clone()),
                Some(ResultMatcher::TypedResult(tr)) => {
                    // For typed_result, compare the value part if present
                    match &tr.result {
                        Some(v) => ExpectedResult::Value(v.clone()),
                        None => {
                            // typed_result with no value defaults to true
                            ExpectedResult::Value(ProtoValue {
                                kind: Some(ValueKind::BoolValue(true)),
                            })
                        }
                    }
                }
                Some(ResultMatcher::EvalError(e)) => ExpectedResult::Error(e.clone()),
                Some(ResultMatcher::AnyEvalErrors(e)) => ExpectedResult::AnyErrors(e.clone()),
                Some(ResultMatcher::Unknown(u)) => ExpectedResult::Unknown(u.clone()),
                Some(ResultMatcher::AnyUnknowns(u)) => ExpectedResult::AnyUnknowns(u.clone()),
                None => {
                    // Default is true boolean value
                    ExpectedResult::Value(ProtoValue {
                        kind: Some(ValueKind::BoolValue(true)),
                    })
                }
            };

            // Parse the expression
            let parse_result = service.parse(&test.expr);
            if !parse_result.is_ok() {
                failures.push(EvalTestFailure {
                    test_name: test_name.clone(),
                    expr: test.expr.clone(),
                    reason: format!(
                        "parse failed: {}",
                        parse_result
                            .issues
                            .iter()
                            .map(|i| i.message.clone())
                            .collect::<Vec<_>>()
                            .join("; ")
                    ),
                });
                continue;
            }
            let parsed_expr = parse_result.parsed_expr.unwrap();

            // Convert bindings
            let bindings = match convert_bindings(&test.bindings) {
                Ok(b) => b,
                Err(e) => {
                    failures.push(EvalTestFailure {
                        test_name: test_name.clone(),
                        expr: test.expr.clone(),
                        reason: format!("binding conversion failed: {}", e),
                    });
                    continue;
                }
            };

            // Build type declarations from type_env
            let type_decls = build_type_decls(test);

            // Evaluate - always get a result to compare
            let eval_result = service.eval(&parsed_expr, &bindings, &type_decls, &test.container);
            let actual_result = match &eval_result.result {
                Some(r) => r,
                None => {
                    failures.push(EvalTestFailure {
                        test_name: test_name.clone(),
                        expr: test.expr.clone(),
                        reason: format!(
                            "eval returned no result: {}",
                            eval_result
                                .issues
                                .iter()
                                .map(|i| i.message.clone())
                                .collect::<Vec<_>>()
                                .join("; ")
                        ),
                    });
                    continue;
                }
            };

            // Compare results based on expected type
            let matches = match &expected_result {
                ExpectedResult::Value(expected) => {
                    match &actual_result.kind {
                        Some(expr_value::Kind::Value(actual)) => {
                            values_equivalent(actual, expected)
                        }
                        Some(expr_value::Kind::Error(_)) => false,
                        Some(expr_value::Kind::Unknown(_)) => false,
                        None => false,
                    }
                }
                ExpectedResult::Error(expected) => {
                    matches_eval_error(actual_result, expected)
                }
                ExpectedResult::AnyErrors(expected) => {
                    matches_any_eval_errors(actual_result, expected)
                }
                ExpectedResult::Unknown(_expected) => {
                    // Check if actual is an unknown
                    matches!(&actual_result.kind, Some(expr_value::Kind::Unknown(_)))
                }
                ExpectedResult::AnyUnknowns(_expected) => {
                    // Check if actual is an unknown
                    matches!(&actual_result.kind, Some(expr_value::Kind::Unknown(_)))
                }
            };

            if !matches {
                let actual_desc = match &actual_result.kind {
                    Some(expr_value::Kind::Value(v)) => format!("Value({:?})", v.kind),
                    Some(expr_value::Kind::Error(e)) => format!("Error({:?})", e),
                    Some(expr_value::Kind::Unknown(u)) => format!("Unknown({:?})", u),
                    None => "None".to_string(),
                };
                let expected_desc = match &expected_result {
                    ExpectedResult::Value(v) => format!("Value({:?})", v.kind),
                    ExpectedResult::Error(e) => format!("Error({:?})", e),
                    ExpectedResult::AnyErrors(e) => format!("AnyErrors({:?})", e),
                    ExpectedResult::Unknown(u) => format!("Unknown({:?})", u),
                    ExpectedResult::AnyUnknowns(u) => format!("AnyUnknowns({:?})", u),
                };
                failures.push(EvalTestFailure {
                    test_name: test_name.clone(),
                    expr: test.expr.clone(),
                    reason: format!("expected {} but got {}", expected_desc, actual_desc),
                });
            }
        }
    }

    (total_tests, failures)
}

/// Expected result type for evaluation tests.
#[derive(Clone, Debug)]
enum ExpectedResult {
    Value(ProtoValue),
    Error(ErrorSet),
    AnyErrors(ErrorSetMatcher),
    Unknown(UnknownSet),
    AnyUnknowns(UnknownSetMatcher),
}

/// Build type declarations from test's type_env
fn build_type_decls(test: &SimpleTest) -> Vec<TypeDecl> {
    let mut decls = Vec::new();

    for decl in &test.type_env {
        match &decl.decl_kind {
            Some(DeclKind::Ident(ident)) => {
                if let Some(ty) = &ident.r#type {
                    decls.push(TypeDecl {
                        name: decl.name.clone(),
                        cel_type: ty.clone(),
                    });
                }
            }
            Some(DeclKind::Function(_func)) => {
                // Function declarations are not yet supported in TypeDecl
                // The checker would need to be extended to support custom functions
            }
            None => {}
        }
    }

    decls
}

/// Run type checking tests in a file and collect failures
fn run_check_conformance_file(filename: &str) -> (usize, Vec<String>) {
    let path = Path::new(TESTDATA_PATH).join(filename);
    let file = load_test_file(&path).expect(&format!("Failed to load {}", filename));

    let service = CelConformanceService::new();
    let mut failures = Vec::new();
    let mut total_tests = 0;

    for section in &file.section {
        for test in &section.test {
            let test_name = format!("{}/{}", section.name, test.name);

            // Skip only if disable_check is true (CEL spec flag)
            if test.disable_check {
                continue;
            }

            // Get expected type if this is a typed_result test
            let expected_type = match &test.result_matcher {
                Some(ResultMatcher::TypedResult(tr)) => tr.deduced_type.as_ref(),
                _ => None,
            };

            total_tests += 1;

            // Parse the expression
            let parse_result = service.parse(&test.expr);
            if !parse_result.is_ok() {
                failures.push(format!("{}: parse failed", test_name));
                continue;
            }
            let parsed_expr = parse_result.parsed_expr.unwrap();

            // Build type declarations from type_env
            let type_decls = build_type_decls(test);

            // Run the type checker with container from test
            let check_result = service.check(&parsed_expr, &type_decls, &test.container);
            if !check_result.is_ok() {
                let errors: Vec<_> = check_result.issues.iter().map(|i| i.message.clone()).collect();
                failures.push(format!("{}: check failed: {}", test_name, errors.join("; ")));
                continue;
            }

            // If no expected type, we're done (just verified check succeeded)
            let expected_type = match expected_type {
                Some(ty) => ty,
                None => continue,
            };

            let checked_expr = check_result.checked_expr.unwrap();

            // Get the root expression ID
            let root_id = match &parsed_expr.expr {
                Some(expr) => expr.id,
                None => {
                    failures.push(format!("{}: no root expression", test_name));
                    continue;
                }
            };

            // Get the actual type from type_map
            let actual_type = match checked_expr.type_map.get(&root_id) {
                Some(ty) => ty,
                None => {
                    // If not in type_map, it might be Dyn (which is omitted)
                    // Create a Dyn type for comparison
                    &ProtoType {
                        type_kind: Some(TypeKind::Dyn(())),
                    }
                }
            };

            // Compare types
            if !types_equivalent(actual_type, expected_type) {
                let actual_cel = cel_type_from_proto(actual_type);
                let expected_cel = cel_type_from_proto(expected_type);
                failures.push(format!(
                    "{}: type mismatch - expected {} but got {}",
                    test_name,
                    expected_cel.display_name(),
                    actual_cel.display_name()
                ));
            }
        }
    }

    (total_tests, failures)
}

/// Run all tests in a file and collect failures (parse + check)
fn run_conformance_file(filename: &str) -> Vec<String> {
    let path = Path::new(TESTDATA_PATH).join(filename);
    let file = load_test_file(&path).expect(&format!("Failed to load {}", filename));

    let service = CelConformanceService::new();
    let mut failures = Vec::new();

    for section in &file.section {
        for test in &section.test {
            let test_name = format!("{}/{}", section.name, test.name);

            // Parse the expression
            let parse_result = service.parse(&test.expr);
            if !parse_result.is_ok() {
                let error_msg = parse_result
                    .issues
                    .iter()
                    .map(|i| i.message.clone())
                    .collect::<Vec<_>>()
                    .join("; ");
                failures.push(format!("{}: parse error: {}", test_name, error_msg));
                continue;
            }

            // Skip type checking if disabled
            if test.disable_check {
                continue;
            }

            // Build type declarations from type_env
            let type_decls = build_type_decls(test);

            // Run type checker with container from test
            let parsed_expr = parse_result.parsed_expr.unwrap();
            let check_result = service.check(&parsed_expr, &type_decls, &test.container);
            if !check_result.is_ok() {
                let error_msg = check_result
                    .issues
                    .iter()
                    .map(|i| i.message.clone())
                    .collect::<Vec<_>>()
                    .join("; ");
                failures.push(format!("{}: check error: {}", test_name, error_msg));
            }
        }
    }

    failures
}

macro_rules! conformance_test {
    ($name:ident, $file:expr) => {
        #[test]
        fn $name() {
            let failures = run_conformance_file($file);

            if !failures.is_empty() {
                panic!(
                    "{}: {} parse failures:\n  {}",
                    $file,
                    failures.len(),
                    failures.join("\n  ")
                );
            }
        }
    };
}

conformance_test!(test_basic, "basic.textproto");
conformance_test!(test_bindings_ext, "bindings_ext.textproto");
conformance_test!(test_block_ext, "block_ext.textproto");
conformance_test!(test_comparisons, "comparisons.textproto");
conformance_test!(test_conversions, "conversions.textproto");
conformance_test!(test_dynamic, "dynamic.textproto");
conformance_test!(test_encoders_ext, "encoders_ext.textproto");
conformance_test!(test_enums, "enums.textproto");
conformance_test!(test_fields, "fields.textproto");
conformance_test!(test_fp_math, "fp_math.textproto");
conformance_test!(test_integer_math, "integer_math.textproto");
conformance_test!(test_lists, "lists.textproto");
conformance_test!(test_logic, "logic.textproto");
conformance_test!(test_macros, "macros.textproto");
conformance_test!(test_macros2, "macros2.textproto");
conformance_test!(test_math_ext, "math_ext.textproto");
conformance_test!(test_namespace, "namespace.textproto");
conformance_test!(test_optionals, "optionals.textproto");
conformance_test!(test_parse, "parse.textproto");
conformance_test!(test_plumbing, "plumbing.textproto");
conformance_test!(test_proto2_ext, "proto2_ext.textproto");
conformance_test!(test_proto2, "proto2.textproto");
conformance_test!(test_proto3, "proto3.textproto");
conformance_test!(test_string_ext, "string_ext.textproto");
conformance_test!(test_string, "string.textproto");
conformance_test!(test_timestamps, "timestamps.textproto");
conformance_test!(test_type_deduction, "type_deduction.textproto");
conformance_test!(test_unknowns, "unknowns.textproto");
conformance_test!(test_wrappers, "wrappers.textproto");

// Type checking conformance tests
macro_rules! check_conformance_test {
    ($name:ident, $file:expr) => {
        #[test]
        fn $name() {
            let (total, failures) = run_check_conformance_file($file);
            let passed = total - failures.len();

            if !failures.is_empty() {
                panic!(
                    "{}: {}/{} type check tests passed, {} failures:\n  {}",
                    $file,
                    passed,
                    total,
                    failures.len(),
                    failures.join("\n  ")
                );
            }
        }
    };
}

check_conformance_test!(test_type_deduction_check, "type_deduction.textproto");

// Evaluation conformance tests
macro_rules! eval_conformance_test {
    ($name:ident, $file:expr) => {
        #[test]
        fn $name() {
            let (total, failures) = run_eval_conformance_file($file);
            let passed = total - failures.len();

            if !failures.is_empty() {
                let failure_details: Vec<String> = failures
                    .iter()
                    .map(|f| format!("  {}: {} - {}", f.test_name, f.expr, f.reason))
                    .collect();
                panic!(
                    "{}: {}/{} eval tests passed, {} failures:\n{}",
                    $file,
                    passed,
                    total,
                    failures.len(),
                    failure_details.join("\n")
                );
            }
        }
    };
}

// Evaluation conformance tests - run ALL test files
eval_conformance_test!(test_basic_eval, "basic.textproto");
eval_conformance_test!(test_bindings_ext_eval, "bindings_ext.textproto");
eval_conformance_test!(test_block_ext_eval, "block_ext.textproto");
eval_conformance_test!(test_comparisons_eval, "comparisons.textproto");
eval_conformance_test!(test_conversions_eval, "conversions.textproto");
eval_conformance_test!(test_dynamic_eval, "dynamic.textproto");
eval_conformance_test!(test_encoders_ext_eval, "encoders_ext.textproto");
eval_conformance_test!(test_enums_eval, "enums.textproto");
eval_conformance_test!(test_fields_eval, "fields.textproto");
eval_conformance_test!(test_fp_math_eval, "fp_math.textproto");
eval_conformance_test!(test_integer_math_eval, "integer_math.textproto");
eval_conformance_test!(test_lists_eval, "lists.textproto");
eval_conformance_test!(test_logic_eval, "logic.textproto");
eval_conformance_test!(test_macros_eval, "macros.textproto");
eval_conformance_test!(test_macros2_eval, "macros2.textproto");
eval_conformance_test!(test_math_ext_eval, "math_ext.textproto");
eval_conformance_test!(test_namespace_eval, "namespace.textproto");
eval_conformance_test!(test_optionals_eval, "optionals.textproto");
eval_conformance_test!(test_parse_eval, "parse.textproto");
eval_conformance_test!(test_plumbing_eval, "plumbing.textproto");
eval_conformance_test!(test_proto2_ext_eval, "proto2_ext.textproto");
eval_conformance_test!(test_proto2_eval, "proto2.textproto");
eval_conformance_test!(test_proto3_eval, "proto3.textproto");
eval_conformance_test!(test_string_ext_eval, "string_ext.textproto");
eval_conformance_test!(test_string_eval, "string.textproto");
eval_conformance_test!(test_timestamps_eval, "timestamps.textproto");
eval_conformance_test!(test_type_deduction_eval, "type_deduction.textproto");
eval_conformance_test!(test_unknowns_eval, "unknowns.textproto");
eval_conformance_test!(test_wrappers_eval, "wrappers.textproto");
