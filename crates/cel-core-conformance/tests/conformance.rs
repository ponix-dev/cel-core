//! CEL conformance integration tests.
//!
//! These tests run the cel-parser and cel-checker against official CEL conformance
//! test files from the cel-spec repository.
//!
//! - Parse tests: Verify expressions parse successfully
//! - Check tests: Verify type checker produces expected types

use cel_core_conformance::{
    load_test_file, CelConformanceService, ConformanceService, SimpleTest, TypeDecl,
};
use cel_core_proto::gen::cel::expr::conformance::test::simple_test::ResultMatcher;
use cel_core_proto::gen::cel::expr::decl::DeclKind;
use cel_core_proto::gen::cel::expr::r#type::TypeKind;
use cel_core_proto::gen::cel::expr::Type as ProtoType;
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

            // Skip if disable_check is true
            if test.disable_check {
                continue;
            }

            // Only process tests with typed_result
            let expected_type = match &test.result_matcher {
                Some(ResultMatcher::TypedResult(tr)) => match &tr.deduced_type {
                    Some(ty) => ty,
                    None => continue, // No deduced_type to check
                },
                _ => continue, // Not a typed_result test
            };

            // Skip tests with function declarations (not yet supported)
            let has_function_decl = test.type_env.iter().any(|d| {
                matches!(&d.decl_kind, Some(DeclKind::Function(_)))
            });
            if has_function_decl {
                continue;
            }

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
