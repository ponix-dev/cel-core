//! CEL conformance integration tests.
//!
//! These tests run the cel-parser against official CEL conformance test files
//! from the cel-spec repository. Each test expression that fails to parse
//! will cause a test failure.

use cel_core_conformance::{load_test_file, CelConformanceService, ConformanceService};
use std::path::Path;

const TESTDATA_PATH: &str = "cel-spec/tests/simple/testdata";

/// Run all tests in a file and collect failures
fn run_conformance_file(filename: &str) -> Vec<String> {
    let path = Path::new(TESTDATA_PATH).join(filename);
    let file = load_test_file(&path).expect(&format!("Failed to load {}", filename));

    let service = CelConformanceService::new();
    let mut failures = Vec::new();

    for section in &file.section {
        for test in &section.test {
            let test_name = format!("{}/{}", section.name, test.name);
            let parse_result = service.parse(&test.expr);

            if !parse_result.is_ok() {
                let error_msg = parse_result
                    .issues
                    .iter()
                    .map(|i| i.message.clone())
                    .collect::<Vec<_>>()
                    .join("; ");
                failures.push(format!("{}: {}", test_name, error_msg));
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
