//! Common test utilities for cel-parser integration tests.

use cel_parser::{parse, ParseError, SpannedExpr};

/// Parse input and assert it succeeds, returning the AST.
#[allow(dead_code)]
pub fn assert_parses(input: &str) -> SpannedExpr {
    let result = parse(input);
    if !result.errors.is_empty() {
        panic!(
            "failed to parse '{}': {:?}",
            input,
            result
                .errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
        );
    }
    result.ast.expect("expected AST")
}

/// Parse input and assert it fails, returning the errors.
#[allow(dead_code)]
pub fn assert_parse_error(input: &str) -> Vec<ParseError> {
    let result = parse(input);
    if result.errors.is_empty() {
        panic!("expected parse error for '{}', but got: {:?}", input, result.ast);
    }
    result.errors
}
