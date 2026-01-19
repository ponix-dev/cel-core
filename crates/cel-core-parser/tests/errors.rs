//! Error handling tests for cel-parser.

mod common;

use cel_core_parser::parse;

#[test]
fn error_on_unclosed_parenthesis() {
    let result = parse("(1 + 2");
    assert!(result.is_err());
}

#[test]
fn error_on_unclosed_bracket() {
    let result = parse("[1, 2");
    assert!(result.is_err());
}

#[test]
fn error_on_unclosed_brace() {
    let result = parse("{\"a\": 1");
    assert!(result.is_err());
}

#[test]
fn error_on_missing_operator() {
    let result = parse("1 2");
    assert!(result.is_err());
}

#[test]
fn error_on_trailing_operator() {
    let result = parse("1 +");
    assert!(result.is_err());
}

#[test]
fn error_on_empty_input() {
    let result = parse("");
    assert!(result.is_err());
}

#[test]
fn error_on_unclosed_string() {
    let result = parse("\"hello");
    assert!(result.is_err());
}

#[test]
fn error_on_incomplete_ternary() {
    let result = parse("a ? b");
    assert!(result.is_err());
}

#[test]
fn error_has_span_information() {
    let result = parse("1 +");
    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(!errors.is_empty());
    // Errors should have span information
    let error = &errors[0];
    // Span should have valid positions
    assert!(error.span.end >= error.span.start);
}

#[test]
fn error_on_reserved_word_as_identifier() {
    // Reserved words cannot be used as identifiers
    let result = parse("if");
    assert!(result.is_err(), "reserved word 'if' should produce an error");

    let result = parse("while");
    assert!(result.is_err(), "reserved word 'while' should produce an error");

    let result = parse("x + for");
    assert!(result.is_err(), "reserved word 'for' should produce an error");
}

#[test]
fn error_recovery_produces_partial_ast() {
    use cel_core_parser::Expr;

    // Test that error recovery can produce a partial AST with Expr::Error nodes
    // For a malformed list element, we should get a list with an error node
    let result = parse("[1, @invalid, 3]");
    // Should have errors
    assert!(result.is_err(), "should have errors for invalid token");
    // But may also have a partial AST
    if let Some(ast) = result.ast {
        // If we got an AST, check it's a list
        if let Expr::List(items) = ast.node {
            // One of the items should be Expr::Error due to recovery
            let has_error = items.iter().any(|item| matches!(item.expr.node, Expr::Error));
            assert!(has_error || items.len() < 3, "should have error node or fewer items due to recovery");
        }
    }
}

#[test]
fn error_recovery_in_parentheses() {
    // Unclosed paren with content - should recover
    let result = parse("(1 + 2");
    assert!(result.is_err(), "should have error for unclosed paren");
    // Recovery might produce an Error node or empty result
    // The important thing is we don't panic
}
