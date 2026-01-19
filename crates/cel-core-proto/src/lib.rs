//! Bidirectional conversion between cel-parser AST and google/cel-spec protobuf format.
//!
//! This crate enables wire compatibility with other CEL implementations (cel-go, cel-cpp, etc.)
//! by providing conversion between the internal `cel-parser` AST representation and the official
//! google/cel-spec protobuf format.
//!
//! # Example
//!
//! ```
//! use cel_core_parser::SpannedExpr;
//! use cel_core_proto::{to_parsed_expr, from_parsed_expr};
//!
//! // Parse a CEL expression
//! let result = cel_core_parser::parse("x + 1");
//! let ast = result.ast.unwrap();
//!
//! // Convert to proto format
//! let source = "x + 1";
//! let parsed_expr = to_parsed_expr(&ast, source);
//!
//! // Convert back to AST
//! let roundtripped = from_parsed_expr(&parsed_expr).unwrap();
//! ```

mod converter;
mod error;
pub mod gen;
mod operators;
mod source_info;

pub use converter::AstConverter;
pub use error::ConversionError;
pub use operators::{
    binary_op_to_function, function_to_binary_op, function_to_unary_op, is_index_function,
    is_ternary_function, unary_op_to_function, INDEX_FUNCTION, TERNARY_FUNCTION,
};
pub use source_info::{build_source_info, compute_line_offsets, get_position};

// Re-export proto types for convenience
pub use gen::cel::expr::{CheckedExpr, Constant, Expr, ParsedExpr, SourceInfo, Type, Value};

use cel_core_parser::{MacroCalls, SpannedExpr};

/// Convert a cel-parser AST to a proto ParsedExpr.
///
/// This is a convenience function that creates an `AstConverter`, converts the AST,
/// and builds the final `ParsedExpr` with source info.
///
/// # Arguments
///
/// * `ast` - The parsed expression AST from cel-parser
/// * `source` - The original source text (used for computing line offsets)
///
/// # Returns
///
/// A `ParsedExpr` containing the converted expression and source info.
pub fn to_parsed_expr(ast: &SpannedExpr, source: &str) -> ParsedExpr {
    let mut converter = AstConverter::new(source);
    let expr = converter.ast_to_expr(ast);
    converter.into_parsed_expr(expr)
}

/// Convert a cel-parser AST to a proto ParsedExpr, including macro call information.
///
/// This is similar to `to_parsed_expr`, but also includes the `macro_calls` map
/// in the `SourceInfo` which records the original call expressions before macro expansion.
/// This is useful for IDE features that need to show the original source form.
///
/// # Arguments
///
/// * `ast` - The expanded AST (after macro expansion)
/// * `source` - The original source text
/// * `macro_calls` - Map of comprehension IDs to original call expressions
///
/// # Returns
///
/// A `ParsedExpr` containing the converted expression and source info with macro_calls.
pub fn to_parsed_expr_with_macros(
    ast: &SpannedExpr,
    source: &str,
    macro_calls: &MacroCalls,
) -> ParsedExpr {
    let mut converter = AstConverter::new(source);
    let expr = converter.ast_to_expr(ast);

    // Convert macro_calls from parser AST format to proto format
    let proto_macro_calls: std::collections::HashMap<i64, gen::cel::expr::Expr> = macro_calls
        .iter()
        .map(|(id, call_expr)| {
            let mut call_converter = AstConverter::new(source);
            (*id, call_converter.ast_to_expr(call_expr))
        })
        .collect();

    converter.into_parsed_expr_with_macros(expr, proto_macro_calls)
}

/// Convert a proto ParsedExpr back to a cel-parser AST.
///
/// Note: Proto only stores start offsets, so the resulting spans will be zero-length
/// at the recorded positions.
///
/// # Arguments
///
/// * `parsed` - The proto ParsedExpr to convert
///
/// # Returns
///
/// The converted AST, or a `ConversionError` if the proto is malformed or contains
/// unsupported constructs (like comprehensions).
pub fn from_parsed_expr(parsed: &ParsedExpr) -> Result<SpannedExpr, ConversionError> {
    let expr = parsed.expr.as_ref().ok_or(ConversionError::MissingField {
        expr_id: 0,
        field: "expr",
    })?;

    let source_info = parsed
        .source_info
        .as_ref()
        .ok_or(ConversionError::MissingField {
            expr_id: 0,
            field: "source_info",
        })?;

    let converter = AstConverter::new("");
    converter.expr_to_ast(expr, source_info)
}

#[cfg(test)]
mod tests {
    use super::*;
    use cel_core_parser::Expr;

    #[test]
    fn test_to_parsed_expr() {
        let source = "x + 1";
        let result = cel_core_parser::parse(source);
        let ast = result.ast.unwrap();

        let parsed = to_parsed_expr(&ast, source);

        assert!(parsed.expr.is_some());
        assert!(parsed.source_info.is_some());
    }

    #[test]
    fn test_from_parsed_expr() {
        let source = "x + 1";
        let result = cel_core_parser::parse(source);
        let ast = result.ast.unwrap();

        let parsed = to_parsed_expr(&ast, source);
        let roundtripped = from_parsed_expr(&parsed).unwrap();

        // Check the structure matches
        match &roundtripped.node {
            Expr::Binary { op, left, right } => {
                assert_eq!(*op, cel_core_parser::BinaryOp::Add);
                assert!(matches!(left.node, Expr::Ident(ref s) if s == "x"));
                assert!(matches!(right.node, Expr::Int(1)));
            }
            _ => panic!("expected binary expression"),
        }
    }

    #[test]
    fn test_roundtrip_complex() {
        let source = "a > 0 && b.contains('x') ? [1, 2, 3] : {'key': value}";
        let result = cel_core_parser::parse(source);
        let ast = result.ast.unwrap();

        let parsed = to_parsed_expr(&ast, source);
        let roundtripped = from_parsed_expr(&parsed).unwrap();

        // Verify it's a ternary at the top level
        assert!(matches!(roundtripped.node, Expr::Ternary { .. }));
    }

    #[test]
    fn test_from_parsed_expr_missing_expr() {
        let parsed = ParsedExpr {
            expr: None,
            source_info: Some(SourceInfo::default()),
        };

        let result = from_parsed_expr(&parsed);
        assert!(matches!(
            result,
            Err(ConversionError::MissingField { field: "expr", .. })
        ));
    }

    #[test]
    fn test_from_parsed_expr_missing_source_info() {
        let parsed = ParsedExpr {
            expr: Some(gen::cel::expr::Expr::default()),
            source_info: None,
        };

        let result = from_parsed_expr(&parsed);
        assert!(matches!(
            result,
            Err(ConversionError::MissingField {
                field: "source_info",
                ..
            })
        ));
    }
}
