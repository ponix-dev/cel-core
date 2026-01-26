//! Conversion between CheckResult and proto CheckedExpr.
//!
//! This module provides functions to convert between the internal type checker
//! result (`CheckResult`) and the proto wire format (`CheckedExpr`).

use std::collections::HashMap;

use cel_core_checker::{CheckResult, ReferenceInfo};
use cel_core_common::{CelType, SpannedExpr};

use crate::error::ConversionError;
use crate::gen::cel::expr::{CheckedExpr, ParsedExpr, Reference};
use crate::type_conversion::{cel_type_from_proto, cel_type_to_proto, cel_value_from_proto, cel_value_to_proto};

/// Convert a check result and parsed expression to a proto CheckedExpr.
///
/// This combines the type checking results with the parsed expression to produce
/// a `CheckedExpr` that can be serialized and sent over the wire.
///
/// # Arguments
///
/// * `check_result` - The result from type checking
/// * `parsed_expr` - The proto ParsedExpr (provides expr and source_info)
///
/// # Returns
///
/// A `CheckedExpr` containing the type map, reference map, and expression.
///
/// # Example
///
/// ```
/// use cel_core_proto::{to_parsed_expr, to_checked_expr};
/// use cel_core_checker::{check, STANDARD_LIBRARY};
/// use cel_core_common::CelType;
/// use std::collections::HashMap;
///
/// let source = "x + 1";
/// let ast = cel_core_parser::parse(source).ast.unwrap();
/// let parsed = to_parsed_expr(&ast, source);
///
/// let mut variables = HashMap::new();
/// variables.insert("x".to_string(), CelType::Int);
/// let functions: HashMap<_, _> = STANDARD_LIBRARY.iter()
///     .map(|f| (f.name.clone(), f.clone()))
///     .collect();
///
/// let check_result = check(&ast, &variables, &functions, "");
/// let checked = to_checked_expr(&check_result, &parsed);
///
/// assert!(!checked.type_map.is_empty());
/// ```
pub fn to_checked_expr(check_result: &CheckResult, parsed_expr: &ParsedExpr) -> CheckedExpr {
    // Convert type_map: CelType -> proto Type
    // Omit Dyn types as per CEL spec (they're implicit)
    let type_map: HashMap<i64, crate::gen::cel::expr::Type> = check_result
        .type_map
        .iter()
        .filter(|(_, ty)| !matches!(ty, CelType::Dyn))
        .map(|(id, ty)| (*id, cel_type_to_proto(ty)))
        .collect();

    // Convert reference_map: ReferenceInfo -> proto Reference
    let reference_map: HashMap<i64, Reference> = check_result
        .reference_map
        .iter()
        .map(|(id, info)| (*id, reference_info_to_proto(info)))
        .collect();

    CheckedExpr {
        reference_map,
        type_map,
        source_info: parsed_expr.source_info.clone(),
        expr_version: String::new(),
        expr: parsed_expr.expr.clone(),
    }
}

/// Convert a check result and AST to a proto CheckedExpr.
///
/// This is a convenience function that first converts the AST to a ParsedExpr,
/// then builds the CheckedExpr. Use this when you have the internal AST
/// rather than an already-converted ParsedExpr.
///
/// # Arguments
///
/// * `check_result` - The result from type checking
/// * `ast` - The internal AST representation
/// * `source` - The original source text
///
/// # Returns
///
/// A `CheckedExpr` containing the type map, reference map, and expression.
///
/// # Example
///
/// ```
/// use cel_core_proto::to_checked_expr_from_ast;
/// use cel_core_checker::{check, STANDARD_LIBRARY};
/// use cel_core_common::CelType;
/// use std::collections::HashMap;
///
/// let source = "x + 1";
/// let ast = cel_core_parser::parse(source).ast.unwrap();
///
/// let mut variables = HashMap::new();
/// variables.insert("x".to_string(), CelType::Int);
/// let functions: HashMap<_, _> = STANDARD_LIBRARY.iter()
///     .map(|f| (f.name.clone(), f.clone()))
///     .collect();
///
/// let check_result = check(&ast, &variables, &functions, "");
/// let checked = to_checked_expr_from_ast(&check_result, &ast, source);
///
/// assert!(!checked.type_map.is_empty());
/// ```
pub fn to_checked_expr_from_ast(
    check_result: &CheckResult,
    ast: &SpannedExpr,
    source: &str,
) -> CheckedExpr {
    let parsed_expr = crate::to_parsed_expr(ast, source);
    to_checked_expr(check_result, &parsed_expr)
}

/// Convert internal ReferenceInfo to proto Reference.
fn reference_info_to_proto(info: &ReferenceInfo) -> Reference {
    Reference {
        name: info.name.clone(),
        overload_id: info.overload_ids.clone(),
        value: info.value.as_ref().map(cel_value_to_proto),
    }
}

/// Reconstruct CheckResult from proto CheckedExpr.
///
/// This enables round-tripping: AST -> CheckedExpr -> AST.
///
/// # Arguments
///
/// * `checked` - The proto CheckedExpr to convert
///
/// # Returns
///
/// A `CheckResult` containing the type map and reference map, or an error
/// if the proto is malformed.
///
/// # Note
///
/// The proto format does not store error information, so `errors` will always be empty.
pub fn check_result_from_proto(checked: &CheckedExpr) -> Result<CheckResult, ConversionError> {
    // Convert type_map: proto Type -> CelType
    let type_map: HashMap<i64, CelType> = checked
        .type_map
        .iter()
        .map(|(id, ty)| (*id, cel_type_from_proto(ty)))
        .collect();

    // Convert reference_map: proto Reference -> ReferenceInfo
    let reference_map: HashMap<i64, ReferenceInfo> = checked
        .reference_map
        .iter()
        .map(|(id, r)| (*id, reference_info_from_proto(r)))
        .collect();

    Ok(CheckResult {
        type_map,
        reference_map,
        errors: vec![], // Proto doesn't store errors
    })
}

/// Convert proto Reference to internal ReferenceInfo.
fn reference_info_from_proto(r: &Reference) -> ReferenceInfo {
    ReferenceInfo {
        name: r.name.clone(),
        overload_ids: r.overload_id.clone(),
        value: r.value.as_ref().and_then(cel_value_from_proto),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cel_core_checker::{check, STANDARD_LIBRARY};

    fn standard_functions() -> HashMap<String, cel_core_common::FunctionDecl> {
        STANDARD_LIBRARY
            .iter()
            .map(|f| (f.name.clone(), f.clone()))
            .collect()
    }

    #[test]
    fn test_to_checked_expr_simple() {
        let source = "1 + 2";
        let ast = cel_core_parser::parse(source).ast.unwrap();
        let parsed = crate::to_parsed_expr(&ast, source);

        let variables = HashMap::new();
        let functions = standard_functions();
        let check_result = check(&ast, &variables, &functions, "");

        let checked = to_checked_expr(&check_result, &parsed);

        // Should have type entries for literals and the binary op
        assert!(!checked.type_map.is_empty());
        // Should have reference for the + operator
        assert!(!checked.reference_map.is_empty());
        // Should preserve the expression
        assert!(checked.expr.is_some());
        // Should preserve source info
        assert!(checked.source_info.is_some());
    }

    #[test]
    fn test_to_checked_expr_with_variable() {
        let source = "x + 1";
        let ast = cel_core_parser::parse(source).ast.unwrap();
        let parsed = crate::to_parsed_expr(&ast, source);

        let mut variables = HashMap::new();
        variables.insert("x".to_string(), CelType::Int);
        let functions = standard_functions();
        let check_result = check(&ast, &variables, &functions, "");

        let checked = to_checked_expr(&check_result, &parsed);

        assert!(check_result.is_ok());
        assert!(!checked.type_map.is_empty());
        // Should have reference for x and for +
        assert!(checked.reference_map.len() >= 2);
    }

    #[test]
    fn test_to_checked_expr_from_ast() {
        let source = "true && false";
        let ast = cel_core_parser::parse(source).ast.unwrap();

        let variables = HashMap::new();
        let functions = standard_functions();
        let check_result = check(&ast, &variables, &functions, "");

        let checked = to_checked_expr_from_ast(&check_result, &ast, source);

        assert!(!checked.type_map.is_empty());
        assert!(checked.expr.is_some());
    }

    #[test]
    fn test_dyn_types_omitted() {
        // When a type is Dyn, it should be omitted from the type_map
        let source = "x";
        let ast = cel_core_parser::parse(source).ast.unwrap();
        let parsed = crate::to_parsed_expr(&ast, source);

        // Don't declare x - it will have type Error or Dyn
        let variables = HashMap::new();
        let functions = standard_functions();
        let check_result = check(&ast, &variables, &functions, "");

        let checked = to_checked_expr(&check_result, &parsed);

        // The error type might still be in the map, but Dyn should be omitted
        // This test mainly verifies the filter logic runs without panic
        assert!(checked.expr.is_some());
    }
}
