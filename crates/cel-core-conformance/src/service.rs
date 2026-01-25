//! CelConformanceService implementation.
//!
//! This module provides the concrete implementation of ConformanceService
//! using the cel-core unified Env.

use crate::{
    Binding, CheckResponse, ConformanceService, EvalResponse, Issue, ParseResponse, TypeDecl,
};
use cel_core::Env;
use cel_core_proto::gen::cel::expr::{CheckedExpr, ParsedExpr, Reference};
use cel_core_proto::{
    cel_type_from_proto, cel_type_to_proto, cel_value_to_proto, from_parsed_expr,
};
use cel_core_common::CelType;
use std::collections::HashMap;

/// CEL conformance service implementation using cel-core Env.
///
/// All operations go through the unified Env API:
/// - Parse: Uses Env::parse()
/// - Check: Uses Env::check()
/// - Eval: Stub (returns unimplemented error)
#[derive(Debug)]
pub struct CelConformanceService {
    /// Base environment with standard library.
    /// Extended per-request with type declarations.
    env: Env,
}

impl CelConformanceService {
    pub fn new() -> Self {
        Self {
            env: Env::with_standard_library(),
        }
    }
}

impl Default for CelConformanceService {
    fn default() -> Self {
        Self::new()
    }
}

impl ConformanceService for CelConformanceService {
    fn parse(&self, source: &str) -> ParseResponse {
        // Use env's parser
        let result = self.env.parse(source);

        // Convert parse errors to issues
        let issues: Vec<Issue> = result
            .errors
            .iter()
            .map(|e| {
                // TODO: Convert byte offset to line/column if needed
                Issue::error(&e.message)
            })
            .collect();

        // Convert AST to ParsedExpr using cel-proto (with macro_calls for IDE features)
        let parsed_expr = result.ast.map(|ast| {
            cel_core_proto::to_parsed_expr_with_macros(&ast, source, &result.macro_calls)
        });

        ParseResponse { parsed_expr, issues }
    }

    fn check(&self, parsed: &ParsedExpr, type_env: &[TypeDecl]) -> CheckResponse {
        // Convert ParsedExpr back to AST
        let ast = match from_parsed_expr(parsed) {
            Ok(ast) => ast,
            Err(e) => {
                return CheckResponse {
                    checked_expr: None,
                    issues: vec![Issue::error(format!("Failed to convert ParsedExpr: {}", e))],
                };
            }
        };

        // Clone base env and add type declarations
        let mut env = self.env.clone();

        for decl in type_env {
            let cel_type = cel_type_from_proto(&decl.cel_type);
            env.add_variable(&decl.name, cel_type);
        }

        // Run the type checker using env
        let check_result = env.check(&ast);

        // Convert errors to issues
        let mut issues: Vec<Issue> = check_result
            .errors
            .iter()
            .map(|e| Issue::error(e.message()))
            .collect();

        // Build CheckedExpr
        let checked_expr = if check_result.is_ok() || !check_result.type_map.is_empty() {
            // Build type_map: convert CelType to proto Type
            let type_map: HashMap<i64, cel_core_proto::Type> = check_result
                .type_map
                .iter()
                .filter(|(_, ty)| !matches!(ty, CelType::Dyn)) // Omit Dyn types
                .map(|(id, ty)| (*id, cel_type_to_proto(ty)))
                .collect();

            // Build reference_map
            let reference_map: HashMap<i64, Reference> = check_result
                .reference_map
                .iter()
                .map(|(id, info)| {
                    (
                        *id,
                        Reference {
                            name: info.name.clone(),
                            overload_id: info.overload_ids.clone(),
                            value: info.value.as_ref().map(cel_value_to_proto),
                        },
                    )
                })
                .collect();

            Some(CheckedExpr {
                reference_map,
                type_map,
                source_info: parsed.source_info.clone(),
                expr_version: String::new(),
                expr: parsed.expr.clone(),
            })
        } else {
            None
        };

        // If we have errors but also a partial result, still return the checked_expr
        if !issues.is_empty() && checked_expr.is_none() {
            issues.push(Issue::error("Type checking failed"));
        }

        CheckResponse {
            checked_expr,
            issues,
        }
    }

    fn eval(&self, _expr: &ParsedExpr, _bindings: &[Binding]) -> EvalResponse {
        EvalResponse {
            result: None,
            issues: vec![Issue::unimplemented("evaluation")],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_int() {
        let service = CelConformanceService::new();
        let result = service.parse("42");
        assert!(result.is_ok());
        assert!(result.parsed_expr.is_some());
    }

    #[test]
    fn test_parse_binary_op() {
        let service = CelConformanceService::new();
        let result = service.parse("1 + 2");
        assert!(result.is_ok());
        assert!(result.parsed_expr.is_some());
    }

    #[test]
    fn test_parse_with_error() {
        let service = CelConformanceService::new();
        let result = service.parse("1 +");
        assert!(!result.issues.is_empty());
    }

    #[test]
    fn test_check_literal() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("42").parsed_expr.unwrap();
        let check_result = service.check(&parse_result, &[]);
        assert!(check_result.is_ok());
        assert!(check_result.checked_expr.is_some());

        let checked = check_result.checked_expr.unwrap();
        assert!(!checked.type_map.is_empty());
    }

    #[test]
    fn test_check_with_variable() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("x + 1").parsed_expr.unwrap();

        // Provide type declaration for x
        let type_decl = TypeDecl {
            name: "x".to_string(),
            cel_type: cel_type_to_proto(&CelType::Int),
        };

        let check_result = service.check(&parse_result, &[type_decl]);
        assert!(check_result.is_ok());
        assert!(check_result.checked_expr.is_some());
    }

    #[test]
    fn test_check_undefined_variable() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("x").parsed_expr.unwrap();
        let check_result = service.check(&parse_result, &[]);
        assert!(!check_result.is_ok());
        assert!(check_result
            .issues
            .iter()
            .any(|i| i.message.contains("undeclared")));
    }

    #[test]
    fn test_check_type_mismatch() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("x + \"str\"").parsed_expr.unwrap();

        let type_decl = TypeDecl {
            name: "x".to_string(),
            cel_type: cel_type_to_proto(&CelType::Int),
        };

        let check_result = service.check(&parse_result, &[type_decl]);
        assert!(!check_result.is_ok());
        assert!(check_result
            .issues
            .iter()
            .any(|i| i.message.contains("no matching overload")));
    }

    #[test]
    fn test_eval_unimplemented() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("x").parsed_expr.unwrap();
        let eval_result = service.eval(&parse_result, &[]);
        assert!(!eval_result.is_ok());
        assert!(eval_result
            .issues
            .iter()
            .any(|i| i.message.contains("not yet implemented")));
    }
}
