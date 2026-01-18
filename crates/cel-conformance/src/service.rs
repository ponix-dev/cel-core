//! CelConformanceService implementation.
//!
//! This module provides the concrete implementation of ConformanceService
//! using the cel-parser crate.

use crate::{
    Binding, CheckResponse, ConformanceService, EvalResponse, Issue, ParseResponse, TypeDecl,
};

/// CEL conformance service implementation using cel-parser.
///
/// Currently supports:
/// - Parse: Full support via cel-parser
/// - Check: Stub (returns unimplemented error)
/// - Eval: Stub (returns unimplemented error)
#[derive(Debug, Default)]
pub struct CelConformanceService;

impl CelConformanceService {
    pub fn new() -> Self {
        Self
    }
}

impl ConformanceService for CelConformanceService {
    fn parse(&self, source: &str) -> ParseResponse {
        let result = cel_parser::parse(source);

        // Convert parse errors to issues
        let issues: Vec<Issue> = result
            .errors
            .iter()
            .map(|e| {
                // TODO: Convert byte offset to line/column if needed
                Issue::error(&e.message)
            })
            .collect();

        // Convert AST to ParsedExpr using cel-proto
        let parsed_expr = result
            .ast
            .map(|ast| cel_proto::to_parsed_expr(&ast, source));

        ParseResponse { parsed_expr, issues }
    }

    fn check(
        &self,
        _parsed: &google_cel_spec_community_neoeinstein_prost::cel::expr::ParsedExpr,
        _type_env: &[TypeDecl],
    ) -> CheckResponse {
        CheckResponse {
            checked_expr: None,
            issues: vec![Issue::unimplemented("type checking")],
        }
    }

    fn eval(
        &self,
        _expr: &google_cel_spec_community_neoeinstein_prost::cel::expr::ParsedExpr,
        _bindings: &[Binding],
    ) -> EvalResponse {
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
    fn test_check_unimplemented() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("x").parsed_expr.unwrap();
        let check_result = service.check(&parse_result, &[]);
        assert!(!check_result.is_ok());
        assert!(check_result
            .issues
            .iter()
            .any(|i| i.message.contains("not yet implemented")));
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
