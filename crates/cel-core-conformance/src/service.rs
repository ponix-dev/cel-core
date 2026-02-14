//! CelConformanceService implementation.
//!
//! This module provides the concrete implementation of ConformanceService
//! using the cel-core unified Env.

use std::sync::Arc;

use crate::{
    Binding, CheckResponse, ConformanceService, EvalResponse, FunctionTypeDecl, Issue,
    ParseResponse, TypeDecl,
};
use cel_core::{Env, MapActivation};
use cel_core_proto::gen::cel::expr::ParsedExpr;
use cel_core_proto::{
    cel_type_from_proto, from_parsed_expr, function_decl_from_proto, proto_value_to_value,
    to_checked_expr, value_to_expr_value, ProstProtoRegistry,
};

#[cfg(test)]
use cel_core::CelType;
#[cfg(test)]
use cel_core_proto::cel_type_to_proto;
#[cfg(test)]
use cel_core_proto::gen::cel::expr::expr_value;
#[cfg(test)]
use cel_core_proto::gen::cel::expr::value::Kind as ProtoValueKind;

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
        Self::with_strong_enums(true)
    }

    /// Create a conformance service with explicit strong enum setting.
    ///
    /// When `strong` is true (default), enum values carry typed information.
    /// When false, enum values are returned as plain integers (legacy behavior).
    pub fn with_strong_enums(strong: bool) -> Self {
        // Create proto type registry with conformance test proto descriptors
        let mut registry = ProstProtoRegistry::new();

        // Add conformance test proto descriptors
        // Order matters: dependencies must be added before dependents
        registry
            .add_file_descriptor_set(cel_core_proto::gen::cel::expr::FILE_DESCRIPTOR_SET)
            .expect("Failed to add cel.expr descriptors");
        registry
            .add_file_descriptor_set(
                cel_core_proto::gen::cel::expr::conformance::FILE_DESCRIPTOR_SET,
            )
            .expect("Failed to add cel.expr.conformance descriptors");
        registry
            .add_file_descriptor_set(
                cel_core_proto::gen::cel::expr::conformance::proto2::FILE_DESCRIPTOR_SET,
            )
            .expect("Failed to add cel.expr.conformance.proto2 descriptors");
        registry
            .add_file_descriptor_set(
                cel_core_proto::gen::cel::expr::conformance::proto3::FILE_DESCRIPTOR_SET,
            )
            .expect("Failed to add cel.expr.conformance.proto3 descriptors");
        registry
            .add_file_descriptor_set(
                cel_core_proto::gen::cel::expr::conformance::test::FILE_DESCRIPTOR_SET,
            )
            .expect("Failed to add cel.expr.conformance.test descriptors");

        let registry = Arc::new(registry);
        let mut env = Env::with_standard_library()
            .with_all_extensions()
            .with_proto_registry(registry as Arc<dyn cel_core::ProtoRegistry>);
        if !strong {
            env = env.with_legacy_enums();
        }

        Self { env }
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
        // cel_core_proto now uses cel_core types directly
        let parsed_expr = result.ast.map(|ast| {
            cel_core_proto::to_parsed_expr_with_macros(&ast, source, &result.macro_calls)
        });

        ParseResponse {
            parsed_expr,
            issues,
        }
    }

    fn check(
        &self,
        parsed: &ParsedExpr,
        type_env: &[TypeDecl],
        func_decls: &[FunctionTypeDecl],
        container: &str,
    ) -> CheckResponse {
        // Convert ParsedExpr back to AST
        // cel_core_proto::from_parsed_expr returns cel_core::SpannedExpr directly now
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

        // Set the container for qualified name resolution
        env.set_container(container);

        for decl in type_env {
            // cel_type_from_proto now returns cel_core::CelType directly
            let cel_type = cel_type_from_proto(&decl.cel_type);
            env.add_variable(&decl.name, cel_type);
        }

        // Add custom function declarations
        for func_decl in func_decls {
            env.add_function(function_decl_from_proto(
                &func_decl.name,
                &func_decl.overloads,
            ));
        }

        // Run the type checker using env
        let check_result = env.check(&ast);

        // Convert errors to issues
        let mut issues: Vec<Issue> = check_result
            .errors
            .iter()
            .map(|e| Issue::error(e.message()))
            .collect();

        // Build CheckedExpr using the helper from cel-core-proto
        // cel_core_proto::to_checked_expr now takes cel_core::CheckResult directly
        let checked_expr = if check_result.is_ok() || !check_result.type_map.is_empty() {
            Some(to_checked_expr(&check_result, parsed))
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

    fn eval(
        &self,
        expr: &ParsedExpr,
        bindings: &[Binding],
        type_env: &[TypeDecl],
        func_decls: &[FunctionTypeDecl],
        container: &str,
    ) -> EvalResponse {
        // Convert ParsedExpr to AST
        let parsed_ast = match from_parsed_expr(expr) {
            Ok(ast) => ast,
            Err(e) => {
                return EvalResponse {
                    result: None,
                    issues: vec![Issue::error(format!("Failed to convert ParsedExpr: {}", e))],
                };
            }
        };

        // Get proto registry for value conversion
        let proto_registry = match self.env.proto_registry() {
            Some(registry) => registry,
            None => {
                return EvalResponse {
                    result: None,
                    issues: vec![Issue::error("proto registry not available")],
                };
            }
        };
        let proto_types = match proto_registry.as_any().downcast_ref::<ProstProtoRegistry>() {
            Some(r) => r,
            None => {
                return EvalResponse {
                    result: None,
                    issues: vec![Issue::error("proto registry is not ProstProtoRegistry")],
                };
            }
        };

        // Convert bindings to activation
        let activation = match bindings_to_activation(bindings, proto_types) {
            Ok(act) => act,
            Err(e) => {
                return EvalResponse {
                    result: None,
                    issues: vec![Issue::error(e)],
                };
            }
        };

        // Clone env and set container + type declarations for proper name resolution
        let mut env = self.env.clone();
        if !container.is_empty() {
            env.set_container(container);
        }
        for decl in type_env {
            let cel_type = cel_type_from_proto(&decl.cel_type);
            env.add_variable(&decl.name, cel_type);
        }
        for func_decl in func_decls {
            env.add_function(function_decl_from_proto(
                &func_decl.name,
                &func_decl.overloads,
            ));
        }

        // Run the checker to get type info (needed for proto message construction)
        // We use the check result even if there are errors, as we still get useful
        // reference_map entries for type resolution
        let check_result = env.check(&parsed_ast);
        let checked_ast = cel_core::Ast::new_checked(parsed_ast, "", check_result);

        match env.program(&checked_ast) {
            Ok(program) => {
                let result = program.eval_with_container(&activation, container);
                let expr_value = value_to_expr_value(&result);
                EvalResponse {
                    result: Some(expr_value),
                    issues: vec![],
                }
            }
            Err(e) => EvalResponse {
                result: None,
                issues: vec![Issue::error(format!("Failed to create program: {}", e))],
            },
        }
    }
}

/// Convert bindings to a MapActivation.
fn bindings_to_activation(
    bindings: &[Binding],
    registry: &ProstProtoRegistry,
) -> Result<MapActivation, String> {
    let mut activation = MapActivation::new();
    for binding in bindings {
        let value = proto_value_to_value(&binding.value, registry)?;
        activation.insert(&binding.name, value);
    }
    Ok(activation)
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
        let check_result = service.check(&parse_result, &[], &[], "");
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

        let check_result = service.check(&parse_result, &[type_decl], &[], "");
        assert!(check_result.is_ok());
        assert!(check_result.checked_expr.is_some());
    }

    #[test]
    fn test_check_undefined_variable() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("x").parsed_expr.unwrap();
        let check_result = service.check(&parse_result, &[], &[], "");
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

        let check_result = service.check(&parse_result, &[type_decl], &[], "");
        assert!(!check_result.is_ok());
        assert!(check_result
            .issues
            .iter()
            .any(|i| i.message.contains("no matching overload")));
    }

    #[test]
    fn test_eval_literal() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("42").parsed_expr.unwrap();
        let eval_result = service.eval(&parse_result, &[], &[], &[], "");
        assert!(
            eval_result.is_ok(),
            "eval should succeed: {:?}",
            eval_result.issues
        );
        assert!(eval_result.result.is_some());

        let result = eval_result.result.unwrap();
        match &result.kind {
            Some(expr_value::Kind::Value(v)) => match &v.kind {
                Some(ProtoValueKind::Int64Value(42)) => {}
                other => panic!("expected Int64Value(42), got {:?}", other),
            },
            other => panic!("expected Value, got {:?}", other),
        }
    }

    #[test]
    fn test_eval_with_variable() {
        use cel_core_proto::gen::cel::expr::value::Kind as ValueKind;

        let service = CelConformanceService::new();
        let parse_result = service.parse("x + 1").parsed_expr.unwrap();

        // Create a binding for x = 41
        let binding = Binding {
            name: "x".to_string(),
            value: cel_core_proto::gen::cel::expr::Value {
                kind: Some(ValueKind::Int64Value(41)),
            },
        };

        let eval_result = service.eval(&parse_result, &[binding], &[], &[], "");
        assert!(
            eval_result.is_ok(),
            "eval should succeed: {:?}",
            eval_result.issues
        );

        let result = eval_result.result.unwrap();
        match &result.kind {
            Some(expr_value::Kind::Value(v)) => match &v.kind {
                Some(ProtoValueKind::Int64Value(42)) => {}
                other => panic!("expected Int64Value(42), got {:?}", other),
            },
            other => panic!("expected Value, got {:?}", other),
        }
    }

    #[test]
    fn test_eval_unknown_variable() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("unknown_var").parsed_expr.unwrap();
        let eval_result = service.eval(&parse_result, &[], &[], &[], "");

        // Should return an error result
        let result = eval_result.result.unwrap();
        match &result.kind {
            Some(expr_value::Kind::Error(_)) => {}
            other => panic!("expected Error, got {:?}", other),
        }
    }
}
