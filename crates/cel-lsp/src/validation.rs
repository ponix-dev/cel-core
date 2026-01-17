//! Semantic validation for CEL expressions.
//!
//! This module provides validation for undefined variables and methods.
//! It uses a `VariableResolver` trait to allow custom variable definitions.

use cel_parser::{Expr, Span, SpannedExpr};
use cel_types::is_builtin;

/// Category of validation error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidationErrorKind {
    UndefinedVariable,
    UndefinedMethod,
}

/// A validation error with source location.
#[derive(Debug, Clone)]
pub struct ValidationError {
    pub kind: ValidationErrorKind,
    pub message: String,
    pub span: Span,
    /// The name that was undefined (for hover lookup).
    pub name: String,
}

/// Trait for resolving variable names.
///
/// Implement this trait to provide custom variable definitions.
/// The default implementation rejects all variables.
pub trait VariableResolver {
    /// Check if a variable name is defined.
    fn is_defined(&self, name: &str) -> bool;
}

/// Default resolver that has no defined variables.
/// All identifiers that aren't builtins are treated as undefined.
#[derive(Debug, Default, Clone, Copy)]
pub struct EmptyResolver;

impl VariableResolver for EmptyResolver {
    fn is_defined(&self, _name: &str) -> bool {
        false
    }
}

/// Validate a CEL expression and collect errors.
pub fn validate<R: VariableResolver>(ast: &SpannedExpr, resolver: &R) -> Vec<ValidationError> {
    let mut errors = Vec::new();
    validate_expr(ast, resolver, &mut errors, false);
    errors
}

/// Recursively validate an expression.
///
/// `in_call_position` is true when this expression is the callee of a Call expression.
fn validate_expr<R: VariableResolver>(
    expr: &SpannedExpr,
    resolver: &R,
    errors: &mut Vec<ValidationError>,
    in_call_position: bool,
) {
    match &expr.node {
        // Check standalone identifiers
        Expr::Ident(name) => {
            // If in call position, builtins are valid
            // If not in call position, builtins that are standalone-only functions
            // should still be flagged as undefined variables
            if in_call_position {
                // In call position: any builtin or defined variable is valid
                if !is_builtin(name) && !resolver.is_defined(name) {
                    errors.push(ValidationError {
                        kind: ValidationErrorKind::UndefinedVariable,
                        message: format!("undefined function '{}'", name),
                        span: expr.span.clone(),
                        name: name.clone(),
                    });
                }
            } else {
                // Not in call position: only variables are valid, not functions
                if !resolver.is_defined(name) {
                    errors.push(ValidationError {
                        kind: ValidationErrorKind::UndefinedVariable,
                        message: format!("undefined variable '{}'", name),
                        span: expr.span.clone(),
                        name: name.clone(),
                    });
                }
            }
        }

        // Root identifiers are always variables, never builtins
        Expr::RootIdent(name) => {
            if !resolver.is_defined(name) {
                errors.push(ValidationError {
                    kind: ValidationErrorKind::UndefinedVariable,
                    message: format!("undefined variable '.{}'", name),
                    span: expr.span.clone(),
                    name: name.clone(),
                });
            }
        }

        // Check function/method calls
        Expr::Call { expr: callee, args } => {
            match &callee.node {
                // Standalone function call: size(x)
                Expr::Ident(_) => {
                    // Validate the identifier in call position
                    validate_expr(callee, resolver, errors, true);
                }

                // Method call: x.method(args)
                Expr::Member { expr: receiver, field } => {
                    // Validate the receiver (not in call position)
                    validate_expr(receiver, resolver, errors, false);

                    // Check if the method exists as a builtin
                    if !is_builtin(field) {
                        // Calculate span for just the method name
                        // The field starts after the '.' in the member expression
                        let method_span = (callee.span.end - field.len())..callee.span.end;
                        errors.push(ValidationError {
                            kind: ValidationErrorKind::UndefinedMethod,
                            message: format!("undefined method '{}'", field),
                            span: method_span,
                            name: field.clone(),
                        });
                    }
                }

                // Other call patterns (e.g., computed callee)
                _ => {
                    validate_expr(callee, resolver, errors, true);
                }
            }

            // Validate arguments
            for arg in args {
                validate_expr(arg, resolver, errors, false);
            }
        }

        // Member access NOT followed by call - this is field access
        // Don't validate the field name as a method
        Expr::Member { expr: inner, .. } => {
            validate_expr(inner, resolver, errors, false);
        }

        // Recursively validate other expressions
        Expr::List(items) => {
            for item in items {
                validate_expr(item, resolver, errors, false);
            }
        }

        Expr::Map(entries) => {
            for (key, value) in entries {
                validate_expr(key, resolver, errors, false);
                validate_expr(value, resolver, errors, false);
            }
        }

        Expr::Unary { expr: inner, .. } => {
            validate_expr(inner, resolver, errors, false);
        }

        Expr::Binary { left, right, .. } => {
            validate_expr(left, resolver, errors, false);
            validate_expr(right, resolver, errors, false);
        }

        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            validate_expr(cond, resolver, errors, false);
            validate_expr(then_expr, resolver, errors, false);
            validate_expr(else_expr, resolver, errors, false);
        }

        Expr::Index { expr: inner, index } => {
            validate_expr(inner, resolver, errors, false);
            validate_expr(index, resolver, errors, false);
        }

        Expr::Struct { type_name, fields } => {
            // Type name could be an identifier or member chain - validate it
            validate_expr(type_name, resolver, errors, false);
            for (_, value) in fields {
                validate_expr(value, resolver, errors, false);
            }
        }

        // Literals and error nodes need no validation
        Expr::Null
        | Expr::Bool(_)
        | Expr::Int(_)
        | Expr::UInt(_)
        | Expr::Float(_)
        | Expr::String(_)
        | Expr::Bytes(_)
        | Expr::Error => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_undefined_variable() {
        let source = "x + 1";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::UndefinedVariable);
        assert_eq!(errors[0].name, "x");
        assert!(errors[0].message.contains("undefined variable"));
    }

    #[test]
    fn allows_builtin_function_calls() {
        let source = "size([1, 2, 3])";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
    }

    #[test]
    fn allows_builtin_method_calls() {
        let source = "\"hello\".startsWith(\"h\")";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
    }

    #[test]
    fn detects_undefined_method() {
        let source = "\"hello\".unknownMethod()";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::UndefinedMethod);
        assert_eq!(errors[0].name, "unknownMethod");
        assert!(errors[0].message.contains("undefined method"));
    }

    #[test]
    fn detects_undefined_variable_with_field_access() {
        // foo.bar - foo is undefined, bar is field access (not validated)
        let source = "foo.bar";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::UndefinedVariable);
        assert_eq!(errors[0].name, "foo");
    }

    #[test]
    fn detects_multiple_errors() {
        // foo.unknownMethod() - both foo is undefined and unknownMethod is undefined
        let source = "foo.unknownMethod()";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 2);
        // Should have both undefined variable and undefined method
        let has_var_error = errors
            .iter()
            .any(|e| e.kind == ValidationErrorKind::UndefinedVariable);
        let has_method_error = errors
            .iter()
            .any(|e| e.kind == ValidationErrorKind::UndefinedMethod);
        assert!(has_var_error, "Expected undefined variable error");
        assert!(has_method_error, "Expected undefined method error");
    }

    #[test]
    fn custom_resolver_allows_variables() {
        struct TestResolver;
        impl VariableResolver for TestResolver {
            fn is_defined(&self, name: &str) -> bool {
                name == "myVar"
            }
        }

        let source = "myVar + 1";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &TestResolver);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
    }

    #[test]
    fn validates_nested_expressions() {
        let source = "[x, y, z].size()";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        // x, y, z are all undefined
        assert_eq!(errors.len(), 3);
        for error in &errors {
            assert_eq!(error.kind, ValidationErrorKind::UndefinedVariable);
        }
    }

    #[test]
    fn validates_ternary() {
        let source = "cond ? then_val : else_val";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 3);
    }

    #[test]
    fn allows_literals() {
        let source = "1 + 2 + 3.14 + \"hello\"";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert!(errors.is_empty());
    }

    #[test]
    fn detects_undefined_function() {
        let source = "unknownFunc(1, 2)";
        let result = cel_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::UndefinedVariable);
        assert_eq!(errors[0].name, "unknownFunc");
        assert!(errors[0].message.contains("undefined function"));
    }
}
