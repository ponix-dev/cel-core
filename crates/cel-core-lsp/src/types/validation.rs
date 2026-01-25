//! Semantic validation for CEL expressions.
//!
//! This module provides validation for undefined variables, methods,
//! argument arity, and argument types.
//! It uses a `VariableResolver` trait to allow custom variable definitions.

use cel_core_parser::{Expr, Span, SpannedExpr};

use crate::protovalidate::{
    check_protovalidate_method_arity, get_protovalidate_receiver_types,
    is_valid_protovalidate_method_call,
};
use super::{
    check_method_arity, check_standalone_arity, get_allowed_receiver_types, infer_literal_type,
    is_builtin, is_method_only, is_standalone_only, is_valid_method_call, ArityCheck, CelType,
};

/// Category of validation error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidationErrorKind {
    UndefinedVariable,
    UndefinedMethod,
    /// A standalone-only function was called as a method (e.g., `"42".int()`)
    StandaloneCalledAsMethod,
    /// A method-only function was called standalone (e.g., `endsWith("hello", "o")`)
    MethodCalledAsStandalone,
    /// Too few arguments provided to a function.
    TooFewArguments,
    /// Too many arguments provided to a function.
    TooManyArguments,
    /// Argument type mismatch.
    InvalidArgumentType,
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

/// Trait for resolving variable names and validating functions.
///
/// Implement this trait to provide custom variable definitions and
/// extend the set of valid functions beyond CEL builtins.
pub trait VariableResolver {
    /// Check if a variable name is defined.
    fn is_defined(&self, name: &str) -> bool;

    /// Check if a function name is valid (builtin or extension).
    /// Default implementation only accepts CEL builtins.
    fn is_valid_function(&self, name: &str) -> bool {
        is_builtin(name)
    }

    /// Check if a function can only be called as a method (not standalone).
    /// Default implementation only checks CEL builtins.
    fn is_method_only(&self, name: &str) -> bool {
        is_method_only(name)
    }

    /// Check if a function can only be called standalone (not as a method).
    /// Default implementation only checks CEL builtins.
    fn is_standalone_only(&self, name: &str) -> bool {
        is_standalone_only(name)
    }
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

/// Validate argument types for method calls.
///
/// Currently validates that string methods like `endsWith`, `startsWith`, `contains`, `matches`
/// receive string arguments.
fn validate_method_arg_types(
    method: &str,
    args: &[SpannedExpr],
    errors: &mut Vec<ValidationError>,
) {
    // String methods that take a string argument
    let string_methods = ["endsWith", "startsWith", "contains", "matches"];

    if string_methods.contains(&method) {
        if let Some(first_arg) = args.first() {
            if let Some(arg_type) = infer_literal_type(&first_arg.node) {
                if arg_type != CelType::String {
                    errors.push(ValidationError {
                        kind: ValidationErrorKind::InvalidArgumentType,
                        message: format!(
                            "'{}' argument must be string, got {}",
                            method,
                            arg_type.as_str()
                        ),
                        span: first_arg.span.clone(),
                        name: method.to_string(),
                    });
                }
            }
        }
    }
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
            // If in call position, builtins/extension functions are valid
            // If not in call position, builtins that are standalone-only functions
            // should still be flagged as undefined variables
            if in_call_position {
                // In call position: any valid function or defined variable is valid
                if !resolver.is_valid_function(name) && !resolver.is_defined(name) {
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
                Expr::Ident(name) => {
                    // Check if a method-only function is being called standalone
                    if resolver.is_method_only(name) {
                        errors.push(ValidationError {
                            kind: ValidationErrorKind::MethodCalledAsStandalone,
                            message: format!(
                                "'{}' must be called as a method. Use receiver.{}(...) instead.",
                                name, name
                            ),
                            span: callee.span.clone(),
                            name: name.clone(),
                        });
                    } else {
                        // Validate the identifier in call position
                        validate_expr(callee, resolver, errors, true);

                        // Check arity for standalone function calls (only for builtins)
                        if is_builtin(name) {
                            match check_standalone_arity(name, args.len()) {
                                ArityCheck::TooFew { expected, got } => {
                                    errors.push(ValidationError {
                                        kind: ValidationErrorKind::TooFewArguments,
                                        message: format!(
                                            "'{}' expects {} argument(s), but got {}",
                                            name,
                                            expected.description(),
                                            got
                                        ),
                                        span: callee.span.clone(),
                                        name: name.clone(),
                                    });
                                }
                                ArityCheck::TooMany { expected, got } => {
                                    errors.push(ValidationError {
                                        kind: ValidationErrorKind::TooManyArguments,
                                        message: format!(
                                            "'{}' expects {} argument(s), but got {}",
                                            name,
                                            expected.description(),
                                            got
                                        ),
                                        span: callee.span.clone(),
                                        name: name.clone(),
                                    });
                                }
                                ArityCheck::Valid | ArityCheck::Unknown => {}
                            }

                            // Type check the first argument for standalone functions
                            // that have receiver type constraints (like `size`)
                            if let Some(first_arg) = args.first() {
                                if let Some(ref arg_type) = infer_literal_type(&first_arg.node) {
                                    if let Some(allowed_types) = get_allowed_receiver_types(name) {
                                        // Use is_assignable_from for compatibility with parameterized types
                                        let is_compatible = allowed_types.iter().any(|allowed| {
                                            arg_type.is_assignable_from(allowed) || allowed.is_assignable_from(arg_type)
                                        });
                                        if !is_compatible {
                                            let allowed_str: Vec<_> =
                                                allowed_types.iter().map(|t: &CelType| t.as_str()).collect();
                                            errors.push(ValidationError {
                                                kind: ValidationErrorKind::InvalidArgumentType,
                                                message: format!(
                                                    "'{}' argument must be {}, got {}",
                                                    name,
                                                    allowed_str.join(" or "),
                                                    arg_type.as_str()
                                                ),
                                                span: first_arg.span.clone(),
                                                name: name.clone(),
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Method call: x.method(args)
                Expr::Member { expr: receiver, field } => {
                    // Validate the receiver (not in call position)
                    validate_expr(receiver, resolver, errors, false);

                    // Calculate span for just the method name
                    // The field starts after the '.' in the member expression
                    let method_span = (callee.span.end - field.len())..callee.span.end;

                    // Check if the method exists as a builtin or extension function
                    if !resolver.is_valid_function(field) {
                        errors.push(ValidationError {
                            kind: ValidationErrorKind::UndefinedMethod,
                            message: format!("undefined method '{}'", field),
                            span: method_span,
                            name: field.clone(),
                        });
                    } else if resolver.is_standalone_only(field) {
                        // Standalone function called as a method
                        errors.push(ValidationError {
                            kind: ValidationErrorKind::StandaloneCalledAsMethod,
                            message: format!(
                                "'{}' is a standalone function, not a method. Use {}(...) instead.",
                                field, field
                            ),
                            span: method_span.clone(),
                            name: field.clone(),
                        });
                    } else {
                        // Check arity for method calls (try builtin first, then protovalidate)
                        let arity_check = match check_method_arity(field, args.len()) {
                            ArityCheck::Unknown => check_protovalidate_method_arity(field, args.len()),
                            result => result,
                        };
                        match arity_check {
                            ArityCheck::TooFew { expected, got } => {
                                errors.push(ValidationError {
                                    kind: ValidationErrorKind::TooFewArguments,
                                    message: format!(
                                        "'{}.{}()' expects {} argument(s), but got {}",
                                        "<receiver>",
                                        field,
                                        expected.description(),
                                        got
                                    ),
                                    span: method_span.clone(),
                                    name: field.clone(),
                                });
                            }
                            ArityCheck::TooMany { expected, got } => {
                                errors.push(ValidationError {
                                    kind: ValidationErrorKind::TooManyArguments,
                                    message: format!(
                                        "'{}.{}()' expects {} argument(s), but got {}",
                                        "<receiver>",
                                        field,
                                        expected.description(),
                                        got
                                    ),
                                    span: method_span.clone(),
                                    name: field.clone(),
                                });
                            }
                            ArityCheck::Valid | ArityCheck::Unknown => {}
                        }

                        // Type check the receiver (try builtin first, then protovalidate)
                        if let Some(ref receiver_type) = infer_literal_type(&receiver.node) {
                            let is_valid = if is_builtin(field) {
                                is_valid_method_call(receiver_type, field)
                            } else {
                                is_valid_protovalidate_method_call(receiver_type, field)
                            };
                            if !is_valid {
                                let allowed_types = get_allowed_receiver_types(field)
                                    .or_else(|| get_protovalidate_receiver_types(field));
                                if let Some(allowed_types) = allowed_types {
                                    let allowed_str: Vec<_> =
                                        allowed_types.iter().map(|t: &CelType| t.as_str()).collect();
                                    errors.push(ValidationError {
                                        kind: ValidationErrorKind::InvalidArgumentType,
                                        message: format!(
                                            "'{}' cannot be called on {}, requires {}",
                                            field,
                                            receiver_type.as_str(),
                                            allowed_str.join(" or ")
                                        ),
                                        span: receiver.span.clone(),
                                        name: field.clone(),
                                    });
                                }
                            }
                        }

                        // Type check method arguments for string methods
                        validate_method_arg_types(field, args, errors);
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
                validate_expr(&item.expr, resolver, errors, false);
            }
        }

        Expr::Map(entries) => {
            for entry in entries {
                validate_expr(&entry.key, resolver, errors, false);
                validate_expr(&entry.value, resolver, errors, false);
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
            for field in fields {
                validate_expr(&field.value, resolver, errors, false);
            }
        }

        Expr::Comprehension {
            iter_var,
            iter_var2,
            iter_range,
            accu_var,
            accu_init,
            loop_condition,
            loop_step,
            result,
        } => {
            // iter_range is validated in outer scope (no access to iter_var/accu_var)
            validate_expr(iter_range, resolver, errors, false);

            // accu_init is also evaluated in outer scope
            validate_expr(accu_init, resolver, errors, false);

            // Validate inner expressions with comprehension variables in scope
            validate_comprehension_body(
                iter_var,
                iter_var2,
                accu_var,
                loop_condition,
                loop_step,
                result,
                resolver,
                errors,
            );
        }

        Expr::MemberTestOnly { expr: inner, .. } => {
            // Validate the inner expression
            validate_expr(inner, resolver, errors, false);
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

/// Validate comprehension body expressions with scoped variables.
///
/// This is separated from `validate_expr` to avoid type recursion issues
/// with generic resolver types.
fn validate_comprehension_body<R: VariableResolver>(
    iter_var: &str,
    iter_var2: &str,
    accu_var: &str,
    loop_condition: &SpannedExpr,
    loop_step: &SpannedExpr,
    result: &SpannedExpr,
    outer_resolver: &R,
    errors: &mut Vec<ValidationError>,
) {
    // Create a scoped resolver that adds iter_var, iter_var2, and accu_var
    let comp_resolver = ComprehensionResolver {
        outer_resolver,
        iter_var,
        iter_var2,
        accu_var,
    };

    // loop_condition, loop_step, and result have access to iteration variables
    validate_expr_with_comp_resolver(loop_condition, &comp_resolver, errors, false);
    validate_expr_with_comp_resolver(loop_step, &comp_resolver, errors, false);
    validate_expr_with_comp_resolver(result, &comp_resolver, errors, false);
}

/// A resolver wrapper that adds comprehension-scoped variables.
struct ComprehensionResolver<'a, R: VariableResolver> {
    outer_resolver: &'a R,
    iter_var: &'a str,
    iter_var2: &'a str,
    accu_var: &'a str,
}

impl<R: VariableResolver> ComprehensionResolver<'_, R> {
    fn is_comp_var(&self, name: &str) -> bool {
        name == self.iter_var
            || (!self.iter_var2.is_empty() && name == self.iter_var2)
            || name == self.accu_var
    }
}

/// Validate expression using a comprehension resolver.
///
/// This is a specialized version of validate_expr that handles comprehension
/// variables without causing type recursion issues.
fn validate_expr_with_comp_resolver<R: VariableResolver>(
    expr: &SpannedExpr,
    comp_resolver: &ComprehensionResolver<'_, R>,
    errors: &mut Vec<ValidationError>,
    in_call_position: bool,
) {
    match &expr.node {
        // Check standalone identifiers - first check comprehension variables
        Expr::Ident(name) => {
            if comp_resolver.is_comp_var(name) {
                // Comprehension variable - always valid
                return;
            }
            // Fall through to normal validation
            if in_call_position {
                if !comp_resolver.outer_resolver.is_valid_function(name)
                    && !comp_resolver.outer_resolver.is_defined(name)
                {
                    errors.push(ValidationError {
                        kind: ValidationErrorKind::UndefinedVariable,
                        message: format!("undefined function '{}'", name),
                        span: expr.span.clone(),
                        name: name.clone(),
                    });
                }
            } else if !comp_resolver.outer_resolver.is_defined(name) {
                errors.push(ValidationError {
                    kind: ValidationErrorKind::UndefinedVariable,
                    message: format!("undefined variable '{}'", name),
                    span: expr.span.clone(),
                    name: name.clone(),
                });
            }
        }

        // Recursively validate sub-expressions, keeping comprehension context
        Expr::List(items) => {
            for item in items {
                validate_expr_with_comp_resolver(&item.expr, comp_resolver, errors, false);
            }
        }
        Expr::Map(entries) => {
            for entry in entries {
                validate_expr_with_comp_resolver(&entry.key, comp_resolver, errors, false);
                validate_expr_with_comp_resolver(&entry.value, comp_resolver, errors, false);
            }
        }
        Expr::Unary { expr: inner, .. } => {
            validate_expr_with_comp_resolver(inner, comp_resolver, errors, false);
        }
        Expr::Binary { left, right, .. } => {
            validate_expr_with_comp_resolver(left, comp_resolver, errors, false);
            validate_expr_with_comp_resolver(right, comp_resolver, errors, false);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            validate_expr_with_comp_resolver(cond, comp_resolver, errors, false);
            validate_expr_with_comp_resolver(then_expr, comp_resolver, errors, false);
            validate_expr_with_comp_resolver(else_expr, comp_resolver, errors, false);
        }
        Expr::Member { expr: inner, .. } => {
            validate_expr_with_comp_resolver(inner, comp_resolver, errors, false);
        }
        Expr::Index { expr: inner, index } => {
            validate_expr_with_comp_resolver(inner, comp_resolver, errors, false);
            validate_expr_with_comp_resolver(index, comp_resolver, errors, false);
        }
        Expr::Call { expr: callee, args } => {
            validate_expr_with_comp_resolver(callee, comp_resolver, errors, true);
            for arg in args {
                validate_expr_with_comp_resolver(arg, comp_resolver, errors, false);
            }
        }
        Expr::Struct { type_name, fields } => {
            validate_expr_with_comp_resolver(type_name, comp_resolver, errors, false);
            for field in fields {
                validate_expr_with_comp_resolver(&field.value, comp_resolver, errors, false);
            }
        }

        // Nested comprehension - this is rare but possible
        Expr::Comprehension {
            iter_var: inner_iter_var,
            iter_var2: inner_iter_var2,
            iter_range,
            accu_var: inner_accu_var,
            accu_init,
            loop_condition,
            loop_step,
            result,
        } => {
            // iter_range and accu_init in current comprehension scope
            validate_expr_with_comp_resolver(iter_range, comp_resolver, errors, false);
            validate_expr_with_comp_resolver(accu_init, comp_resolver, errors, false);

            // For nested comprehension body, we need both outer and inner variables
            // But since this is rare and complex, we use the outer resolver for simplicity
            // The inner comprehension variables will shadow outer ones
            validate_comprehension_body(
                inner_iter_var,
                inner_iter_var2,
                inner_accu_var,
                loop_condition,
                loop_step,
                result,
                comp_resolver.outer_resolver,
                errors,
            );
        }

        Expr::MemberTestOnly { expr: inner, .. } => {
            validate_expr_with_comp_resolver(inner, comp_resolver, errors, false);
        }

        // These don't need validation or don't have sub-expressions
        Expr::RootIdent(name) => {
            if !comp_resolver.outer_resolver.is_defined(name) {
                errors.push(ValidationError {
                    kind: ValidationErrorKind::UndefinedVariable,
                    message: format!("undefined variable '.{}'", name),
                    span: expr.span.clone(),
                    name: name.clone(),
                });
            }
        }
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
    use crate::protovalidate::ProtovalidateResolver;

    #[test]
    fn detects_undefined_variable() {
        let source = "x + 1";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::UndefinedVariable);
        assert_eq!(errors[0].name, "x");
        assert!(errors[0].message.contains("undefined variable"));
    }

    #[test]
    fn allows_builtin_function_calls() {
        let source = "size([1, 2, 3])";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
    }

    #[test]
    fn allows_builtin_method_calls() {
        let source = "\"hello\".startsWith(\"h\")";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
    }

    #[test]
    fn detects_undefined_method() {
        let source = "\"hello\".unknownMethod()";
        let result = cel_core_parser::parse(source);
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
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::UndefinedVariable);
        assert_eq!(errors[0].name, "foo");
    }

    #[test]
    fn detects_multiple_errors() {
        // foo.unknownMethod() - both foo is undefined and unknownMethod is undefined
        let source = "foo.unknownMethod()";
        let result = cel_core_parser::parse(source);
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
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &TestResolver);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
    }

    #[test]
    fn validates_nested_expressions() {
        let source = "[x, y, z].size()";
        let result = cel_core_parser::parse(source);
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
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 3);
    }

    #[test]
    fn allows_literals() {
        let source = "1 + 2 + 3.14 + \"hello\"";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert!(errors.is_empty());
    }

    #[test]
    fn detects_undefined_function() {
        let source = "unknownFunc(1, 2)";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::UndefinedVariable);
        assert_eq!(errors[0].name, "unknownFunc");
        assert!(errors[0].message.contains("undefined function"));
    }

    #[test]
    fn standalone_called_as_method_error() {
        // int() is a standalone function, should not be called as a method
        let source = "\"42\".int()";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::StandaloneCalledAsMethod);
        assert_eq!(errors[0].name, "int");
        assert!(errors[0].message.contains("standalone function"));
    }

    #[test]
    fn standalone_bool_called_as_method_error() {
        // bool() is a standalone function
        let source = "1.bool()";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::StandaloneCalledAsMethod);
        assert_eq!(errors[0].name, "bool");
    }

    #[test]
    fn method_called_as_standalone_error() {
        // endsWith() is a method-only function
        let source = "endsWith(\"hello\", \"o\")";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::MethodCalledAsStandalone);
        assert_eq!(errors[0].name, "endsWith");
        assert!(errors[0].message.contains("must be called as a method"));
    }

    #[test]
    fn method_startswith_called_as_standalone_error() {
        // startsWith() is a method-only function
        let source = "startsWith(\"hello\", \"h\")";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::MethodCalledAsStandalone);
        assert_eq!(errors[0].name, "startsWith");
    }

    #[test]
    fn method_all_called_as_standalone_error() {
        // all() is a method-only macro
        // Note: `all([1,2,3], x, x > 0)` also generates errors for `x` since
        // the validator doesn't understand comprehension semantics
        let source = "all([1,2,3], x, x > 0)";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        // Check that one of the errors is the method-as-standalone error
        let method_error = errors
            .iter()
            .find(|e| e.kind == ValidationErrorKind::MethodCalledAsStandalone);
        assert!(
            method_error.is_some(),
            "Expected MethodCalledAsStandalone error, got: {:?}",
            errors
        );
        assert_eq!(method_error.unwrap().name, "all");
    }

    #[test]
    fn both_functions_work_either_way() {
        // size() has FunctionKind::Both, should work both ways
        let standalone = "size([1,2,3])";
        let method = "[1,2,3].size()";

        let result1 = cel_core_parser::parse(standalone);
        let errors1 = validate(result1.ast.as_ref().unwrap(), &EmptyResolver);
        assert!(errors1.is_empty(), "size() should work as standalone: {:?}", errors1);

        let result2 = cel_core_parser::parse(method);
        let errors2 = validate(result2.ast.as_ref().unwrap(), &EmptyResolver);
        assert!(errors2.is_empty(), "size() should work as method: {:?}", errors2);
    }

    // ==================== Arity Validation Tests ====================

    #[test]
    fn too_few_args_standalone_size() {
        let source = "size()";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::TooFewArguments);
        assert!(errors[0].message.contains("expects 1"));
        assert!(errors[0].message.contains("got 0"));
    }

    #[test]
    fn too_many_args_standalone_size() {
        let source = "size([1], [2])";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::TooManyArguments);
        assert!(errors[0].message.contains("expects 1"));
        assert!(errors[0].message.contains("got 2"));
    }

    #[test]
    fn too_many_args_method_size() {
        let source = "[1,2,3].size(1)";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::TooManyArguments);
        assert!(errors[0].message.contains("expects 0"));
    }

    #[test]
    fn too_few_args_method_ends_with() {
        let source = "\"hello\".endsWith()";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::TooFewArguments);
        assert!(errors[0].message.contains("expects 1"));
    }

    #[test]
    fn too_many_args_method_ends_with() {
        let source = "\"hello\".endsWith(\"o\", \"extra\")";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, ValidationErrorKind::TooManyArguments);
    }

    #[test]
    fn correct_arity_passes() {
        let sources = [
            "size(\"hello\")",
            "\"hello\".size()",
            "\"hello\".startsWith(\"h\")",
            "\"hello\".endsWith(\"o\")",
            "int(42)",
            "[1,2,3].all(x, x > 0)",
            "[1,2,3].map(x, x * 2)",
        ];

        for source in sources {
            let result = cel_core_parser::parse(source);
            let errors: Vec<_> = validate(result.ast.as_ref().unwrap(), &EmptyResolver)
                .into_iter()
                .filter(|e| {
                    matches!(
                        e.kind,
                        ValidationErrorKind::TooFewArguments | ValidationErrorKind::TooManyArguments
                    )
                })
                .collect();
            assert!(errors.is_empty(), "Expected no arity errors for '{}', got: {:?}", source, errors);
        }
    }

    #[test]
    fn map_macro_accepts_2_or_3_args() {
        // 2 args - valid
        let source1 = "[1,2,3].map(x, x * 2)";
        let result1 = cel_core_parser::parse(source1);
        assert!(result1.is_ok(), "map with 2 args should parse successfully");
        let arity_errors1: Vec<_> = validate(result1.ast.as_ref().unwrap(), &EmptyResolver)
            .into_iter()
            .filter(|e| {
                matches!(
                    e.kind,
                    ValidationErrorKind::TooFewArguments | ValidationErrorKind::TooManyArguments
                )
            })
            .collect();
        assert!(arity_errors1.is_empty(), "map with 2 args should be valid");

        // 3 args - valid (with filter)
        let source2 = "[1,2,3].map(x, x > 1, x * 2)";
        let result2 = cel_core_parser::parse(source2);
        assert!(result2.is_ok(), "map with 3 args should parse successfully");
        let arity_errors2: Vec<_> = validate(result2.ast.as_ref().unwrap(), &EmptyResolver)
            .into_iter()
            .filter(|e| {
                matches!(
                    e.kind,
                    ValidationErrorKind::TooFewArguments | ValidationErrorKind::TooManyArguments
                )
            })
            .collect();
        assert!(arity_errors2.is_empty(), "map with 3 args should be valid");

        // 4 args - invalid (results in unexpanded call, matching cel-go behavior)
        // Note: macro errors don't appear in parse result - they silently leave calls unexpanded
        let source3 = "[1,2,3].map(x, x > 1, x * 2, \"extra\")";
        let result3 = cel_core_parser::parse(source3);
        // Should have AST (unexpanded call) and no parse errors
        assert!(
            result3.ast.is_some(),
            "map with 4 args should still produce an AST (unexpanded call)"
        );
        assert!(
            result3.is_ok(),
            "map with invalid args should not report parse errors (macro errors are silent)"
        );
        // The call is left unexpanded - it's a Call node, not a Comprehension
        // This matches cel-go behavior where invalid macros are left as regular calls
    }

    #[test]
    fn timestamp_methods_accept_optional_timezone() {
        // This would normally require a timestamp variable, but we can test the arity
        // by using a variable and checking there are no arity errors
        struct TimestampResolver;
        impl VariableResolver for TimestampResolver {
            fn is_defined(&self, name: &str) -> bool {
                name == "ts"
            }
        }

        // 0 args - valid
        let source1 = "ts.getHours()";
        let result1 = cel_core_parser::parse(source1);
        let arity_errors1: Vec<_> = validate(result1.ast.as_ref().unwrap(), &TimestampResolver)
            .into_iter()
            .filter(|e| {
                matches!(
                    e.kind,
                    ValidationErrorKind::TooFewArguments | ValidationErrorKind::TooManyArguments
                )
            })
            .collect();
        assert!(arity_errors1.is_empty(), "getHours() with 0 args should be valid");

        // 1 arg - valid (timezone)
        let source2 = "ts.getHours(\"America/New_York\")";
        let result2 = cel_core_parser::parse(source2);
        let arity_errors2: Vec<_> = validate(result2.ast.as_ref().unwrap(), &TimestampResolver)
            .into_iter()
            .filter(|e| {
                matches!(
                    e.kind,
                    ValidationErrorKind::TooFewArguments | ValidationErrorKind::TooManyArguments
                )
            })
            .collect();
        assert!(arity_errors2.is_empty(), "getHours() with 1 arg should be valid");

        // 2 args - invalid
        let source3 = "ts.getHours(\"tz1\", \"tz2\")";
        let result3 = cel_core_parser::parse(source3);
        let arity_errors3: Vec<_> = validate(result3.ast.as_ref().unwrap(), &TimestampResolver)
            .into_iter()
            .filter(|e| e.kind == ValidationErrorKind::TooManyArguments)
            .collect();
        assert_eq!(arity_errors3.len(), 1, "getHours() with 2 args should be invalid");
    }

    // ==================== Type Validation Tests ====================

    #[test]
    fn invalid_type_for_size_standalone() {
        let source = "size(123)";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        let type_error = errors
            .iter()
            .find(|e| e.kind == ValidationErrorKind::InvalidArgumentType);
        assert!(type_error.is_some(), "Expected InvalidArgumentType error");
        assert!(type_error.unwrap().message.contains("int"));
    }

    #[test]
    fn valid_types_for_size_standalone() {
        let valid_sources = [
            "size(\"hello\")",   // string
            "size([1, 2, 3])",   // list
            "size({\"a\": 1})",  // map
        ];

        for source in valid_sources {
            let result = cel_core_parser::parse(source);
            let type_errors: Vec<_> = validate(result.ast.as_ref().unwrap(), &EmptyResolver)
                .into_iter()
                .filter(|e| e.kind == ValidationErrorKind::InvalidArgumentType)
                .collect();
            assert!(type_errors.is_empty(), "Expected no type errors for '{}', got: {:?}", source, type_errors);
        }
    }

    #[test]
    fn invalid_receiver_type_for_method() {
        let source = "123.startsWith(\"1\")";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        let type_error = errors
            .iter()
            .find(|e| e.kind == ValidationErrorKind::InvalidArgumentType);
        assert!(type_error.is_some(), "Expected InvalidArgumentType error for int receiver");
        assert!(type_error.unwrap().message.contains("int"));
    }

    #[test]
    fn invalid_arg_type_for_string_method() {
        let source = "\"hello\".endsWith(123)";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        let type_error = errors
            .iter()
            .find(|e| e.kind == ValidationErrorKind::InvalidArgumentType);
        assert!(type_error.is_some(), "Expected InvalidArgumentType error for int arg");
        assert!(type_error.unwrap().message.contains("int"));
    }

    #[test]
    fn unknown_type_not_validated() {
        // When we can't infer the type, we shouldn't produce type errors
        struct AllowX;
        impl VariableResolver for AllowX {
            fn is_defined(&self, name: &str) -> bool {
                name == "x"
            }
        }

        let source = "size(x)";
        let result = cel_core_parser::parse(source);
        let type_errors: Vec<_> = validate(result.ast.as_ref().unwrap(), &AllowX)
            .into_iter()
            .filter(|e| e.kind == ValidationErrorKind::InvalidArgumentType)
            .collect();

        assert!(type_errors.is_empty(), "Should not validate type of variable x");
    }

    #[test]
    fn valid_method_calls_pass_type_check() {
        let valid_sources = [
            "\"hello\".startsWith(\"h\")",
            "\"hello\".endsWith(\"o\")",
            "\"hello\".contains(\"ll\")",
            "\"hello\".matches(\"h.*\")",
            "[1, 2, 3].size()",
            "{\"a\": 1}.size()",
        ];

        for source in valid_sources {
            let result = cel_core_parser::parse(source);
            let type_errors: Vec<_> = validate(result.ast.as_ref().unwrap(), &EmptyResolver)
                .into_iter()
                .filter(|e| e.kind == ValidationErrorKind::InvalidArgumentType)
                .collect();
            assert!(type_errors.is_empty(), "Expected no type errors for '{}', got: {:?}", source, type_errors);
        }
    }

    // ==================== Protovalidate Function Tests ====================

    #[test]
    fn protovalidate_functions_valid_in_proto_context() {
        // Protovalidate functions should be valid when using ProtovalidateResolver
        let valid_sources = [
            "this.isEmail()",
            "this.isUri()",
            "this.isHostname()",
            "this.unique()",
            "this.isNan()",
            "this.isInf()",
            "this.isIp()",
            "this.isIp(4)",
        ];

        for source in valid_sources {
            let result = cel_core_parser::parse(source);
            let errors: Vec<_> = validate(result.ast.as_ref().unwrap(), &ProtovalidateResolver)
                .into_iter()
                .filter(|e| e.kind == ValidationErrorKind::UndefinedMethod)
                .collect();
            assert!(errors.is_empty(), "Expected no undefined method errors for '{}', got: {:?}", source, errors);
        }
    }

    #[test]
    fn protovalidate_functions_invalid_in_cel_context() {
        // Protovalidate functions should be flagged as undefined in plain CEL context
        let source = "\"test\".isEmail()";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &EmptyResolver);

        let method_error = errors
            .iter()
            .find(|e| e.kind == ValidationErrorKind::UndefinedMethod);
        assert!(
            method_error.is_some(),
            "Expected UndefinedMethod error for protovalidate function in CEL context"
        );
        assert_eq!(method_error.unwrap().name, "isEmail");
    }

    #[test]
    fn protovalidate_functions_are_method_only() {
        // Protovalidate functions should not be callable as standalone
        let source = "isEmail(this)";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &ProtovalidateResolver);

        let method_error = errors
            .iter()
            .find(|e| e.kind == ValidationErrorKind::MethodCalledAsStandalone);
        assert!(
            method_error.is_some(),
            "Expected MethodCalledAsStandalone error for protovalidate function called standalone"
        );
    }

    #[test]
    fn protovalidate_arity_checking() {
        // isEmail takes 0 arguments
        let source1 = "this.isEmail(\"extra\")";
        let result1 = cel_core_parser::parse(source1);
        let arity_errors1: Vec<_> = validate(result1.ast.as_ref().unwrap(), &ProtovalidateResolver)
            .into_iter()
            .filter(|e| e.kind == ValidationErrorKind::TooManyArguments)
            .collect();
        assert_eq!(arity_errors1.len(), 1, "isEmail with args should be invalid");

        // isIp takes 0-1 arguments
        let source2 = "this.isIp()";
        let result2 = cel_core_parser::parse(source2);
        let arity_errors2: Vec<_> = validate(result2.ast.as_ref().unwrap(), &ProtovalidateResolver)
            .into_iter()
            .filter(|e| matches!(e.kind, ValidationErrorKind::TooFewArguments | ValidationErrorKind::TooManyArguments))
            .collect();
        assert!(arity_errors2.is_empty(), "isIp() with 0 args should be valid");

        let source3 = "this.isIp(4)";
        let result3 = cel_core_parser::parse(source3);
        let arity_errors3: Vec<_> = validate(result3.ast.as_ref().unwrap(), &ProtovalidateResolver)
            .into_iter()
            .filter(|e| matches!(e.kind, ValidationErrorKind::TooFewArguments | ValidationErrorKind::TooManyArguments))
            .collect();
        assert!(arity_errors3.is_empty(), "isIp(4) with 1 arg should be valid");

        let source4 = "this.isIp(4, 6)";
        let result4 = cel_core_parser::parse(source4);
        let arity_errors4: Vec<_> = validate(result4.ast.as_ref().unwrap(), &ProtovalidateResolver)
            .into_iter()
            .filter(|e| e.kind == ValidationErrorKind::TooManyArguments)
            .collect();
        assert_eq!(arity_errors4.len(), 1, "isIp with 2 args should be invalid");
    }

    #[test]
    fn protovalidate_receiver_type_checking() {
        // isEmail requires string receiver
        let source = "123.isEmail()";
        let result = cel_core_parser::parse(source);
        let errors = validate(result.ast.as_ref().unwrap(), &ProtovalidateResolver);

        let type_error = errors
            .iter()
            .find(|e| e.kind == ValidationErrorKind::InvalidArgumentType);
        assert!(
            type_error.is_some(),
            "Expected InvalidArgumentType error for int receiver on isEmail"
        );
        assert!(type_error.unwrap().message.contains("int"));
    }
}
