//! Type checking utilities for CEL expressions.

use cel_core_parser::ast::Expr;

use super::builtins::get_builtin;
use super::cel_type::CelType;
use super::function::{Arity, FunctionKind};

/// Result of arity validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArityCheck {
    /// Arity is valid.
    Valid,
    /// Too few arguments provided.
    TooFew {
        expected: Arity,
        got: usize,
    },
    /// Too many arguments provided.
    TooMany {
        expected: Arity,
        got: usize,
    },
    /// Unknown function (no arity info available).
    Unknown,
}

/// Check if the argument count is valid for a standalone function call.
///
/// Returns `ArityCheck::Valid` if the arity is correct, `TooFew`/`TooMany` if not,
/// or `Unknown` if the function doesn't exist or has no standalone form.
pub fn check_standalone_arity(name: &str, arg_count: usize) -> ArityCheck {
    let Some(builtin) = get_builtin(name) else {
        return ArityCheck::Unknown;
    };

    let Some(arity) = builtin.standalone_arity else {
        // Function exists but not callable as standalone
        return ArityCheck::Unknown;
    };

    check_arity(arity, arg_count)
}

/// Check if the argument count is valid for a method call.
///
/// Returns `ArityCheck::Valid` if the arity is correct, `TooFew`/`TooMany` if not,
/// or `Unknown` if the function doesn't exist or has no method form.
pub fn check_method_arity(name: &str, arg_count: usize) -> ArityCheck {
    let Some(builtin) = get_builtin(name) else {
        return ArityCheck::Unknown;
    };

    let Some(arity) = builtin.method_arity else {
        // Function exists but not callable as method
        return ArityCheck::Unknown;
    };

    check_arity(arity, arg_count)
}

/// Internal helper to check an arity constraint.
fn check_arity(arity: Arity, count: usize) -> ArityCheck {
    if arity.is_valid(count) {
        return ArityCheck::Valid;
    }

    let min = match arity {
        Arity::Exact(n) => n,
        Arity::Range(min, _) => min,
    };

    if count < min {
        ArityCheck::TooFew {
            expected: arity,
            got: count,
        }
    } else {
        ArityCheck::TooMany {
            expected: arity,
            got: count,
        }
    }
}

/// Infer the type of a literal expression.
///
/// Returns `Some(CelType)` for literal expressions (strings, ints, lists, etc.)
/// and `None` for expressions whose type cannot be determined statically
/// (variables, function calls, etc.).
pub fn infer_literal_type(expr: &Expr) -> Option<CelType> {
    match expr {
        Expr::Null => Some(CelType::Null),
        Expr::Bool(_) => Some(CelType::Bool),
        Expr::Int(_) => Some(CelType::Int),
        Expr::UInt(_) => Some(CelType::UInt),
        Expr::Float(_) => Some(CelType::Double),
        Expr::String(_) => Some(CelType::String),
        Expr::Bytes(_) => Some(CelType::Bytes),
        Expr::List(_) => Some(CelType::List),
        Expr::Map(_) => Some(CelType::Map),
        // Cannot determine type statically for these
        Expr::Ident(_) | Expr::RootIdent(_) => None,
        Expr::Member { .. } => None,
        Expr::MemberTestOnly { .. } => Some(CelType::Bool), // has() always returns bool
        Expr::Index { .. } => None,
        Expr::Call { .. } => None,
        Expr::Unary { .. } => None,
        Expr::Binary { .. } => None,
        Expr::Ternary { .. } => None,
        Expr::Struct { .. } => None,
        Expr::Comprehension { .. } => None, // Result type depends on the macro
        Expr::Error => None,
    }
}

/// Check if a method can be validly called on the given receiver type.
///
/// Returns `true` if:
/// - The method doesn't exist (we don't validate unknown methods here)
/// - The method is `Standalone` (it shouldn't be called as a method, but we allow it)
/// - The method is `Method` or `Both` and the receiver type is in the allowed list
///
/// Returns `false` if the method exists and the receiver type is not allowed.
pub fn is_valid_method_call(receiver_type: CelType, method: &str) -> bool {
    let Some(builtin) = get_builtin(method) else {
        // Unknown method - don't validate
        return true;
    };

    match &builtin.kind {
        FunctionKind::Standalone => {
            // This shouldn't be called as a method, but we're not
            // validating that here - just checking receiver type
            true
        }
        FunctionKind::Method(allowed_types) | FunctionKind::Both(allowed_types) => {
            allowed_types.contains(&receiver_type)
        }
    }
}

/// Check if a function can only be called as a standalone function (not as a method).
pub fn is_standalone_only(name: &str) -> bool {
    get_builtin(name)
        .map(|b| matches!(b.kind, FunctionKind::Standalone))
        .unwrap_or(false)
}

/// Check if a function can only be called as a method (not standalone).
pub fn is_method_only(name: &str) -> bool {
    get_builtin(name)
        .map(|b| matches!(b.kind, FunctionKind::Method(_)))
        .unwrap_or(false)
}

/// Get the allowed receiver types for a method.
///
/// Returns `None` if the function is standalone-only or unknown.
pub fn get_allowed_receiver_types(name: &str) -> Option<&'static [CelType]> {
    get_builtin(name).and_then(|b| match &b.kind {
        FunctionKind::Standalone => None,
        FunctionKind::Method(types) | FunctionKind::Both(types) => Some(*types),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn infer_literal_types() {
        assert_eq!(infer_literal_type(&Expr::Bool(true)), Some(CelType::Bool));
        assert_eq!(infer_literal_type(&Expr::Int(42)), Some(CelType::Int));
        assert_eq!(infer_literal_type(&Expr::UInt(42)), Some(CelType::UInt));
        assert_eq!(infer_literal_type(&Expr::Float(3.14)), Some(CelType::Double));
        assert_eq!(
            infer_literal_type(&Expr::String("hello".to_string())),
            Some(CelType::String)
        );
        assert_eq!(
            infer_literal_type(&Expr::Bytes(vec![1, 2, 3])),
            Some(CelType::Bytes)
        );
        assert_eq!(infer_literal_type(&Expr::List(vec![])), Some(CelType::List));
        assert_eq!(infer_literal_type(&Expr::Map(vec![])), Some(CelType::Map));
        assert_eq!(infer_literal_type(&Expr::Null), Some(CelType::Null));
    }

    #[test]
    fn infer_returns_none_for_non_literals() {
        assert_eq!(
            infer_literal_type(&Expr::Ident("x".to_string())),
            None
        );
    }

    #[test]
    fn valid_method_calls() {
        // endsWith on string - valid
        assert!(is_valid_method_call(CelType::String, "endsWith"));
        // size on string - valid
        assert!(is_valid_method_call(CelType::String, "size"));
        // size on list - valid
        assert!(is_valid_method_call(CelType::List, "size"));
        // contains on string - valid
        assert!(is_valid_method_call(CelType::String, "contains"));
    }

    #[test]
    fn invalid_method_calls() {
        // endsWith on list - invalid
        assert!(!is_valid_method_call(CelType::List, "endsWith"));
        // endsWith on int - invalid
        assert!(!is_valid_method_call(CelType::Int, "endsWith"));
        // startsWith on map - invalid
        assert!(!is_valid_method_call(CelType::Map, "startsWith"));
    }

    #[test]
    fn unknown_methods_pass() {
        // Unknown method - we don't validate
        assert!(is_valid_method_call(CelType::String, "unknownMethod"));
    }

    #[test]
    fn standalone_only_functions() {
        assert!(is_standalone_only("int"));
        assert!(is_standalone_only("bool"));
        assert!(is_standalone_only("has"));
        assert!(!is_standalone_only("endsWith"));
        assert!(!is_standalone_only("size"));
    }

    #[test]
    fn method_only_functions() {
        assert!(is_method_only("endsWith"));
        assert!(is_method_only("startsWith"));
        assert!(is_method_only("all"));
        assert!(!is_method_only("int"));
        assert!(!is_method_only("size")); // size is Both, not Method-only
    }

    #[test]
    fn get_receiver_types() {
        // Standalone functions have no receiver types
        assert!(get_allowed_receiver_types("int").is_none());

        // Method functions have receiver types
        let ends_with_types = get_allowed_receiver_types("endsWith").unwrap();
        assert!(ends_with_types.contains(&CelType::String));
        assert!(!ends_with_types.contains(&CelType::List));

        // Both functions also have receiver types
        let size_types = get_allowed_receiver_types("size").unwrap();
        assert!(size_types.contains(&CelType::String));
        assert!(size_types.contains(&CelType::List));
    }

    #[test]
    fn check_standalone_arity_size() {
        // size() standalone takes 1 argument
        assert_eq!(check_standalone_arity("size", 1), ArityCheck::Valid);
        assert!(matches!(
            check_standalone_arity("size", 0),
            ArityCheck::TooFew { .. }
        ));
        assert!(matches!(
            check_standalone_arity("size", 2),
            ArityCheck::TooMany { .. }
        ));
    }

    #[test]
    fn check_method_arity_size() {
        // size() as method takes 0 arguments
        assert_eq!(check_method_arity("size", 0), ArityCheck::Valid);
        assert!(matches!(
            check_method_arity("size", 1),
            ArityCheck::TooMany { .. }
        ));
    }

    #[test]
    fn check_method_arity_ends_with() {
        // endsWith() as method takes 1 argument
        assert_eq!(check_method_arity("endsWith", 1), ArityCheck::Valid);
        assert!(matches!(
            check_method_arity("endsWith", 0),
            ArityCheck::TooFew { .. }
        ));
        assert!(matches!(
            check_method_arity("endsWith", 2),
            ArityCheck::TooMany { .. }
        ));
    }

    #[test]
    fn check_method_arity_map_macro() {
        // map() macro takes 2-3 arguments
        assert_eq!(check_method_arity("map", 2), ArityCheck::Valid);
        assert_eq!(check_method_arity("map", 3), ArityCheck::Valid);
        assert!(matches!(
            check_method_arity("map", 1),
            ArityCheck::TooFew { .. }
        ));
        assert!(matches!(
            check_method_arity("map", 4),
            ArityCheck::TooMany { .. }
        ));
    }

    #[test]
    fn check_method_arity_timestamp_methods() {
        // Timestamp methods take 0-1 arguments (optional timezone)
        assert_eq!(check_method_arity("getHours", 0), ArityCheck::Valid);
        assert_eq!(check_method_arity("getHours", 1), ArityCheck::Valid);
        assert!(matches!(
            check_method_arity("getHours", 2),
            ArityCheck::TooMany { .. }
        ));
    }

    #[test]
    fn check_arity_unknown_function() {
        assert_eq!(check_standalone_arity("unknownFunc", 1), ArityCheck::Unknown);
        assert_eq!(check_method_arity("unknownFunc", 1), ArityCheck::Unknown);
    }

    #[test]
    fn check_standalone_arity_method_only() {
        // endsWith is method-only, so standalone check returns Unknown
        assert_eq!(check_standalone_arity("endsWith", 1), ArityCheck::Unknown);
    }

    #[test]
    fn check_method_arity_standalone_only() {
        // int is standalone-only, so method check returns Unknown
        assert_eq!(check_method_arity("int", 0), ArityCheck::Unknown);
    }
}
