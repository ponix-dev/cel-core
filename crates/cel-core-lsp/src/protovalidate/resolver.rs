//! Protovalidate resolver for CEL validation.
//!
//! This module provides the `ProtovalidateResolver` which defines the variables
//! and functions available in protovalidate CEL expressions.

use cel_core_types::CelType;

use crate::types::{
    is_builtin, is_method_only, is_standalone_only, Arity, ArityCheck, FunctionKind,
    VariableResolver,
};

use super::builtins::{get_protovalidate_builtin, is_protovalidate_builtin};

/// Resolver for protovalidate CEL expressions.
///
/// In protovalidate, the following variables are always available:
/// - `this` - refers to the field or message being validated
/// - `rules` - refers to the constraint rules (in predefined constraints)
/// - `now` - the current timestamp
///
/// Additionally, protovalidate extension functions like `isEmail()`, `isUri()`, etc.
/// are available.
#[derive(Debug, Default, Clone, Copy)]
pub struct ProtovalidateResolver;

impl VariableResolver for ProtovalidateResolver {
    fn is_defined(&self, name: &str) -> bool {
        matches!(name, "this" | "rules" | "now")
    }

    fn is_valid_function(&self, name: &str) -> bool {
        is_builtin(name) || is_protovalidate_builtin(name)
    }

    fn is_method_only(&self, name: &str) -> bool {
        // Protovalidate functions are all methods
        is_method_only(name) || is_protovalidate_builtin(name)
    }

    fn is_standalone_only(&self, name: &str) -> bool {
        // Protovalidate functions are never standalone-only
        is_standalone_only(name)
    }
}

/// Check method arity for protovalidate extension functions.
///
/// Returns the same ArityCheck enum as the builtin checker.
pub fn check_protovalidate_method_arity(name: &str, arg_count: usize) -> ArityCheck {
    let Some(builtin) = get_protovalidate_builtin(name) else {
        return ArityCheck::Unknown;
    };

    let Some(arity) = builtin.method_arity else {
        return ArityCheck::Unknown;
    };

    if arity.is_valid(arg_count) {
        return ArityCheck::Valid;
    }

    let min = match arity {
        Arity::Exact(n) => n,
        Arity::Range(min, _) => min,
    };

    if arg_count < min {
        ArityCheck::TooFew {
            expected: arity,
            got: arg_count,
        }
    } else {
        ArityCheck::TooMany {
            expected: arity,
            got: arg_count,
        }
    }
}

/// Get allowed receiver types for a protovalidate method.
pub fn get_protovalidate_receiver_types(name: &str) -> Option<&'static [CelType]> {
    get_protovalidate_builtin(name).and_then(|b| b.kind.receiver_types())
}

/// Check if a protovalidate method can be called on the given receiver type.
pub fn is_valid_protovalidate_method_call(receiver_type: &CelType, method: &str) -> bool {
    let Some(builtin) = get_protovalidate_builtin(method) else {
        return true;
    };

    match &builtin.kind {
        FunctionKind::Standalone => true,
        FunctionKind::Method(allowed_types) | FunctionKind::Both(allowed_types) => {
            // Check if receiver type is compatible with any allowed type
            allowed_types.iter().any(|allowed| {
                receiver_type.is_assignable_from(allowed) || allowed.is_assignable_from(receiver_type)
            })
        }
    }
}
