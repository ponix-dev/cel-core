//! Protovalidate support for CEL.
//!
//! This module provides protovalidate-specific functionality.
//! With the migration to cel-core's checker, most validation logic
//! is now handled by cel-core. This module retains utilities
//! for protovalidate builtin documentation lookups.

use cel_core::CelType;

use crate::types::FunctionKind;

use super::builtins::get_protovalidate_builtin;

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
