//! CEL type system and builtin function definitions.
//!
//! This module provides:
//! - `CelType` enum representing CEL's type system
//! - `FunctionKind` to distinguish standalone functions from methods
//! - `Arity` to specify expected argument counts
//! - Builtin function definitions with type information
//! - Type checking utilities for method call validation
//! - Arity checking utilities for function argument validation

mod builtins;
mod checker;
mod function;
pub mod validation;

pub use builtins::{get_builtin, is_builtin, BUILTINS};
pub use cel_core::CelType;
pub use checker::{
    check_method_arity, check_standalone_arity, get_allowed_receiver_types, infer_literal_type,
    is_method_only, is_standalone_only, is_valid_method_call, ArityCheck,
};
pub use function::{Arity, FunctionDef, FunctionKind};
pub use validation::{validate, EmptyResolver, ValidationError, ValidationErrorKind, VariableResolver};
