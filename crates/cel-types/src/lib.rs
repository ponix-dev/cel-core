//! CEL type system and builtin function definitions.
//!
//! This crate provides:
//! - `CelType` enum representing CEL's type system
//! - `FunctionKind` to distinguish standalone functions from methods
//! - Builtin function definitions with type information
//! - Type checking utilities for method call validation

mod checker;
mod functions;
mod types;

pub use checker::{infer_literal_type, is_valid_method_call};
pub use functions::{
    get_builtin, is_builtin, BuiltinFunction, FunctionKind, BUILTINS,
};
pub use types::CelType;
