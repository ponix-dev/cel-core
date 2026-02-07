//! CEL type system and builtin function definitions.
//!
//! This module provides:
//! - `FunctionKind` to distinguish standalone functions from methods
//! - `Arity` to specify expected argument counts
//! - Builtin function definitions with type information for hover docs

mod builtins;
mod function;

pub use builtins::{get_builtin, is_builtin, BUILTINS};
pub use cel_core::CelType;
pub use function::{Arity, FunctionDef, FunctionKind};
