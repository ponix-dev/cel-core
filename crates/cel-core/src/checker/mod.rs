//! CEL Type Checker
//!
//! This module provides type checking for CEL (Common Expression Language) expressions,
//! producing `CheckedExpr` with `type_map` and `reference_map` for downstream processing.
//!
//! The checker is designed to be independent - it takes raw data (variables, functions,
//! container) rather than a type environment struct. This makes it reusable as a building
//! block for higher-level APIs like `cel-core::Env`.

mod checker;
mod errors;
mod overload;
mod scope;
mod standard_library;

pub use checker::{check, check_with_proto_types, CheckResult, Checker, ReferenceInfo};
pub use errors::{CheckError, CheckErrorKind};
pub use standard_library::STANDARD_LIBRARY;
