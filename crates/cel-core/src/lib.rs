//! CEL-Core: High-level API for the Common Expression Language
//!
//! This crate provides a unified `Env` for working with CEL expressions,
//! following the cel-go architecture pattern.
//!
//! # Quick Start
//!
//! ```
//! use cel_core::Env;
//! use cel_core_common::CelType;
//!
//! // Create an environment with standard library and a variable
//! let env = Env::with_standard_library()
//!     .with_variable("x", CelType::Int);
//!
//! // Parse and type-check in one step
//! let result = env.compile("x + 1");
//! assert!(result.is_ok());
//! ```
//!
//! # Architecture
//!
//! The `Env` struct coordinates:
//! - **Parser**: Converts source text into an AST
//! - **Checker**: Type-checks expressions and resolves references
//! - **Variables**: User-defined variable declarations
//! - **Functions**: Standard library + custom function declarations
//!
//! # Re-exports
//!
//! This crate re-exports commonly used types from the underlying crates:
//! - `FunctionDecl`, `OverloadDecl`, `VariableDecl` from `cel-core-checker`
//! - `CelType`, `SpannedExpr` from `cel-core-common`
//! - `ParseResult` from `cel-core-parser`

mod env;

pub use env::Env;

// Re-export from checker
pub use cel_core_checker::{
    check, CheckError, CheckErrorKind, CheckResult, FunctionDecl, OverloadDecl, ReferenceInfo,
    VariableDecl, STANDARD_LIBRARY,
};

// Re-export from parser
pub use cel_core_parser::ParseResult;

// Re-export from common
pub use cel_core_common::{CelType, SpannedExpr};
