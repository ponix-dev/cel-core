//! CEL Type Checker
//!
//! This crate provides type checking for CEL (Common Expression Language) expressions,
//! producing `CheckedExpr` with `type_map` and `reference_map` for downstream processing.
//!
//! The checker is designed to be independent - it takes raw data (variables, functions,
//! container) rather than a type environment struct. This makes it reusable as a building
//! block for higher-level APIs like `cel-core::Env`.
//!
//! # Example
//!
//! ```
//! use std::collections::HashMap;
//! use cel_core_checker::{check, FunctionDecl, STANDARD_LIBRARY};
//! use cel_core_common::CelType;
//!
//! let source = "x + 1";
//! let result = cel_core_parser::parse(source);
//! let ast = result.ast.unwrap();
//!
//! // Build variables map
//! let mut variables = HashMap::new();
//! variables.insert("x".to_string(), CelType::Int);
//!
//! // Build functions map from standard library
//! let functions: HashMap<String, FunctionDecl> = STANDARD_LIBRARY
//!     .iter()
//!     .map(|f| (f.name.clone(), f.clone()))
//!     .collect();
//!
//! let check_result = check(&ast, &variables, &functions, "");
//! assert!(check_result.is_ok());
//! ```

mod checker;
mod errors;
mod overload;
mod scope;
mod standard_library;

pub use checker::{check, check_with_proto_types, CheckResult, Checker, ReferenceInfo};
pub use errors::{CheckError, CheckErrorKind};
pub use standard_library::STANDARD_LIBRARY;

// Re-export declaration types from cel-core-common for backwards compatibility
pub use cel_core_common::{FunctionDecl, OverloadDecl, VariableDecl};
