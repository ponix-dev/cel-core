//! CEL Type Checker
//!
//! This crate provides type checking for CEL (Common Expression Language) expressions,
//! producing `CheckedExpr` with `type_map` and `reference_map` for downstream processing.
//!
//! # Example
//!
//! ```
//! use cel_core_checker::{check, TypeEnv};
//!
//! let source = "x + 1";
//! let result = cel_core_parser::parse(source);
//! let ast = result.ast.unwrap();
//!
//! let mut env = TypeEnv::with_standard_library();
//! env.add_variable("x", cel_core_types::CelType::Int);
//!
//! let check_result = check(&ast, &mut env);
//! assert!(check_result.is_ok());
//! ```

mod checker;
mod decls;
mod env;
mod errors;
mod overload;
mod scope;
mod standard_library;

pub use checker::{check, CheckResult, Checker, ReferenceInfo};
pub use decls::{FunctionDecl, OverloadDecl, VariableDecl};
pub use env::TypeEnv;
pub use errors::{CheckError, CheckErrorKind};
pub use standard_library::STANDARD_LIBRARY;
