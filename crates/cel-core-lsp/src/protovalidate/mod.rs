//! Protovalidate support for CEL.
//!
//! This module provides:
//! - Protovalidate extension functions (isEmail, isUri, etc.) for hover documentation
//! - Proto file parsing to extract CEL expressions
//! - Context extraction for typed protovalidate validation

mod builtins;
pub mod proto_parser;

pub use builtins::{get_protovalidate_builtin, PROTOVALIDATE_BUILTINS};
pub use proto_parser::{extract_cel_regions, ProtovalidateContext};
