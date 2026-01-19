//! Protovalidate support for CEL.
//!
//! This module provides:
//! - Protovalidate extension functions (isEmail, isUri, etc.)
//! - Proto file parsing to extract CEL expressions
//! - ProtovalidateResolver for validating protovalidate CEL expressions

mod builtins;
pub mod proto_parser;
mod resolver;

pub use builtins::{get_protovalidate_builtin, is_protovalidate_builtin, PROTOVALIDATE_BUILTINS};
pub use proto_parser::{extract_cel_regions, ExtractedRegion};
pub use resolver::{
    check_protovalidate_method_arity, get_protovalidate_receiver_types,
    is_valid_protovalidate_method_call, ProtovalidateResolver,
};
