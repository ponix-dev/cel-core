//! LSP protocol feature implementations.
//!
//! This module provides implementations for LSP features:
//! - Diagnostics conversion from parser/validation errors
//! - Hover information for CEL expressions
//! - Semantic tokens for syntax highlighting

mod completion;
mod diagnostics;
mod hover;
mod semantic_tokens;

pub use completion::{completion_at_position, completion_at_position_proto};
pub use diagnostics::{proto_to_diagnostics, to_diagnostics};
pub use hover::{hover_at_position, hover_at_position_proto};
pub use semantic_tokens::{legend, tokens_for_ast, tokens_for_proto};
