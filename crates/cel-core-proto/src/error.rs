//! Conversion error types.

use thiserror::Error;

/// Errors that can occur during conversion between AST and proto formats.
#[derive(Debug, Error)]
pub enum ConversionError {
    /// A required field was missing from the proto expression.
    #[error("expr {expr_id}: missing required field '{field}'")]
    MissingField { expr_id: i64, field: &'static str },

    /// The proto expression has an unknown or unsupported kind.
    #[error("expr {expr_id}: unknown expression kind")]
    UnknownExprKind { expr_id: i64 },

    /// A call expression uses an unknown operator function.
    #[error("expr {expr_id}: unknown operator function '{function}'")]
    UnknownOperator { expr_id: i64, function: String },

    /// A constant value is invalid or malformed.
    #[error("expr {expr_id}: invalid constant: {message}")]
    InvalidConstant { expr_id: i64, message: String },
}
