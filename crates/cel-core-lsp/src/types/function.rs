//! Function definition types for CEL.
//!
//! This module provides `FunctionDef`, a documentation-only definition of a CEL function
//! used for hover information and completion labels. Actual type checking and arity
//! validation is handled by cel-core's checker.

/// Definition of a CEL function with documentation.
#[derive(Debug, Clone)]
pub struct FunctionDef {
    /// Function name (e.g., "size")
    pub name: &'static str,
    /// Function signature (e.g., "(list<T>) -> int")
    pub signature: &'static str,
    /// Description of what the function does
    pub description: &'static str,
    /// Optional example usage
    pub example: Option<&'static str>,
}
