//! Function definition types for CEL.
//!
//! This module provides the core types for defining CEL functions:
//! - `Arity` - specifies expected argument counts
//! - `FunctionKind` - distinguishes standalone functions from methods
//! - `FunctionDef` - complete function definition with documentation

use cel_core_common::CelType;

/// Expected number of arguments for a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arity {
    /// Exactly N arguments required
    Exact(usize),
    /// Between min and max arguments (inclusive)
    Range(usize, usize),
}

impl Arity {
    /// Check if the given count satisfies this arity constraint.
    pub fn is_valid(&self, count: usize) -> bool {
        match self {
            Arity::Exact(n) => count == *n,
            Arity::Range(min, max) => count >= *min && count <= *max,
        }
    }

    /// Return a human-readable description of expected arity.
    pub fn description(&self) -> String {
        match self {
            Arity::Exact(n) => format!("{}", n),
            Arity::Range(min, max) if min == max => format!("{}", min),
            Arity::Range(min, max) => format!("{} to {}", min, max),
        }
    }
}

/// How a function can be called.
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionKind {
    /// Only callable as a standalone function: `int(x)`, `bool(x)`
    Standalone,
    /// Only callable as a method on specific receiver types: `"foo".endsWith("o")`
    Method(Vec<CelType>),
    /// Callable both as standalone and as method: `size(x)` or `x.size()`
    Both(Vec<CelType>),
}

impl FunctionKind {
    /// Get the allowed receiver types for this function kind.
    pub fn receiver_types(&self) -> Option<&[CelType]> {
        match self {
            FunctionKind::Standalone => None,
            FunctionKind::Method(types) | FunctionKind::Both(types) => Some(types),
        }
    }
}

/// Definition of a CEL function with documentation and type information.
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
    /// How this function can be called (standalone, method, or both)
    pub kind: FunctionKind,
    /// Arity when called as standalone function. None if not callable standalone.
    pub standalone_arity: Option<Arity>,
    /// Arity when called as a method. None if not callable as method.
    pub method_arity: Option<Arity>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arity_exact_validation() {
        let arity = Arity::Exact(1);
        assert!(!arity.is_valid(0));
        assert!(arity.is_valid(1));
        assert!(!arity.is_valid(2));
        assert_eq!(arity.description(), "1");
    }

    #[test]
    fn arity_range_validation() {
        let arity = Arity::Range(0, 1);
        assert!(arity.is_valid(0));
        assert!(arity.is_valid(1));
        assert!(!arity.is_valid(2));
        assert_eq!(arity.description(), "0 to 1");

        // Same min/max should show single number
        let single = Arity::Range(2, 2);
        assert_eq!(single.description(), "2");
    }
}
