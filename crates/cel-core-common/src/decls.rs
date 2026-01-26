//! Declaration types for variables, functions, and overloads.
//!
//! These types are shared between the checker (type checking) and evaluator (runtime).
//! They mirror cel-go's `checker/decls.go` and define the type environment
//! for CEL expressions.

use crate::{CelType, CelValue};

/// Variable declaration (mirrors cel-go `decls.VariableDecl`).
///
/// Represents a variable or constant that can be referenced in CEL expressions.
#[derive(Debug, Clone)]
pub struct VariableDecl {
    /// The variable name.
    pub name: String,
    /// The CEL type of the variable.
    pub cel_type: CelType,
    /// For enum constants, the compile-time value.
    pub const_value: Option<CelValue>,
}

impl VariableDecl {
    /// Create a new variable declaration.
    pub fn new(name: impl Into<String>, cel_type: CelType) -> Self {
        Self {
            name: name.into(),
            cel_type,
            const_value: None,
        }
    }

    /// Create a constant declaration with a compile-time value.
    pub fn constant(name: impl Into<String>, cel_type: CelType, value: CelValue) -> Self {
        Self {
            name: name.into(),
            cel_type,
            const_value: Some(value),
        }
    }
}

/// Function overload declaration (mirrors cel-go `decls.OverloadDecl`).
///
/// Represents a single signature for a function. Functions can have multiple
/// overloads with different parameter types.
#[derive(Debug, Clone)]
pub struct OverloadDecl {
    /// Unique identifier for this overload (e.g., "add_int64_int64").
    pub id: String,
    /// Parameter types (receiver first for member functions).
    pub params: Vec<CelType>,
    /// Return type.
    pub result: CelType,
    /// Whether this is a member function (receiver.method(args)).
    pub is_member: bool,
    /// Type parameters for generic functions (e.g., ["T", "K", "V"]).
    pub type_params: Vec<String>,
}

impl OverloadDecl {
    /// Create a new standalone function overload.
    pub fn function(id: impl Into<String>, params: Vec<CelType>, result: CelType) -> Self {
        Self {
            id: id.into(),
            params,
            result,
            is_member: false,
            type_params: Vec::new(),
        }
    }

    /// Create a new member function overload.
    ///
    /// The first parameter in `params` is the receiver type.
    pub fn method(id: impl Into<String>, params: Vec<CelType>, result: CelType) -> Self {
        Self {
            id: id.into(),
            params,
            result,
            is_member: true,
            type_params: Vec::new(),
        }
    }

    /// Add type parameters for generic functions.
    pub fn with_type_params(mut self, params: Vec<String>) -> Self {
        self.type_params = params;
        self
    }

    /// Get the receiver type for member functions.
    pub fn receiver_type(&self) -> Option<&CelType> {
        if self.is_member && !self.params.is_empty() {
            Some(&self.params[0])
        } else {
            None
        }
    }

    /// Get the argument types (excluding receiver for member functions).
    pub fn arg_types(&self) -> &[CelType] {
        if self.is_member && !self.params.is_empty() {
            &self.params[1..]
        } else {
            &self.params
        }
    }
}

/// Function declaration with overloads (mirrors cel-go `decls.FunctionDecl`).
///
/// A function can have multiple overloads with different parameter types.
/// During type checking, overload resolution selects the appropriate signature.
#[derive(Debug, Clone)]
pub struct FunctionDecl {
    /// The function name.
    pub name: String,
    /// All overloads for this function.
    pub overloads: Vec<OverloadDecl>,
}

impl FunctionDecl {
    /// Create a new function declaration with no overloads.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            overloads: Vec::new(),
        }
    }

    /// Add an overload to this function.
    pub fn with_overload(mut self, overload: OverloadDecl) -> Self {
        self.overloads.push(overload);
        self
    }

    /// Add multiple overloads to this function.
    pub fn with_overloads(mut self, overloads: impl IntoIterator<Item = OverloadDecl>) -> Self {
        self.overloads.extend(overloads);
        self
    }

    /// Check if this function has any member overloads.
    pub fn has_member_overloads(&self) -> bool {
        self.overloads.iter().any(|o| o.is_member)
    }

    /// Check if this function has any standalone overloads.
    pub fn has_standalone_overloads(&self) -> bool {
        self.overloads.iter().any(|o| !o.is_member)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variable_decl() {
        let var = VariableDecl::new("x", CelType::Int);
        assert_eq!(var.name, "x");
        assert_eq!(var.cel_type, CelType::Int);
        assert!(var.const_value.is_none());
    }

    #[test]
    fn test_overload_decl_function() {
        let overload =
            OverloadDecl::function("add_int64_int64", vec![CelType::Int, CelType::Int], CelType::Int);
        assert_eq!(overload.id, "add_int64_int64");
        assert!(!overload.is_member);
        assert!(overload.receiver_type().is_none());
        assert_eq!(overload.arg_types(), &[CelType::Int, CelType::Int]);
    }

    #[test]
    fn test_overload_decl_method() {
        let overload = OverloadDecl::method(
            "string_contains_string",
            vec![CelType::String, CelType::String],
            CelType::Bool,
        );
        assert_eq!(overload.id, "string_contains_string");
        assert!(overload.is_member);
        assert_eq!(overload.receiver_type(), Some(&CelType::String));
        assert_eq!(overload.arg_types(), &[CelType::String]);
    }

    #[test]
    fn test_function_decl() {
        let func = FunctionDecl::new("size")
            .with_overload(OverloadDecl::function("size_string", vec![CelType::String], CelType::Int))
            .with_overload(OverloadDecl::method("string_size", vec![CelType::String], CelType::Int));

        assert_eq!(func.name, "size");
        assert_eq!(func.overloads.len(), 2);
        assert!(func.has_member_overloads());
        assert!(func.has_standalone_overloads());
    }
}
