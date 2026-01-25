//! Type environment for CEL type checking.
//!
//! The type environment contains variable declarations and function declarations
//! used during type checking.

use std::collections::HashMap;

use cel_core_types::CelType;

use crate::decls::{FunctionDecl, VariableDecl};
use crate::scope::ScopeStack;
use crate::standard_library::STANDARD_LIBRARY;

/// Type environment for CEL type checking (mirrors cel-go `checker.Env`).
///
/// The environment manages:
/// - Variable declarations with their types
/// - Function declarations with their overloads
/// - Scope stack for comprehensions and nested expressions
/// - Container namespace for qualified name resolution
#[derive(Debug)]
pub struct TypeEnv {
    /// Scope stack for variable resolution.
    scopes: ScopeStack,
    /// Function declarations indexed by name.
    functions: HashMap<String, FunctionDecl>,
    /// Container namespace for qualified name resolution.
    container: String,
}

impl TypeEnv {
    /// Create a new empty type environment.
    pub fn new() -> Self {
        Self {
            scopes: ScopeStack::new(),
            functions: HashMap::new(),
            container: String::new(),
        }
    }

    /// Create a new type environment with the CEL standard library.
    pub fn with_standard_library() -> Self {
        let mut env = Self::new();

        // Add standard library functions
        for func in STANDARD_LIBRARY.iter() {
            env.functions.insert(func.name.clone(), func.clone());
        }

        // Add type constants (type values for type checking)
        env.add_type_constant("bool", CelType::Bool);
        env.add_type_constant("int", CelType::Int);
        env.add_type_constant("uint", CelType::UInt);
        env.add_type_constant("double", CelType::Double);
        env.add_type_constant("string", CelType::String);
        env.add_type_constant("bytes", CelType::Bytes);
        env.add_type_constant("list", CelType::list(CelType::Dyn));
        env.add_type_constant("map", CelType::map(CelType::Dyn, CelType::Dyn));
        env.add_type_constant("null_type", CelType::Null);
        env.add_type_constant("type", CelType::type_of(CelType::Dyn));
        env.add_type_constant("dyn", CelType::Dyn);

        env
    }

    /// Add a type constant to the environment.
    fn add_type_constant(&mut self, name: &str, cel_type: CelType) {
        self.scopes.add_variable(name, CelType::type_of(cel_type));
    }

    /// Set the container namespace for qualified name resolution.
    pub fn set_container(&mut self, container: impl Into<String>) {
        self.container = container.into();
    }

    /// Get the container namespace.
    pub fn container(&self) -> &str {
        &self.container
    }

    /// Add a variable to the current scope.
    pub fn add_variable(&mut self, name: &str, cel_type: CelType) {
        self.scopes.add_variable(name, cel_type);
    }

    /// Add a variable declaration to the current scope.
    pub fn add_variable_decl(&mut self, decl: VariableDecl) {
        self.scopes.add_decl(decl);
    }

    /// Add a function declaration to the environment.
    pub fn add_function(&mut self, decl: FunctionDecl) {
        if let Some(existing) = self.functions.get_mut(&decl.name) {
            // Merge overloads
            existing.overloads.extend(decl.overloads);
        } else {
            self.functions.insert(decl.name.clone(), decl);
        }
    }

    /// Enter a new scope (e.g., for comprehensions).
    pub fn enter_scope(&mut self) {
        self.scopes.enter_scope();
    }

    /// Exit the current scope.
    pub fn exit_scope(&mut self) {
        self.scopes.exit_scope();
    }

    /// Resolve a simple identifier to a variable declaration.
    ///
    /// Searches from innermost to outermost scope.
    pub fn resolve_ident(&self, name: &str) -> Option<&VariableDecl> {
        self.scopes.resolve(name)
    }

    /// Look up a function by name.
    pub fn lookup_function(&self, name: &str) -> Option<&FunctionDecl> {
        self.functions.get(name)
    }

    /// Try to resolve a qualified name (e.g., `pkg.Type`).
    ///
    /// This checks for the name in the following order:
    /// 1. As-is
    /// 2. Prepended with container
    pub fn resolve_qualified(&self, name: &str) -> Option<&VariableDecl> {
        // Try as-is first
        if let Some(decl) = self.scopes.resolve(name) {
            return Some(decl);
        }

        // Try with container prefix
        if !self.container.is_empty() {
            let qualified = format!("{}.{}", self.container, name);
            if let Some(decl) = self.scopes.resolve(&qualified) {
                return Some(decl);
            }
        }

        None
    }

    /// Get the current scope depth.
    pub fn scope_depth(&self) -> usize {
        self.scopes.depth()
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_env() {
        let env = TypeEnv::new();
        assert!(env.resolve_ident("x").is_none());
    }

    #[test]
    fn test_add_variable() {
        let mut env = TypeEnv::new();
        env.add_variable("x", CelType::Int);

        let decl = env.resolve_ident("x").unwrap();
        assert_eq!(decl.cel_type, CelType::Int);
    }

    #[test]
    fn test_standard_library() {
        let env = TypeEnv::with_standard_library();

        // Should have standard functions
        assert!(env.lookup_function("_+_").is_some());
        assert!(env.lookup_function("size").is_some());
        assert!(env.lookup_function("contains").is_some());

        // Should have type constants
        assert!(env.resolve_ident("bool").is_some());
        assert!(env.resolve_ident("int").is_some());
    }

    #[test]
    fn test_scope_enter_exit() {
        let mut env = TypeEnv::new();
        env.add_variable("x", CelType::Int);

        env.enter_scope();
        env.add_variable("y", CelType::String);

        // Both should be visible
        assert!(env.resolve_ident("x").is_some());
        assert!(env.resolve_ident("y").is_some());

        env.exit_scope();

        // Only x should be visible
        assert!(env.resolve_ident("x").is_some());
        assert!(env.resolve_ident("y").is_none());
    }

    #[test]
    fn test_scope_shadowing() {
        let mut env = TypeEnv::new();
        env.add_variable("x", CelType::Int);

        env.enter_scope();
        env.add_variable("x", CelType::String);

        // Should see shadowed version
        let decl = env.resolve_ident("x").unwrap();
        assert_eq!(decl.cel_type, CelType::String);

        env.exit_scope();

        // Should see original again
        let decl = env.resolve_ident("x").unwrap();
        assert_eq!(decl.cel_type, CelType::Int);
    }

    #[test]
    fn test_container() {
        let mut env = TypeEnv::new();
        env.set_container("google.protobuf");

        // Add qualified name
        env.add_variable("google.protobuf.Timestamp", CelType::Timestamp);

        // Should resolve via container
        let decl = env.resolve_qualified("Timestamp").unwrap();
        assert_eq!(decl.cel_type, CelType::Timestamp);
    }

    #[test]
    fn test_add_function() {
        let mut env = TypeEnv::new();

        let func = FunctionDecl::new("custom")
            .with_overload(crate::decls::OverloadDecl::function(
                "custom_int",
                vec![CelType::Int],
                CelType::Bool,
            ));

        env.add_function(func);

        let retrieved = env.lookup_function("custom").unwrap();
        assert_eq!(retrieved.name, "custom");
        assert_eq!(retrieved.overloads.len(), 1);
    }
}
