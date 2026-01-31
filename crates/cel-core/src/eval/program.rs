//! Compiled CEL program ready for evaluation.
//!
//! A `Program` combines a parsed/checked AST with a function registry,
//! providing a convenient interface for evaluating expressions.

use std::collections::HashMap;
use std::sync::Arc;

use super::{Activation, EmptyActivation, Evaluator, FunctionRegistry, Value};
use crate::types::ProtoTypeRegistry;
use crate::Ast;

/// A compiled CEL program ready for evaluation.
///
/// The program holds a reference to the AST and a function registry,
/// providing a convenient interface for evaluating the expression
/// against different variable bindings.
#[derive(Clone)]
pub struct Program {
    ast: Arc<Ast>,
    functions: Arc<FunctionRegistry>,
    proto_types: Option<Arc<ProtoTypeRegistry>>,
    abbreviations: Option<Arc<HashMap<String, String>>>,
    strong_enums: bool,
}

impl Program {
    /// Create a new program from an AST and function registry.
    pub fn new(ast: Arc<Ast>, functions: Arc<FunctionRegistry>) -> Self {
        Self {
            ast,
            functions,
            proto_types: None,
            abbreviations: None,
            strong_enums: true,
        }
    }

    /// Create a new program with proto type registry.
    pub fn with_proto_types(
        ast: Arc<Ast>,
        functions: Arc<FunctionRegistry>,
        proto_types: Arc<ProtoTypeRegistry>,
    ) -> Self {
        Self {
            ast,
            functions,
            proto_types: Some(proto_types),
            abbreviations: None,
            strong_enums: true,
        }
    }

    /// Create a new program with abbreviations.
    pub fn with_abbreviations(
        ast: Arc<Ast>,
        functions: Arc<FunctionRegistry>,
        abbreviations: HashMap<String, String>,
    ) -> Self {
        Self {
            ast,
            functions,
            proto_types: None,
            abbreviations: Some(Arc::new(abbreviations)),
            strong_enums: true,
        }
    }

    /// Create a new program with proto type registry and abbreviations.
    pub fn with_proto_types_and_abbreviations(
        ast: Arc<Ast>,
        functions: Arc<FunctionRegistry>,
        proto_types: Arc<ProtoTypeRegistry>,
        abbreviations: HashMap<String, String>,
    ) -> Self {
        Self {
            ast,
            functions,
            proto_types: Some(proto_types),
            abbreviations: Some(Arc::new(abbreviations)),
            strong_enums: true,
        }
    }

    /// Use legacy (weak) enum mode where enum values are returned as plain integers.
    pub fn with_legacy_enums(mut self) -> Self {
        self.strong_enums = false;
        self
    }

    /// Get the AST for this program.
    pub fn ast(&self) -> &Ast {
        &self.ast
    }

    /// Get the function registry for this program.
    pub fn functions(&self) -> &FunctionRegistry {
        &self.functions
    }

    /// Evaluate the program with the given variable bindings.
    pub fn eval(&self, activation: &dyn Activation) -> Value {
        self.eval_with_container(activation, "")
    }

    /// Evaluate the program with the given variable bindings and container namespace.
    ///
    /// The container is used for resolving unqualified type names following
    /// C++ namespace rules. For example, with container "cel.expr.conformance.proto3"
    /// and type name "TestAllTypes", resolution tries:
    /// 1. cel.expr.conformance.proto3.TestAllTypes
    /// 2. cel.expr.conformance.TestAllTypes
    /// 3. cel.expr.TestAllTypes
    /// 4. cel.TestAllTypes
    /// 5. TestAllTypes
    pub fn eval_with_container(&self, activation: &dyn Activation, container: &str) -> Value {
        let mut evaluator = Evaluator::new(activation, &self.functions);

        // Pass type info from checked AST
        if let Some(type_info) = self.ast.type_info() {
            evaluator = evaluator.with_reference_map(&type_info.reference_map);
        }

        // Pass proto type registry
        if let Some(ref proto_types) = self.proto_types {
            evaluator = evaluator.with_proto_types(proto_types);
        }

        // Set container for type resolution
        if !container.is_empty() {
            evaluator = evaluator.with_container(container);
        }

        // Pass abbreviations
        if let Some(ref abbreviations) = self.abbreviations {
            evaluator = evaluator.with_abbreviations(abbreviations);
        }

        // Pass strong enum setting
        if !self.strong_enums {
            evaluator = evaluator.with_legacy_enums();
        }

        evaluator.eval(self.ast.expr())
    }

    /// Evaluate the program with no variable bindings.
    pub fn eval_empty(&self) -> Value {
        self.eval(&EmptyActivation)
    }
}

impl std::fmt::Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Program")
            .field("ast", &self.ast)
            .field("functions", &format!("{} functions", self.functions.len()))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::MapActivation;
    use crate::parse;

    fn create_program(source: &str) -> Program {
        let result = parse(source);
        assert!(result.errors.is_empty());
        let ast = Ast::new_unchecked(result.ast.unwrap(), source);
        Program::new(Arc::new(ast), Arc::new(FunctionRegistry::new()))
    }

    #[test]
    fn test_eval_literal() {
        let program = create_program("42");
        assert_eq!(program.eval_empty(), Value::Int(42));
    }

    #[test]
    fn test_eval_with_variables() {
        let program = create_program("x + y");
        let mut activation = MapActivation::new();
        activation.insert("x", Value::Int(1));
        activation.insert("y", Value::Int(2));
        assert_eq!(program.eval(&activation), Value::Int(3));
    }

    #[test]
    fn test_reuse_program() {
        let program = create_program("x * 2");

        let mut act1 = MapActivation::new();
        act1.insert("x", Value::Int(5));
        assert_eq!(program.eval(&act1), Value::Int(10));

        let mut act2 = MapActivation::new();
        act2.insert("x", Value::Int(21));
        assert_eq!(program.eval(&act2), Value::Int(42));
    }
}
