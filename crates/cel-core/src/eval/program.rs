//! Compiled CEL program ready for evaluation.
//!
//! A `Program` combines a parsed/checked AST with a function registry,
//! providing a convenient interface for evaluating expressions.

use std::sync::Arc;

use super::{Activation, EmptyActivation, Evaluator, FunctionRegistry, Value};
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
}

impl Program {
    /// Create a new program from an AST and function registry.
    pub fn new(ast: Arc<Ast>, functions: Arc<FunctionRegistry>) -> Self {
        Self { ast, functions }
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
        let evaluator = Evaluator::new(activation, &self.functions);
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
