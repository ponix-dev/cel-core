//! CEL conformance testing for cel-parser.
//!
//! This crate implements conformance testing using the official CEL spec test data.
//! It follows the in-process trait-based approach used by cel-go and cel-cpp.

pub mod loader;
pub mod service;

pub use service::CelConformanceService;

use cel_core_proto::gen::cel::expr::{CheckedExpr, ExprValue, ParsedExpr};

// Re-export commonly used types
pub use cel_core_proto::gen::cel::expr::conformance::test::{
    SimpleTest, SimpleTestFile, SimpleTestSection,
};
pub use loader::{load_test_file, LoadError};

/// An issue encountered during parsing, checking, or evaluation.
#[derive(Debug, Clone)]
pub struct Issue {
    /// Human-readable message describing the issue.
    pub message: String,
    /// Position in source where the issue occurred (if applicable).
    pub position: Option<SourcePosition>,
    /// Severity of the issue.
    pub severity: IssueSeverity,
}

/// Source position information.
#[derive(Debug, Clone)]
pub struct SourcePosition {
    /// Line number (1-indexed).
    pub line: i32,
    /// Column number (1-indexed).
    pub column: i32,
}

/// Severity level for issues.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IssueSeverity {
    /// Informational message.
    Info,
    /// Warning that doesn't prevent compilation.
    Warning,
    /// Error that prevents compilation.
    Error,
}

impl Issue {
    /// Create a new error issue with just a message.
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            position: None,
            severity: IssueSeverity::Error,
        }
    }

    /// Create an error issue with position information.
    pub fn error_at(message: impl Into<String>, line: i32, column: i32) -> Self {
        Self {
            message: message.into(),
            position: Some(SourcePosition { line, column }),
            severity: IssueSeverity::Error,
        }
    }

    /// Create an "unimplemented" error for stubbed functionality.
    pub fn unimplemented(feature: &str) -> Self {
        Self::error(format!("{} is not yet implemented", feature))
    }
}

/// Response from parsing a CEL expression.
#[derive(Debug)]
pub struct ParseResponse {
    /// The parsed expression, if parsing succeeded.
    pub parsed_expr: Option<ParsedExpr>,
    /// Any issues encountered during parsing.
    pub issues: Vec<Issue>,
}

impl ParseResponse {
    /// Check if parsing was successful (no errors).
    pub fn is_ok(&self) -> bool {
        self.parsed_expr.is_some()
            && !self.issues.iter().any(|i| i.severity == IssueSeverity::Error)
    }
}

/// Response from type-checking an expression.
#[derive(Debug)]
pub struct CheckResponse {
    /// The type-checked expression, if checking succeeded.
    pub checked_expr: Option<CheckedExpr>,
    /// Any issues encountered during type checking.
    pub issues: Vec<Issue>,
}

impl CheckResponse {
    /// Check if type checking was successful (no errors).
    pub fn is_ok(&self) -> bool {
        self.checked_expr.is_some()
            && !self.issues.iter().any(|i| i.severity == IssueSeverity::Error)
    }
}

/// Response from evaluating an expression.
#[derive(Debug)]
pub struct EvalResponse {
    /// The evaluation result, if evaluation succeeded.
    pub result: Option<ExprValue>,
    /// Any issues encountered during evaluation.
    pub issues: Vec<Issue>,
}

impl EvalResponse {
    /// Check if evaluation was successful (no errors).
    pub fn is_ok(&self) -> bool {
        self.result.is_some()
            && !self.issues.iter().any(|i| i.severity == IssueSeverity::Error)
    }
}

/// Trait for CEL conformance service implementations.
///
/// This trait abstracts over the three phases of CEL expression processing:
/// 1. Parse: Convert source text into an AST
/// 2. Check: Type-check the parsed expression (optional)
/// 3. Eval: Evaluate the expression to produce a value
pub trait ConformanceService {
    /// Parse a CEL expression into a ParsedExpr.
    ///
    /// # Arguments
    /// * `source` - The CEL expression source text
    ///
    /// # Returns
    /// A ParseResponse containing the parsed expression or issues.
    fn parse(&self, source: &str) -> ParseResponse;

    /// Type-check a parsed expression.
    ///
    /// # Arguments
    /// * `parsed` - The parsed expression to check
    /// * `type_env` - Type declarations for variables in the expression
    /// * `container` - Container namespace for qualified name resolution
    ///
    /// # Returns
    /// A CheckResponse containing the checked expression or issues.
    fn check(&self, parsed: &ParsedExpr, type_env: &[TypeDecl], container: &str) -> CheckResponse;

    /// Evaluate an expression with the given bindings.
    ///
    /// # Arguments
    /// * `expr` - The expression to evaluate (either parsed or checked)
    /// * `bindings` - Variable bindings for evaluation
    ///
    /// # Returns
    /// An EvalResponse containing the result or issues.
    fn eval(&self, expr: &ParsedExpr, bindings: &[Binding]) -> EvalResponse;
}

/// A type declaration for a variable in the type environment.
#[derive(Debug, Clone)]
pub struct TypeDecl {
    /// The variable name.
    pub name: String,
    /// The CEL type (using proto Type message).
    pub cel_type: cel_core_proto::gen::cel::expr::Type,
}

/// A variable binding for evaluation.
#[derive(Debug, Clone)]
pub struct Binding {
    /// The variable name.
    pub name: String,
    /// The bound value.
    pub value: cel_core_proto::gen::cel::expr::Value,
}
