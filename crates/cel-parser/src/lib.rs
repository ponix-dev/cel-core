//! CEL (Common Expression Language) parser.

pub mod ast;
mod lexer;
mod parser;

pub use ast::{BinaryOp, Expr, Span, Spanned, SpannedExpr, UnaryOp};

/// A parse error with source location.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for ParseError {}

/// Result of parsing a CEL expression.
///
/// Supports error recovery: may return both an AST and errors.
/// The AST may contain `Expr::Error` nodes where parsing failed.
#[derive(Debug, Clone)]
pub struct ParseResult {
    /// The parsed AST, if any parsing succeeded.
    /// May contain `Expr::Error` nodes for unparseable sections.
    pub ast: Option<SpannedExpr>,
    /// Any parse errors encountered.
    pub errors: Vec<ParseError>,
}

impl ParseResult {
    /// Returns true if parsing completed without errors.
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty() && self.ast.is_some()
    }

    /// Returns true if there are any parse errors.
    pub fn is_err(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Converts to a Result, discarding partial AST on error.
    pub fn into_result(self) -> Result<SpannedExpr, Vec<ParseError>> {
        if self.errors.is_empty() {
            self.ast.ok_or_else(Vec::new)
        } else {
            Err(self.errors)
        }
    }

    /// Unwraps the errors, panicking if there are none.
    pub fn unwrap_err(self) -> Vec<ParseError> {
        if self.errors.is_empty() {
            panic!("called unwrap_err on a ParseResult with no errors");
        }
        self.errors
    }
}

/// Parse a CEL expression from source.
///
/// Returns a `ParseResult` which may contain both a partial AST and errors
/// when error recovery is successful.
pub fn parse(input: &str) -> ParseResult {
    // First, lex the input
    let tokens = match lexer::lex(input) {
        Ok(tokens) => tokens,
        Err(e) => {
            return ParseResult {
                ast: None,
                errors: vec![ParseError {
                    message: e.message,
                    span: e.span,
                }],
            };
        }
    };

    // Parse the tokens
    let (ast, parse_errors) = parser::parse_tokens(&tokens);

    let errors: Vec<ParseError> = parse_errors
        .into_iter()
        .map(|e| ParseError {
            message: e.message,
            span: e.span,
        })
        .collect();

    ParseResult { ast, errors }
}
