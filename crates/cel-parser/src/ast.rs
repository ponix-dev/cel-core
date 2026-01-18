//! CEL Abstract Syntax Tree definitions.

/// Source span for error reporting and LSP features.
/// Uses byte offsets into the source string.
pub type Span = std::ops::Range<usize>;

/// AST node with source location and unique ID.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    /// Unique identifier for this node (1-indexed, assigned during parsing)
    pub id: i64,
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(id: i64, node: T, span: Span) -> Self {
        Self { id, node, span }
    }
}

/// A spanned expression.
pub type SpannedExpr = Spanned<Expr>;

/// CEL expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    Null,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),

    // Identifiers
    Ident(String),
    /// Root-scoped identifier (`.name`) - resolves in root scope only
    RootIdent(String),

    // Collections
    List(Vec<SpannedExpr>),
    Map(Vec<(SpannedExpr, SpannedExpr)>),

    // Operations
    Unary {
        op: UnaryOp,
        expr: Box<SpannedExpr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<SpannedExpr>,
        right: Box<SpannedExpr>,
    },
    Ternary {
        cond: Box<SpannedExpr>,
        then_expr: Box<SpannedExpr>,
        else_expr: Box<SpannedExpr>,
    },

    // Access
    Member {
        expr: Box<SpannedExpr>,
        field: String,
    },
    Index {
        expr: Box<SpannedExpr>,
        index: Box<SpannedExpr>,
    },
    Call {
        expr: Box<SpannedExpr>,
        args: Vec<SpannedExpr>,
    },
    /// Struct/message literal: TypeName{field: value, ...}
    /// The type_name is the expression preceding the braces (Ident, RootIdent, or Member chain)
    Struct {
        type_name: Box<SpannedExpr>,
        fields: Vec<(String, SpannedExpr)>,
    },

    /// Placeholder for parse errors (enables partial AST).
    /// Used during error recovery to represent unparseable sections.
    Error,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Arithmetic negation (`-`)
    Neg,
    /// Logical negation (`!`)
    Not,
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    // Membership
    In,

    // Logical
    And,
    Or,
}
