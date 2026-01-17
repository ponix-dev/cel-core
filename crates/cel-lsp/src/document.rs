//! Document state management for the CEL LSP.

use std::sync::Arc;

use cel_parser::{ParseError, SpannedExpr};
use dashmap::DashMap;
use tower_lsp::lsp_types::Url;

use crate::text::LineIndex;
use crate::validation::{validate, EmptyResolver, ValidationError};

/// State for a single document.
#[derive(Debug, Clone)]
pub struct DocumentState {
    /// Pre-computed line index for position conversion.
    pub line_index: LineIndex,
    /// The parsed AST (may be partial with Expr::Error nodes).
    pub ast: Option<SpannedExpr>,
    /// Any parse errors encountered.
    pub errors: Vec<ParseError>,
    /// Any validation errors (undefined variables, methods, etc.).
    pub validation_errors: Vec<ValidationError>,
    /// Document version from the client.
    pub version: i32,
}

impl DocumentState {
    /// Create a new document state by parsing and validating the source.
    pub fn new(source: String, version: i32) -> Self {
        let result = cel_parser::parse(&source);
        let line_index = LineIndex::new(source);

        // Run validation if we have an AST
        let validation_errors = result
            .ast
            .as_ref()
            .map(|ast| validate(ast, &EmptyResolver))
            .unwrap_or_default();

        Self {
            line_index,
            ast: result.ast,
            errors: result.errors,
            validation_errors,
            version,
        }
    }

    /// Get the AST if available.
    /// Note: The AST may contain Expr::Error nodes if there were parse errors.
    pub fn ast(&self) -> Option<&SpannedExpr> {
        self.ast.as_ref()
    }
}

/// Thread-safe storage for open documents.
#[derive(Debug, Default)]
pub struct DocumentStore {
    documents: DashMap<Url, Arc<DocumentState>>,
}

impl DocumentStore {
    /// Create a new empty document store.
    pub fn new() -> Self {
        Self {
            documents: DashMap::new(),
        }
    }

    /// Open or update a document with the given source text.
    pub fn open(&self, uri: Url, source: String, version: i32) -> Arc<DocumentState> {
        let state = Arc::new(DocumentState::new(source, version));
        self.documents.insert(uri, Arc::clone(&state));
        state
    }

    /// Close a document.
    pub fn close(&self, uri: &Url) {
        self.documents.remove(uri);
    }

    /// Get a document's state.
    pub fn get(&self, uri: &Url) -> Option<Arc<DocumentState>> {
        self.documents.get(uri).map(|r| Arc::clone(&r))
    }
}
