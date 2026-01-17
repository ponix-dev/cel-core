//! Document state management for the CEL LSP.

use std::sync::Arc;

use cel_parser::{ParseError, SpannedExpr};
use dashmap::DashMap;
use tower_lsp::lsp_types::Url;

use crate::protovalidate::extract_cel_regions;
use crate::types::{validate, EmptyResolver, ValidationError};

use super::region::CelRegionState;
use super::text::LineIndex;

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

/// State for a .proto file containing embedded CEL expressions.
#[derive(Debug, Clone)]
pub struct ProtoDocumentState {
    /// Pre-computed line index for the full proto file.
    pub line_index: LineIndex,

    /// All CEL regions extracted from this proto file.
    pub regions: Vec<CelRegionState>,

    /// Document version from the client.
    pub version: i32,
}

impl ProtoDocumentState {
    /// Create a new proto document state by extracting and parsing CEL regions.
    pub fn new(source: String, version: i32) -> Self {
        let line_index = LineIndex::new(source.clone());

        // Extract CEL regions from the proto file
        let extracted = extract_cel_regions(&source);

        // Parse and validate each region
        let regions = extracted
            .into_iter()
            .map(|ext| {
                let (region, mapper) = ext.into_region_and_mapper();
                CelRegionState::new(region, mapper)
            })
            .collect();

        Self {
            line_index,
            regions,
            version,
        }
    }

    /// Find the CEL region containing the given host document offset.
    pub fn region_at_offset(&self, host_offset: usize) -> Option<&CelRegionState> {
        self.regions
            .iter()
            .find(|r| r.contains_host_offset(host_offset))
    }
}

/// Unified document state that can be either a pure CEL file or a proto file.
#[derive(Debug, Clone)]
pub enum DocumentKind {
    /// A .cel file containing a single CEL expression.
    Cel(DocumentState),
    /// A .proto file containing embedded CEL expressions.
    Proto(ProtoDocumentState),
}

/// Thread-safe storage for open documents.
#[derive(Debug, Default)]
pub struct DocumentStore {
    documents: DashMap<Url, Arc<DocumentKind>>,
}

impl DocumentStore {
    /// Create a new empty document store.
    pub fn new() -> Self {
        Self {
            documents: DashMap::new(),
        }
    }

    /// Open or update a document with the given source text.
    /// Auto-detects document type based on file extension.
    pub fn open(&self, uri: Url, source: String, version: i32) -> Arc<DocumentKind> {
        let kind = if is_proto_file(&uri) {
            DocumentKind::Proto(ProtoDocumentState::new(source, version))
        } else {
            DocumentKind::Cel(DocumentState::new(source, version))
        };
        let state = Arc::new(kind);
        self.documents.insert(uri, Arc::clone(&state));
        state
    }

    /// Close a document.
    pub fn close(&self, uri: &Url) {
        self.documents.remove(uri);
    }

    /// Get a document's state.
    pub fn get(&self, uri: &Url) -> Option<Arc<DocumentKind>> {
        self.documents.get(uri).map(|r| Arc::clone(&r))
    }
}

/// Check if a URI refers to a .proto file.
fn is_proto_file(uri: &Url) -> bool {
    uri.path().ends_with(".proto")
}
