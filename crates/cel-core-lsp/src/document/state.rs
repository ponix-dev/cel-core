//! Document state management for the CEL LSP.

use std::sync::Arc;

use cel_core::{parse, CheckError, CheckResult, Env, ParseError, SpannedExpr};
use cel_core_proto::ProstProtoRegistry;
use dashmap::DashMap;
use tower_lsp::lsp_types::Url;

use crate::protovalidate::extract_cel_regions;

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
    /// Check result from type checking (contains errors and type info).
    pub check_result: Option<CheckResult>,
    /// Document version from the client.
    pub version: i32,
    /// The original source text (needed for completion re-parsing).
    pub source: String,
    /// The environment used for type checking (needed for completion).
    pub env: Arc<Env>,
}

impl DocumentState {
    /// Create a new document state by parsing and type-checking the source.
    pub fn new(source: String, version: i32) -> Self {
        let env = Arc::new(Env::with_standard_library().with_all_extensions());
        Self::with_env(source, version, env)
    }

    /// Create a new document state with a custom Env.
    pub fn with_env(source: String, version: i32, env: Arc<Env>) -> Self {
        let result = parse(&source);
        let line_index = LineIndex::new(source.clone());

        // Run type checking if we have an AST
        let check_result = result.ast.as_ref().map(|ast| env.check(ast));

        Self {
            line_index,
            ast: result.ast,
            errors: result.errors,
            check_result,
            version,
            source,
            env,
        }
    }

    /// Get the AST if available.
    /// Note: The AST may contain Expr::Error nodes if there were parse errors.
    pub fn ast(&self) -> Option<&SpannedExpr> {
        self.ast.as_ref()
    }

    /// Get the check errors if any.
    pub fn check_errors(&self) -> &[CheckError] {
        self.check_result
            .as_ref()
            .map(|r| r.errors.as_slice())
            .unwrap_or(&[])
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
    pub fn new(
        source: String,
        version: i32,
        proto_registry: Option<&Arc<ProstProtoRegistry>>,
    ) -> Self {
        let line_index = LineIndex::new(source.clone());

        // Extract CEL regions from the proto file
        let extracted = extract_cel_regions(&source);

        // Parse and validate each region with its context
        let regions = extracted
            .into_iter()
            .map(|ext| {
                let context = ext.context.clone();
                let (region, mapper) = ext.into_region_and_mapper();
                CelRegionState::with_context(region, mapper, context, proto_registry)
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
    Cel(Box<DocumentState>),
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
    ///
    /// If `env` is provided, `.cel` files will use it instead of the default environment.
    pub fn open(
        &self,
        uri: Url,
        source: String,
        version: i32,
        proto_registry: Option<&Arc<ProstProtoRegistry>>,
        env: Option<&Arc<Env>>,
    ) -> Arc<DocumentKind> {
        let kind = if is_proto_file(&uri) {
            DocumentKind::Proto(ProtoDocumentState::new(source, version, proto_registry))
        } else if let Some(env) = env {
            DocumentKind::Cel(Box::new(DocumentState::with_env(
                source,
                version,
                Arc::clone(env),
            )))
        } else {
            DocumentKind::Cel(Box::new(DocumentState::new(source, version)))
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
