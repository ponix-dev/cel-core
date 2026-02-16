//! CEL Language Server implementation.

use std::path::PathBuf;
use std::sync::{Arc, OnceLock};

use cel_core::Env;
use cel_core_proto::ProstProtoRegistry;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService};

mod document;
mod lsp;
pub(crate) mod protovalidate;
pub(crate) mod settings;
pub(crate) mod types;

pub use document::{DocumentState, LineIndex, ProtoDocumentState};
pub use lsp::{completion_at_position_proto, proto_to_diagnostics, to_diagnostics};
pub use settings::{build_env_with_protos, discover_settings, load_proto_registry, load_settings};

use document::{DocumentKind, DocumentStore};

pub struct Backend {
    client: Client,
    documents: DocumentStore,
    workspace_root: OnceLock<PathBuf>,
    proto_registry: OnceLock<Option<Arc<ProstProtoRegistry>>>,
    env: OnceLock<Arc<Env>>,
}

impl Backend {
    pub(crate) fn new(client: Client) -> Self {
        Self {
            client,
            documents: DocumentStore::new(),
            workspace_root: OnceLock::new(),
            proto_registry: OnceLock::new(),
            env: OnceLock::new(),
        }
    }

    /// Parse document and publish diagnostics.
    async fn on_document_change(&self, uri: Url, text: String, version: i32) {
        let registry = self.proto_registry.get().and_then(|r| r.clone());
        let env = self.env.get();
        let state = self
            .documents
            .open(uri.clone(), text, version, registry.as_ref(), env);
        self.publish_diagnostics_for(&uri, &state).await;
    }

    /// Publish diagnostics for a document.
    async fn publish_diagnostics_for(&self, uri: &Url, state: &DocumentKind) {
        let (diagnostics, version) = match state {
            DocumentKind::Cel(cel_state) => {
                let diags = lsp::to_diagnostics(
                    &cel_state.errors,
                    cel_state.check_errors(),
                    &cel_state.line_index,
                );
                (diags, cel_state.version)
            }
            DocumentKind::Proto(proto_state) => {
                let diags = lsp::proto_to_diagnostics(proto_state);
                (diags, proto_state.version)
            }
        };

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, Some(version))
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Extract workspace root from params
        let workspace_root = params
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .and_then(|f| f.uri.to_file_path().ok())
            .or_else(|| {
                #[allow(deprecated)]
                params.root_uri.as_ref()?.to_file_path().ok()
            });

        if let Some(root) = workspace_root {
            let _ = self.workspace_root.set(root.clone());

            // Discover settings by walking up the directory tree
            let (settings, settings_dir) = settings::discover_settings(&root);
            let registry = settings::load_proto_registry(&settings, &settings_dir);
            let env = Arc::new(settings::build_env_with_protos(&settings, &settings_dir));
            let _ = self.proto_registry.set(registry);
            let _ = self.env.set(env);
        } else {
            let _ = self.proto_registry.set(None);
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string()]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: lsp::legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            work_done_progress_options: WorkDoneProgressOptions::default(),
                        },
                    ),
                ),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "CEL language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_document_change(
            params.text_document.uri,
            params.text_document.text,
            params.text_document.version,
        )
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // We use FULL sync, so there's exactly one change with the full text
        if let Some(change) = params.content_changes.into_iter().next() {
            self.on_document_change(
                params.text_document.uri,
                change.text,
                params.text_document.version,
            )
            .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.close(&params.text_document.uri);
        // Clear diagnostics
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        match doc.as_ref() {
            DocumentKind::Cel(state) => {
                let Some(ast) = state.ast() else {
                    return Ok(None);
                };
                Ok(lsp::hover_at_position(
                    &state.line_index,
                    ast,
                    state.check_result.as_ref(),
                    position,
                ))
            }
            DocumentKind::Proto(state) => Ok(lsp::hover_at_position_proto(state, position)),
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let Some(doc) = self.documents.get(uri) else {
            eprintln!("[completion] no document found for {}", uri);
            return Ok(None);
        };

        match doc.as_ref() {
            DocumentKind::Cel(state) => Ok(lsp::completion_at_position(
                &state.line_index,
                &state.source,
                &state.env,
                position,
            )),
            DocumentKind::Proto(state) => {
                let host_offset = state.line_index.position_to_offset(position);
                eprintln!(
                    "[completion] proto position={:?} host_offset={:?} regions={}",
                    position,
                    host_offset,
                    state.regions.len()
                );
                if let Some(offset) = host_offset {
                    for (i, r) in state.regions.iter().enumerate() {
                        let start = r.mapper.host_offset();
                        let end = start + r.mapper.host_length(r.region.source.len());
                        eprintln!(
                            "[completion]   region[{}]: host=[{}..{}] source={:?} contains={}",
                            i,
                            start,
                            end,
                            r.region.source,
                            r.contains_host_offset(offset)
                        );
                    }
                }
                let result = lsp::completion_at_position_proto(state, position);
                eprintln!(
                    "[completion] result items={}",
                    result
                        .as_ref()
                        .map(|r| match r {
                            CompletionResponse::Array(items) => items.len(),
                            _ => 0,
                        })
                        .unwrap_or(0)
                );
                Ok(result)
            }
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let tokens = match doc.as_ref() {
            DocumentKind::Cel(state) => {
                let Some(ast) = state.ast() else {
                    return Ok(None);
                };
                lsp::tokens_for_ast(&state.line_index, ast)
            }
            DocumentKind::Proto(state) => lsp::tokens_for_proto(state),
        };

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }
}

pub fn create_service() -> (LspService<Backend>, tower_lsp::ClientSocket) {
    LspService::new(Backend::new)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn service_can_be_created() {
        let (_service, _socket) = create_service();
    }
}
