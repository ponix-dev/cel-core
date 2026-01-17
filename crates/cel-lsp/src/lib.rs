//! CEL Language Server implementation.

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService};

mod diagnostics;
mod document;
mod hover;
mod semantic_tokens;
mod text;
mod validation;

use document::{DocumentState, DocumentStore};

pub struct Backend {
    client: Client,
    documents: DocumentStore,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DocumentStore::new(),
        }
    }

    /// Parse document and publish diagnostics.
    async fn on_document_change(&self, uri: Url, text: String, version: i32) {
        let state = self.documents.open(uri.clone(), text, version);
        self.publish_diagnostics_for(&uri, &state).await;
    }

    /// Publish diagnostics for a document.
    async fn publish_diagnostics_for(&self, uri: &Url, state: &DocumentState) {
        let diagnostics = diagnostics::to_diagnostics(
            &state.errors,
            &state.validation_errors,
            &state.line_index,
        );

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, Some(state.version))
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: semantic_tokens::legend(),
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

        let Some(ast) = doc.ast() else {
            return Ok(None);
        };

        Ok(hover::hover_at_position(
            &doc.line_index,
            ast,
            &doc.validation_errors,
            position,
        ))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        let Some(doc) = self.documents.get(uri) else {
            return Ok(None);
        };

        let Some(ast) = doc.ast() else {
            return Ok(None);
        };

        let tokens = semantic_tokens::tokens_for_ast(&doc.line_index, ast);

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
