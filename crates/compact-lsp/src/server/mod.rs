//! The main Language Server implementation.
//!
//! # LSP Lifecycle
//!
//! 1. Editor starts our binary and sends `initialize` request
//! 2. We respond with our capabilities (what features we support)
//! 3. Editor sends `initialized` notification (handshake complete)
//! 4. Normal operation: file events, requests flow both directions
//! 5. Editor sends `shutdown` request, we respond, then `exit` notification

mod base;
mod builtins;
mod completion;
mod format;
mod formatting;
mod helpers;
mod imports;
mod lifecycle;
mod navigation;
mod references;
mod resolver;
mod signature;
mod state;
mod stdlib;
mod symbols;
mod utils;
mod validation;
mod workspace;

pub use state::Document;

use base::*;
use format::*;
use helpers::*;

use std::path::Path;
use std::str::FromStr;
use std::sync::Arc;
use std::sync::Mutex;

use compact_analyzer::{
    CompletionSymbol, CompletionSymbolKind, ContractTypeInfo, DiagnosticEngine, FormatterEngine,
    LanguageVersion, MemberBase, ParserEngine, SemanticToken, SymbolLocation, TypeMemberKind,
};
use dashmap::DashMap;
use lsp_types::*;
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LanguageServer};

/// The Compact Language Server.
///
/// This struct holds all the state needed by the server:
/// - `client`: Used to send notifications TO the editor (e.g., diagnostics)
/// - `documents`: Map of open files (Uri -> Document)
/// - `diagnostic_engine`: Wraps the compactc compiler
/// - `formatter_engine`: Wraps the format-compact binary
pub struct CompactLanguageServer {
    /// The LSP client - used to send messages TO the editor.
    client: Client,

    /// Open documents, keyed by their URI.
    documents: Arc<DashMap<String, Document>>,

    /// The diagnostic engine that wraps compactc.
    diagnostic_engine: Arc<DiagnosticEngine>,

    /// The formatter engine that wraps format-compact.
    formatter_engine: Arc<FormatterEngine>,

    /// The language version the detected compiler targets (e.g. `0.23.0` for the
    /// mainnet toolchain 0.31.1). Used to gate registry symbols added in later
    /// language versions. `None` when no compiler is available (gating disabled).
    language_version: Option<LanguageVersion>,

    /// The parser engine for tree-sitter based features.
    parser_engine: Arc<Mutex<ParserEngine>>,

    /// Workspace root URI (captured from initialize params).
    workspace_root: Arc<Mutex<Option<String>>>,

    /// Symbol cache for cross-file completion.
    symbol_cache: Arc<DashMap<String, Vec<CompletionSymbol>>>,

    /// Source cache for cross-file hover and definition.
    source_cache: Arc<DashMap<String, String>>,

    /// Pending semantic diagnostics tasks.
    pending_diagnostics: Arc<DashMap<String, tokio::task::JoinHandle<()>>>,

    /// Reverse dependency map for cross-file error propagation.
    reverse_dependencies: Arc<DashMap<String, Vec<String>>>,
}

impl CompactLanguageServer {
    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        let diagnostic_engine = DiagnosticEngine::new();
        let formatter_engine = FormatterEngine::new();

        if diagnostic_engine.is_available() {
            tracing::info!("Compact compiler found");
        } else {
            tracing::warn!("Compact compiler not found - diagnostics will be unavailable");
        }

        if formatter_engine.is_available() {
            tracing::info!("Compact formatter found");
        } else {
            tracing::warn!("Compact formatter not found - formatting will be unavailable");
        }

        let parser_engine = ParserEngine::new();
        tracing::info!("Tree-sitter parser initialized");

        let language_version = diagnostic_engine.language_version();
        match language_version {
            Some(v) => tracing::info!("Detected language version: {v} (registry gating enabled)"),
            None => tracing::warn!(
                "Language version unavailable - registry gating disabled (all symbols shown)"
            ),
        }

        Self {
            client,
            documents: Arc::new(DashMap::new()),
            diagnostic_engine: Arc::new(diagnostic_engine),
            formatter_engine: Arc::new(formatter_engine),
            language_version,
            parser_engine: Arc::new(Mutex::new(parser_engine)),
            workspace_root: Arc::new(Mutex::new(None)),
            symbol_cache: Arc::new(DashMap::new()),
            source_cache: Arc::new(DashMap::new()),
            pending_diagnostics: Arc::new(DashMap::new()),
            reverse_dependencies: Arc::new(DashMap::new()),
        }
    }

    /// Find an imported symbol by name (with prefix handling).
    /// Prefer the URI the client is using for a file (an open document), so
    /// edits and locations land on the buffer the editor knows about.
    fn preferred_uri(&self, uri: &str) -> String {
        if self.documents.contains_key(uri) {
            return uri.to_string();
        }
        let key = canonical_key(uri);
        for entry in self.documents.iter() {
            if canonical_key(entry.key()) == key {
                return entry.key().clone();
            }
        }
        for entry in self.source_cache.iter() {
            if canonical_key(entry.key()) == key {
                return entry.key().clone();
            }
        }
        uri.to_string()
    }

    /// Content of a file: the open document if present, else the cached scan.
    fn source_for(&self, uri: &str) -> Option<String> {
        if let Some(doc) = self.documents.get(uri) {
            return Some(doc.content.to_string());
        }
        self.source_cache.get(uri).map(|c| c.clone())
    }
}

/// Implementation of the Language Server Protocol.
impl LanguageServer for CompactLanguageServer {
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        self.handle_formatting(params).await
    }
    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        self.handle_range_formatting(params).await
    }

    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.handle_initialize(params).await
    }
    async fn initialized(&self, _params: InitializedParams) {
        self.handle_initialized(_params).await
    }
    async fn shutdown(&self) -> Result<()> {
        self.handle_shutdown().await
    }
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.handle_did_open(params).await
    }
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.handle_did_change(params).await
    }
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.handle_did_save(params).await
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.handle_did_close(params).await
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        self.handle_document_symbol(params).await
    }
    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<WorkspaceSymbolResponse>> {
        self.handle_symbol(params).await
    }
    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        self.handle_folding_range(params).await
    }
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.handle_semantic_tokens_full(params).await
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.handle_hover(params).await
    }
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        self.handle_goto_definition(params).await
    }
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        self.handle_references(params).await
    }
    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        self.handle_prepare_rename(params).await
    }
    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        self.handle_rename(params).await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        self.handle_completion(params).await
    }
    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        self.handle_signature_help(params).await
    }
}

#[cfg(test)]
mod tests;
