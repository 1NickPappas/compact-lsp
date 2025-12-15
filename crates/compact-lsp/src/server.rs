// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! The main Language Server implementation.
//!
//! # LSP Lifecycle
//!
//! 1. Editor starts our binary and sends `initialize` request
//! 2. We respond with our capabilities (what features we support)
//! 3. Editor sends `initialized` notification (handshake complete)
//! 4. Normal operation: file events, requests flow both directions
//! 5. Editor sends `shutdown` request, we respond, then `exit` notification

use std::sync::Arc;

use dashmap::DashMap;
use ropey::Rope;
use lsp_types::*;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LanguageServer};

use compact_analyzer::DiagnosticEngine;

/// A document we're tracking (an open file in the editor).
#[derive(Debug, Clone)]
pub struct Document {
    /// The document content, stored as a rope for efficient editing.
    /// Rope is a data structure optimized for text editing:
    /// - O(log N) for insert/delete at any position
    /// - Cheap clones (structural sharing)
    pub content: Rope,

    /// Document version (incremented by editor on each change).
    /// We can use this to detect out-of-order updates.
    pub version: i32,
}

/// The Compact Language Server.
///
/// This struct holds all the state needed by the server:
/// - `client`: Used to send notifications TO the editor (e.g., diagnostics)
/// - `documents`: Map of open files (Uri -> Document)
/// - `diagnostic_engine`: Wraps the compactc compiler
pub struct CompactLanguageServer {
    /// The LSP client - used to send messages TO the editor.
    /// For example, we use this to publish diagnostics.
    client: Client,

    /// Open documents, keyed by their URI.
    /// DashMap is like HashMap but thread-safe without a global lock.
    /// Multiple async tasks can access different documents concurrently.
    documents: Arc<DashMap<String, Document>>,

    /// The diagnostic engine that wraps compactc.
    diagnostic_engine: Arc<DiagnosticEngine>,
}

impl CompactLanguageServer {
    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        let diagnostic_engine = DiagnosticEngine::new();

        if diagnostic_engine.is_available() {
            tracing::info!("Compact compiler found");
        } else {
            tracing::warn!("Compact compiler not found - diagnostics will be unavailable");
        }

        Self {
            client,
            documents: Arc::new(DashMap::new()),
            diagnostic_engine: Arc::new(diagnostic_engine),
        }
    }

    /// Publish diagnostics for a document.
    ///
    /// This is called after file save to show compiler errors in the editor.
    async fn publish_diagnostics(&self, uri: Uri) {
        // Get the document content
        let content = match self.documents.get(&uri.to_string()) {
            Some(doc) => doc.content.to_string(),
            None => return,
        };

        // Run the compiler and get diagnostics
        let diagnostics = self
            .diagnostic_engine
            .diagnose(&uri.to_string(), &content)
            .await;

        // Send diagnostics to the editor
        // The third parameter is the document version (None = latest)
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

/// Implementation of the Language Server Protocol.
///
/// This trait defines all the LSP methods. We implement the ones we support.
/// Methods we don't implement will return "method not found" automatically.
///
/// Note: tower-lsp-server doesn't require #[async_trait] macro anymore!
impl LanguageServer for CompactLanguageServer {
    /// Handle the `initialize` request.
    ///
    /// This is the first message from the editor. We respond with:
    /// - Our capabilities (what features we support)
    /// - Server info (name, version)
    ///
    /// The editor uses this to know what features to enable.
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        tracing::info!("Received initialize request");

        Ok(InitializeResult {
            // Tell the editor what we can do
            capabilities: ServerCapabilities {
                // We want to know when files are opened, changed, saved, closed
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        // We want didOpen and didClose notifications
                        open_close: Some(true),
                        // We want incremental updates (not full document on each keystroke)
                        // INCREMENTAL = editor sends only the changed range
                        // FULL = editor sends entire document content
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        // We want to know when files are saved
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            // We don't need the full text on save (we already have it)
                            include_text: Some(false),
                        })),
                        ..Default::default()
                    },
                )),
                // We'll add more capabilities as we implement features:
                // - completion_provider: for autocomplete
                // - hover_provider: for hover information
                // - definition_provider: for go-to-definition
                // - document_formatting_provider: for format on save
                ..Default::default()
            },
            // Server identification
            server_info: Some(ServerInfo {
                name: "compact-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    /// Handle the `initialized` notification.
    ///
    /// This is sent by the editor after it receives our initialize response.
    /// The handshake is now complete and normal operation can begin.
    async fn initialized(&self, _params: InitializedParams) {
        tracing::info!("Server initialized - handshake complete");

        // We can send a message to the editor's log
        self.client
            .log_message(MessageType::INFO, "Compact LSP server ready")
            .await;
    }

    /// Handle the `shutdown` request.
    ///
    /// The editor is asking us to prepare to exit.
    /// We should clean up resources here.
    async fn shutdown(&self) -> Result<()> {
        tracing::info!("Shutdown requested");
        Ok(())
    }

    /// Handle `textDocument/didOpen` notification.
    ///
    /// The editor opened a file. We receive:
    /// - URI: file path as a URL (file:///path/to/file.compact)
    /// - Text: the full file content
    /// - Version: starts at 0 or 1
    /// - Language ID: "compact"
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        tracing::debug!("Document opened: {}", uri);

        // Store the document
        let rope = Rope::from_str(&params.text_document.text);
        self.documents.insert(
            uri,
            Document {
                content: rope,
                version: params.text_document.version,
            },
        );

        // Run diagnostics on the newly opened file
        self.publish_diagnostics(params.text_document.uri).await;
    }

    /// Handle `textDocument/didChange` notification.
    ///
    /// The user edited the file. We receive the changes:
    /// - With INCREMENTAL sync: array of {range, text} changes
    /// - With FULL sync: single change with full document text
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        tracing::trace!("Document changed: {}", uri);

        // Apply changes to our stored document
        if let Some(mut doc) = self.documents.get_mut(&uri) {
            for change in params.content_changes {
                if let Some(range) = change.range {
                    // Incremental change: we have a range to replace
                    // Convert LSP positions (line, character) to rope indices
                    let start_line = range.start.line as usize;
                    let start_char = range.start.character as usize;
                    let end_line = range.end.line as usize;
                    let end_char = range.end.character as usize;

                    // Rope uses character indices, not byte indices
                    let start_idx = doc.content.line_to_char(start_line) + start_char;
                    let end_idx = doc.content.line_to_char(end_line) + end_char;

                    // Remove old text
                    doc.content.remove(start_idx..end_idx);
                    // Insert new text
                    doc.content.insert(start_idx, &change.text);
                } else {
                    // Full document replacement (shouldn't happen with INCREMENTAL)
                    doc.content = Rope::from_str(&change.text);
                }
            }
            doc.version = params.text_document.version;
        }
    }

    /// Handle `textDocument/didSave` notification.
    ///
    /// The user saved the file. This is when we run the compiler
    /// and publish any diagnostics (errors/warnings).
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        tracing::debug!("Document saved: {:?}", uri);

        // Run diagnostics
        self.publish_diagnostics(uri).await;
    }

    /// Handle `textDocument/didClose` notification.
    ///
    /// The editor closed the file. We should:
    /// - Remove it from our document store
    /// - Clear any diagnostics for this file
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        tracing::debug!("Document closed: {}", uri);

        // Remove from our store
        self.documents.remove(&uri);

        // Clear diagnostics for this file (send empty array)
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }
}
