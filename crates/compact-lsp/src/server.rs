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

use compact_analyzer::{DiagnosticEngine, FormatterEngine, ParserEngine};
use std::sync::Mutex;

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
/// - `formatter_engine`: Wraps the format-compact binary
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

    /// The formatter engine that wraps format-compact.
    formatter_engine: Arc<FormatterEngine>,

    /// The parser engine for tree-sitter based features.
    /// Wrapped in Mutex because tree-sitter Parser is not Send.
    parser_engine: Arc<Mutex<ParserEngine>>,
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

        Self {
            client,
            documents: Arc::new(DashMap::new()),
            diagnostic_engine: Arc::new(diagnostic_engine),
            formatter_engine: Arc::new(formatter_engine),
            parser_engine: Arc::new(Mutex::new(parser_engine)),
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
                // Completion provider for autocomplete
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ".".to_string(),  // Member access
                        ":".to_string(),  // Type annotations
                        "<".to_string(),  // Generic parameters
                    ]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                // Document formatting provider
                document_formatting_provider: Some(OneOf::Left(true)),
                // Document symbols provider (outline view)
                document_symbol_provider: Some(OneOf::Left(true)),
                // Folding ranges provider
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                // We'll add more capabilities as we implement features:
                // - hover_provider: for hover information
                // - definition_provider: for go-to-definition
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

    /// Handle `textDocument/completion` request.
    ///
    /// Provides autocomplete suggestions for:
    /// - Keywords (circuit, ledger, struct, etc.)
    /// - Built-in types (Boolean, Field, Uint, etc.)
    /// - Snippets (circuit template, struct template, etc.)
    async fn completion(
        &self,
        _params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let mut items = Vec::new();

        // Keywords
        let keywords = [
            ("pragma", "Version pragma declaration"),
            ("import", "Import a module"),
            ("export", "Export a declaration"),
            ("module", "Define a module"),
            ("include", "Include a file"),
            ("ledger", "Declare ledger state"),
            ("circuit", "Define a circuit function"),
            ("witness", "Declare a witness function"),
            ("contract", "Declare an external contract"),
            ("struct", "Define a struct type"),
            ("enum", "Define an enum type"),
            ("constructor", "Define a constructor"),
            ("return", "Return from function"),
            ("if", "Conditional statement"),
            ("else", "Else branch"),
            ("for", "For loop"),
            ("of", "Iterator/range keyword"),
            ("assert", "Assertion with error message"),
            ("const", "Constant declaration"),
            ("default", "Default value"),
            ("map", "Map over values"),
            ("fold", "Fold/reduce values"),
            ("disclose", "Disclose a value"),
            ("pad", "Pad a string"),
            ("as", "Type cast"),
            ("pure", "Pure function modifier"),
            ("sealed", "Sealed ledger modifier"),
            ("prefix", "Import prefix"),
        ];

        for (keyword, detail) in keywords {
            items.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some(detail.to_string()),
                insert_text: Some(keyword.to_string()),
                ..Default::default()
            });
        }

        // Built-in types
        let types = [
            ("Boolean", "Boolean type (true/false)"),
            ("Field", "Field arithmetic type"),
            ("Uint", "Unsigned integer with bit size"),
            ("Bytes", "Fixed-size byte array"),
            ("Opaque", "Opaque type wrapper"),
            ("Vector", "Fixed-size vector"),
        ];

        for (type_name, detail) in types {
            items.push(CompletionItem {
                label: type_name.to_string(),
                kind: Some(CompletionItemKind::TYPE_PARAMETER),
                detail: Some(detail.to_string()),
                insert_text: Some(type_name.to_string()),
                ..Default::default()
            });
        }

        // Type snippets with parameters
        let type_snippets = [
            ("Uint<>", "Uint<${1:32}>", "Unsigned integer (e.g., Uint<32>)"),
            ("Bytes<>", "Bytes<${1:32}>", "Byte array (e.g., Bytes<32>)"),
            ("Vector<>", "Vector<${1:10}, ${2:Field}>", "Vector (e.g., Vector<10, Field>)"),
            ("Opaque<>", "Opaque<\"${1:name}\">", "Opaque type (e.g., Opaque<\"mytype\">)"),
        ];

        for (label, snippet, detail) in type_snippets {
            items.push(CompletionItem {
                label: label.to_string(),
                kind: Some(CompletionItemKind::SNIPPET),
                detail: Some(detail.to_string()),
                insert_text: Some(snippet.to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        }

        // Boolean literals
        items.push(CompletionItem {
            label: "true".to_string(),
            kind: Some(CompletionItemKind::CONSTANT),
            detail: Some("Boolean true".to_string()),
            ..Default::default()
        });
        items.push(CompletionItem {
            label: "false".to_string(),
            kind: Some(CompletionItemKind::CONSTANT),
            detail: Some("Boolean false".to_string()),
            ..Default::default()
        });

        // Code snippets
        let snippets = [
            (
                "circuit",
                "circuit snippet",
                "circuit ${1:name}(${2:params}): ${3:ReturnType} {\n\t$0\n}",
                "Circuit function template",
            ),
            (
                "export circuit",
                "export circuit snippet",
                "export circuit ${1:name}(${2:params}): ${3:ReturnType} {\n\t$0\n}",
                "Exported circuit function template",
            ),
            (
                "pure circuit",
                "pure circuit snippet",
                "export pure circuit ${1:name}(${2:params}): ${3:ReturnType} {\n\t$0\n}",
                "Pure circuit function template",
            ),
            (
                "struct",
                "struct snippet",
                "struct ${1:Name} {\n\t${2:field}: ${3:Type};\n}",
                "Struct definition template",
            ),
            (
                "export struct",
                "export struct snippet",
                "export struct ${1:Name} {\n\t${2:field}: ${3:Type};\n}",
                "Exported struct definition template",
            ),
            (
                "enum",
                "enum snippet",
                "enum ${1:Name} {\n\t${2:Variant1},\n\t${3:Variant2},\n}",
                "Enum definition template",
            ),
            (
                "ledger",
                "ledger snippet",
                "ledger ${1:name}: ${2:Type};",
                "Ledger declaration template",
            ),
            (
                "witness",
                "witness snippet",
                "witness ${1:name}(${2:params}): ${3:ReturnType};",
                "Witness declaration template",
            ),
            (
                "constructor",
                "constructor snippet",
                "constructor(${1:params}) {\n\t$0\n}",
                "Constructor template",
            ),
            (
                "if",
                "if snippet",
                "if (${1:condition}) {\n\t$0\n}",
                "If statement template",
            ),
            (
                "if-else",
                "if-else snippet",
                "if (${1:condition}) {\n\t$2\n} else {\n\t$0\n}",
                "If-else statement template",
            ),
            (
                "for",
                "for snippet",
                "for (const ${1:i} of ${2:0}..${3:10}) {\n\t$0\n}",
                "For loop template",
            ),
            (
                "assert",
                "assert snippet",
                "assert ${1:condition} \"${2:error message}\";",
                "Assertion template",
            ),
            (
                "pragma",
                "pragma snippet",
                "pragma ${1:compact} ${2:>=0.1.0};",
                "Pragma declaration template",
            ),
            (
                "import",
                "import snippet",
                "import ${1:Module};",
                "Import statement template",
            ),
        ];

        for (label, filter, snippet, detail) in snippets {
            items.push(CompletionItem {
                label: label.to_string(),
                kind: Some(CompletionItemKind::SNIPPET),
                detail: Some(detail.to_string()),
                filter_text: Some(filter.to_string()),
                insert_text: Some(snippet.to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        }

        tracing::debug!("Returning {} completion items", items.len());
        Ok(Some(CompletionResponse::Array(items)))
    }

    /// Handle `textDocument/formatting` request.
    ///
    /// Formats the entire document using the format-compact binary.
    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.to_string();
        tracing::debug!("Formatting requested for: {}", uri);

        // Get the document content
        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => {
                tracing::warn!("Document not found: {}", uri);
                return Ok(None);
            }
        };

        // Run the formatter
        let formatted = match self.formatter_engine.format(&content).await {
            Ok(formatted) => formatted,
            Err(e) => {
                tracing::warn!("Formatting failed: {}", e);
                // Show error to user
                self.client
                    .show_message(MessageType::ERROR, format!("Formatting failed: {}", e))
                    .await;
                return Ok(None);
            }
        };

        // If content is unchanged, return empty edits
        if formatted == content {
            tracing::debug!("Content unchanged after formatting");
            return Ok(Some(vec![]));
        }

        // Calculate the range of the entire document
        let line_count = content.lines().count();
        let last_line = content.lines().last().unwrap_or("");

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: line_count as u32,
                character: last_line.len() as u32,
            },
        };

        // Return a single edit that replaces the entire document
        let edit = TextEdit {
            range,
            new_text: formatted,
        };

        tracing::debug!("Formatting complete");
        Ok(Some(vec![edit]))
    }

    /// Handle `textDocument/documentSymbol` request.
    ///
    /// Returns a hierarchical list of symbols (functions, types, etc.) in the document.
    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri.to_string();
        tracing::debug!("Document symbols requested for: {}", uri);

        // Get the document content
        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => {
                tracing::warn!("Document not found: {}", uri);
                return Ok(None);
            }
        };

        // Parse and extract symbols
        let symbols = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.document_symbols(&content)
        };

        tracing::debug!("Found {} symbols", symbols.len());
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    /// Handle `textDocument/foldingRange` request.
    ///
    /// Returns a list of folding ranges in the document.
    async fn folding_range(
        &self,
        params: FoldingRangeParams,
    ) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri.to_string();
        tracing::debug!("Folding ranges requested for: {}", uri);

        // Get the document content
        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => {
                tracing::warn!("Document not found: {}", uri);
                return Ok(None);
            }
        };

        // Parse and extract folding ranges
        let ranges = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.folding_ranges(&content)
        };

        tracing::debug!("Found {} folding ranges", ranges.len());
        Ok(Some(ranges))
    }
}
