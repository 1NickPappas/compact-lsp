//! The main Language Server implementation.
//!
//! # LSP Lifecycle
//!
//! 1. Editor starts our binary and sends `initialize` request
//! 2. We respond with our capabilities (what features we support)
//! 3. Editor sends `initialized` notification (handshake complete)
//! 4. Normal operation: file events, requests flow both directions
//! 5. Editor sends `shutdown` request, we respond, then `exit` notification

mod builtins;
mod imports;
mod state;
mod stdlib;
mod utils;
mod validation;
mod workspace;

pub use state::Document;

use std::str::FromStr;
use std::sync::Arc;
use std::sync::Mutex;

use compact_analyzer::{
    CompletionSymbol, CompletionSymbolKind, DiagnosticEngine, FormatterEngine, ParserEngine,
    SymbolLocation,
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

/// Map a parser symbol kind to the LSP `SymbolKind` used for outline/workspace symbols.
fn completion_kind_to_symbol_kind(kind: CompletionSymbolKind) -> SymbolKind {
    match kind {
        CompletionSymbolKind::Function => SymbolKind::FUNCTION,
        CompletionSymbolKind::Struct => SymbolKind::STRUCT,
        CompletionSymbolKind::Enum => SymbolKind::ENUM,
        CompletionSymbolKind::Variable => SymbolKind::VARIABLE,
        CompletionSymbolKind::Module => SymbolKind::MODULE,
    }
}

/// Convert a parser `SymbolLocation` (0-based line/char) to an LSP `Range`.
fn symbol_location_to_range(loc: &SymbolLocation) -> Range {
    Range {
        start: Position {
            line: loc.start_line,
            character: loc.start_char,
        },
        end: Position {
            line: loc.end_line,
            character: loc.end_char,
        },
    }
}

/// Whether a symbol name matches a workspace-symbol query (case-insensitive
/// substring; an empty query matches everything). `query` must be lowercased.
fn symbol_matches_query(name: &str, query: &str) -> bool {
    query.is_empty() || name.to_lowercase().contains(query)
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
            workspace_root: Arc::new(Mutex::new(None)),
            symbol_cache: Arc::new(DashMap::new()),
            source_cache: Arc::new(DashMap::new()),
            pending_diagnostics: Arc::new(DashMap::new()),
            reverse_dependencies: Arc::new(DashMap::new()),
        }
    }

    /// Publish diagnostics for a document.
    async fn publish_diagnostics(&self, uri: Uri) {
        let content = match self.documents.get(&uri.to_string()) {
            Some(doc) => doc.content.to_string(),
            None => {
                let path = uri.path().as_str();
                match std::fs::read_to_string(path) {
                    Ok(content) => content,
                    Err(_) => return,
                }
            }
        };

        let syntax_diagnostics: Vec<Diagnostic> = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser
                .get_syntax_errors(&content)
                .into_iter()
                .map(|e| Diagnostic {
                    range: e.range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("compact-syntax".to_string()),
                    message: e.message,
                    ..Default::default()
                })
                .collect()
        };

        let compiler_diagnostics = self
            .diagnostic_engine
            .diagnose(&uri.to_string(), &content)
            .await;

        let mut all_diagnostics = syntax_diagnostics;
        all_diagnostics.extend(compiler_diagnostics);

        self.client
            .publish_diagnostics(uri, all_diagnostics, None)
            .await;
    }

    /// Publish syntax diagnostics for a document (instant, on every keystroke).
    async fn publish_syntax_diagnostics(&self, uri: Uri) {
        let content = match self.documents.get(&uri.to_string()) {
            Some(doc) => doc.content.to_string(),
            None => return,
        };

        let syntax_errors = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_syntax_errors(&content)
        };

        let diagnostics: Vec<Diagnostic> = syntax_errors
            .into_iter()
            .map(|e| Diagnostic {
                range: e.range,
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("compact-syntax".to_string()),
                message: e.message,
                ..Default::default()
            })
            .collect();

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    /// Schedule semantic diagnostics with debounce.
    async fn schedule_semantic_diagnostics(&self, uri: Uri, content: String) {
        let uri_string = uri.to_string();

        if let Some((_, handle)) = self.pending_diagnostics.remove(&uri_string) {
            handle.abort();
        }

        let client = self.client.clone();
        let diagnostic_engine = self.diagnostic_engine.clone();
        let parser_engine = self.parser_engine.clone();
        let pending = self.pending_diagnostics.clone();
        let uri_clone = uri_string.clone();

        let handle = tokio::spawn(async move {
            tokio::time::sleep(std::time::Duration::from_millis(500)).await;

            let syntax_diagnostics: Vec<Diagnostic> = {
                let mut parser = parser_engine.lock().unwrap();
                parser
                    .get_syntax_errors(&content)
                    .into_iter()
                    .map(|e| Diagnostic {
                        range: e.range,
                        severity: Some(DiagnosticSeverity::ERROR),
                        source: Some("compact-syntax".to_string()),
                        message: e.message,
                        ..Default::default()
                    })
                    .collect()
            };

            let compiler_diagnostics = diagnostic_engine
                .diagnose_content(&uri_clone, &content)
                .await;

            let mut all_diagnostics = syntax_diagnostics;
            all_diagnostics.extend(compiler_diagnostics);

            client.publish_diagnostics(uri, all_diagnostics, None).await;
            pending.remove(&uri_clone);
        });

        self.pending_diagnostics.insert(uri_string, handle);
    }

    /// Scan workspace for all .compact files and cache their symbols.
    async fn scan_workspace(&self) {
        let root = {
            let guard = self.workspace_root.lock().unwrap();
            match guard.as_ref() {
                Some(uri) => uri.clone(),
                None => {
                    tracing::warn!("No workspace root set, skipping workspace scan");
                    return;
                }
            }
        };

        let root_path = match root.strip_prefix("file://") {
            Some(path) => path,
            None => {
                tracing::warn!("Workspace root is not a file URI: {}", root);
                return;
            }
        };

        tracing::info!("Scanning workspace for .compact files: {}", root_path);

        let mut files_found = 0;
        let mut symbols_found = 0;

        if let Ok(entries) = workspace::find_compact_files(root_path) {
            for file_path in entries {
                let content = match std::fs::read_to_string(&file_path) {
                    Ok(content) => content,
                    Err(e) => {
                        tracing::warn!("Failed to read {}: {}", file_path, e);
                        continue;
                    }
                };

                let symbols = {
                    let mut parser = self.parser_engine.lock().unwrap();
                    parser.get_completion_symbols(&content)
                };

                let canonical_path = std::path::Path::new(&file_path)
                    .canonicalize()
                    .ok()
                    .and_then(|p| p.to_str().map(|s| s.to_string()))
                    .unwrap_or(file_path.clone());
                let uri = format!("file://{}", canonical_path);

                self.source_cache.insert(uri.clone(), content.clone());

                let symbol_count = symbols.len();
                if symbol_count > 0 {
                    self.symbol_cache.insert(uri.clone(), symbols);
                    symbols_found += symbol_count;
                }

                self.update_reverse_dependencies(&uri, &content);
                files_found += 1;
            }
        }

        tracing::info!(
            "Workspace scan complete: {} files, {} symbols cached",
            files_found,
            symbols_found
        );

        let dep_count: usize = self
            .reverse_dependencies
            .iter()
            .map(|e| e.value().len())
            .sum();
        tracing::info!("Reverse dependencies: {} entries", dep_count);
    }

    /// Update the symbol and source cache for a specific file.
    fn update_symbol_cache(&self, uri: &str, content: &str) {
        let symbols = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_completion_symbols(content)
        };

        self.source_cache
            .insert(uri.to_string(), content.to_string());

        if symbols.is_empty() {
            self.symbol_cache.remove(uri);
        } else {
            self.symbol_cache.insert(uri.to_string(), symbols);
        }
    }

    /// Update reverse dependencies for a file based on its imports.
    fn update_reverse_dependencies(&self, uri: &str, content: &str) {
        self.remove_reverse_dependencies(uri);

        let file_imports = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_imports(content)
        };

        for import in file_imports {
            if import.is_file {
                if let Some(imported_uri) = imports::resolve_import_path(uri, &import.path) {
                    self.reverse_dependencies
                        .entry(imported_uri)
                        .or_default()
                        .push(uri.to_string());
                }
            }
        }
    }

    /// Remove a file from all reverse dependency lists.
    fn remove_reverse_dependencies(&self, uri: &str) {
        for mut entry in self.reverse_dependencies.iter_mut() {
            entry.value_mut().retain(|dep| dep != uri);
        }
        self.reverse_dependencies.retain(|_, deps| !deps.is_empty());
    }

    /// Get all files that depend on (import) the given file.
    fn get_dependents(&self, uri: &str) -> Vec<String> {
        self.reverse_dependencies
            .get(uri)
            .map(|deps| deps.value().clone())
            .unwrap_or_default()
    }

    /// Find an imported symbol by name (with prefix handling).
    fn find_imported_symbol(
        &self,
        current_uri: &str,
        name: &str,
    ) -> Option<(String, CompletionSymbol)> {
        let file_imports = {
            let content = self.documents.get(current_uri)?.content.to_string();
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_imports(&content)
        };

        for import in file_imports {
            if !import.is_file {
                continue;
            }

            let prefix = match &import.prefix {
                Some(p) => p,
                None => continue,
            };

            if !name.starts_with(prefix) {
                continue;
            }

            let symbol_name = &name[prefix.len()..];
            if symbol_name.is_empty() {
                continue;
            }

            let resolved_uri = match imports::resolve_import_path(current_uri, &import.path) {
                Some(uri) => uri,
                None => continue,
            };

            if let Some(symbols) = self.symbol_cache.get(&resolved_uri) {
                for symbol in symbols.iter() {
                    if symbol.name == symbol_name {
                        return Some((resolved_uri, symbol.clone()));
                    }
                }
            }
        }

        None
    }

    /// Build a SignatureHelp response from SignatureInfo.
    fn build_signature_help_response(
        &self,
        info: compact_analyzer::SignatureInfo,
    ) -> SignatureHelp {
        let parameters: Vec<ParameterInformation> = info
            .parameters
            .iter()
            .map(|p| ParameterInformation {
                label: ParameterLabel::Simple(p.label.clone()),
                documentation: None,
            })
            .collect();

        let signature = SignatureInformation {
            label: info.label,
            documentation: info.documentation.map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: d,
                })
            }),
            parameters: Some(parameters),
            active_parameter: Some(info.active_parameter),
        };

        SignatureHelp {
            signatures: vec![signature],
            active_signature: Some(0),
            active_parameter: Some(info.active_parameter),
        }
    }

    /// Get the symbol names to search for in a given file (handles import prefixes).
    fn get_search_names_for_file(
        &self,
        searching_file: &str,
        defining_file: &str,
        symbol_name: &str,
    ) -> Vec<String> {
        let mut names = Vec::new();

        let content = match self.source_cache.get(searching_file) {
            Some(c) => c.clone(),
            None => return names,
        };

        let file_imports = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_imports(&content)
        };

        for import in file_imports {
            if !import.is_file {
                continue;
            }
            if let Some(resolved) = imports::resolve_import_path(searching_file, &import.path) {
                if resolved == defining_file {
                    let prefixed_name = match &import.prefix {
                        Some(prefix) => format!("{}{}", prefix, symbol_name),
                        None => symbol_name.to_string(),
                    };
                    names.push(prefixed_name);
                }
            }
        }

        if !names.contains(&symbol_name.to_string()) {
            names.push(symbol_name.to_string());
        }

        names
    }
}

/// Implementation of the Language Server Protocol.
impl LanguageServer for CompactLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        tracing::info!("Received initialize request");

        let workspace_root = params
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .map(|f| f.uri.to_string())
            .or_else(|| {
                #[allow(deprecated)]
                params.root_uri.as_ref().map(|u| u.to_string())
            });

        if let Some(root_str) = workspace_root {
            tracing::info!("Workspace root: {}", root_str);
            let mut guard = self.workspace_root.lock().unwrap();
            *guard = Some(root_str);
        } else {
            tracing::warn!("No workspace root provided by client");
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(false),
                        })),
                        ..Default::default()
                    },
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ".".to_string(),
                        ":".to_string(),
                        "<".to_string(),
                    ]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    SemanticTokenType::FUNCTION,
                                    SemanticTokenType::TYPE,
                                    SemanticTokenType::STRUCT,
                                    SemanticTokenType::ENUM,
                                    SemanticTokenType::ENUM_MEMBER,
                                    SemanticTokenType::PARAMETER,
                                    SemanticTokenType::PROPERTY,
                                    SemanticTokenType::VARIABLE,
                                    SemanticTokenType::NAMESPACE,
                                    SemanticTokenType::TYPE_PARAMETER,
                                ],
                                token_modifiers: vec![
                                    SemanticTokenModifier::DECLARATION,
                                    SemanticTokenModifier::READONLY,
                                    SemanticTokenModifier::DEFAULT_LIBRARY,
                                ],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: Some(false),
                            ..Default::default()
                        },
                    ),
                ),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "compact-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        tracing::info!("Server initialized - handshake complete");

        // Surface the detected compiler/language version so registry-vs-compiler
        // drift (e.g. the LSP registry being ahead of the installed compiler) is
        // visible in the logs.
        let version_msg = match self.diagnostic_engine.version_info() {
            Some((compiler, language)) if !language.is_empty() => {
                format!("Compact compiler {compiler} (language {language})")
            }
            Some((compiler, _)) => format!("Compact compiler {compiler}"),
            None => "Compact compiler version unavailable".to_string(),
        };
        tracing::info!("{version_msg}");

        self.scan_workspace().await;
        self.client
            .log_message(
                MessageType::INFO,
                format!("Compact LSP server ready — {version_msg}"),
            )
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        tracing::info!("Shutdown requested");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        tracing::debug!("Document opened: {}", uri);

        let rope = Rope::from_str(&params.text_document.text);
        self.documents.insert(
            uri.clone(),
            Document {
                content: rope,
                version: params.text_document.version,
            },
        );

        // Index the opened file immediately so it's searchable via workspace
        // symbols and usable for cross-file features even before it's saved.
        self.update_symbol_cache(&uri, &params.text_document.text);
        self.update_reverse_dependencies(&uri, &params.text_document.text);

        self.publish_diagnostics(params.text_document.uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let uri_string = uri.to_string();

        if let Some(mut doc) = self.documents.get_mut(&uri_string) {
            for change in params.content_changes {
                if let Some(range) = change.range {
                    let start_line = range.start.line as usize;
                    let start_char = range.start.character as usize;
                    let end_line = range.end.line as usize;
                    let end_char = range.end.character as usize;

                    let start_idx = doc.content.line_to_char(start_line) + start_char;
                    let end_idx = doc.content.line_to_char(end_line) + end_char;

                    doc.content.remove(start_idx..end_idx);
                    doc.content.insert(start_idx, &change.text);
                } else {
                    doc.content = Rope::from_str(&change.text);
                }
            }
            doc.version = params.text_document.version;
        }

        self.publish_syntax_diagnostics(uri.clone()).await;

        let content = match self.documents.get(&uri_string) {
            Some(doc) => doc.content.to_string(),
            None => return,
        };
        self.schedule_semantic_diagnostics(uri, content).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let uri_str = uri.to_string();

        if let Some(doc) = self.documents.get(&uri_str) {
            let content = doc.content.to_string();
            self.update_symbol_cache(&uri_str, &content);
            self.update_reverse_dependencies(&uri_str, &content);
        }

        self.publish_diagnostics(uri).await;

        let dependents = self.get_dependents(&uri_str);
        if !dependents.is_empty() {
            for dependent_uri in dependents {
                if let Ok(dep_uri) = dependent_uri.parse::<Uri>() {
                    self.publish_diagnostics(dep_uri).await;
                }
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();

        if let Some((_, handle)) = self.pending_diagnostics.remove(&uri) {
            handle.abort();
        }

        self.documents.remove(&uri);

        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;

        // Handle dot-completion for built-in type methods.
        // Detect dot context via trigger character OR by scanning the text.
        let is_dot_trigger = params
            .context
            .as_ref()
            .and_then(|ctx| ctx.trigger_character.as_deref())
            == Some(".");

        if let Some(doc) = self.documents.get(&uri) {
            let content = doc.content.to_string();

            // Try trigger-character-based detection first, then text-based fallback
            let var_name = if is_dot_trigger {
                tracing::debug!("Dot completion: trigger character detected");
                utils::get_dot_access_variable(&content, position.line, position.character)
            } else {
                tracing::debug!("Dot completion: checking text context");
                utils::detect_dot_context(&content, position.line, position.character)
            };

            if let Some(var_name) = var_name {
                tracing::debug!("Dot completion: base variable = {}", var_name);
                let var_type = {
                    let mut parser = self.parser_engine.lock().unwrap();
                    parser.get_variable_type(&content, &var_name)
                };
                // Fallback: `kernel` is implicitly available without a ledger declaration
                let var_type = var_type.or_else(|| {
                    if var_name == "kernel" {
                        Some("Kernel".to_string())
                    } else {
                        None
                    }
                });
                if let Some(type_str) = var_type {
                    let base_type = builtins::extract_base_type(&type_str);
                    tracing::debug!("Dot completion: resolved type = {}", base_type);
                    let methods = builtins::methods_for_type(base_type);
                    if !methods.is_empty() {
                        let items: Vec<CompletionItem> = methods
                            .iter()
                            .map(|m| CompletionItem {
                                label: m.name.to_string(),
                                kind: Some(CompletionItemKind::METHOD),
                                detail: Some(m.signature.to_string()),
                                documentation: Some(Documentation::String(
                                    m.documentation.to_string(),
                                )),
                                insert_text: Some(m.snippet.to_string()),
                                insert_text_format: Some(InsertTextFormat::SNIPPET),
                                ..Default::default()
                            })
                            .collect();
                        return Ok(Some(CompletionResponse::Array(items)));
                    }
                }
            }
        }

        let mut items = Vec::new();

        fn symbol_to_lsp_kind(kind: compact_analyzer::CompletionSymbolKind) -> CompletionItemKind {
            use compact_analyzer::CompletionSymbolKind;
            match kind {
                CompletionSymbolKind::Function => CompletionItemKind::FUNCTION,
                CompletionSymbolKind::Struct => CompletionItemKind::STRUCT,
                CompletionSymbolKind::Enum => CompletionItemKind::ENUM,
                CompletionSymbolKind::Variable => CompletionItemKind::VARIABLE,
                CompletionSymbolKind::Module => CompletionItemKind::MODULE,
            }
        }

        let file_imports = if let Some(doc) = self.documents.get(&uri) {
            let content = doc.content.to_string();

            let symbols = {
                let mut parser = self.parser_engine.lock().unwrap();
                parser.get_completion_symbols(&content)
            };

            for sym in symbols {
                items.push(CompletionItem {
                    label: sym.name.clone(),
                    kind: Some(symbol_to_lsp_kind(sym.kind)),
                    detail: sym.detail,
                    insert_text: Some(sym.name),
                    ..Default::default()
                });
            }

            let file_imports = {
                let mut parser = self.parser_engine.lock().unwrap();
                parser.get_imports(&content)
            };
            file_imports
        } else {
            vec![]
        };

        for import in &file_imports {
            if !import.is_file {
                continue;
            }

            let resolved_uri = match imports::resolve_import_path(&uri, &import.path) {
                Some(uri) => uri,
                None => continue,
            };

            if let Some(entry) = self.symbol_cache.get(&resolved_uri) {
                let prefix = import.prefix.as_deref().unwrap_or("");

                for sym in entry.value().iter() {
                    let prefixed_name = if prefix.is_empty() {
                        sym.name.clone()
                    } else {
                        format!("{}{}", prefix, sym.name)
                    };

                    let source_file = import.path.rsplit('/').next().unwrap_or(&import.path);

                    items.push(CompletionItem {
                        label: prefixed_name.clone(),
                        kind: Some(symbol_to_lsp_kind(sym.kind)),
                        detail: Some(format!(
                            "{} (from {})",
                            sym.detail.as_deref().unwrap_or(""),
                            source_file
                        )),
                        insert_text: Some(prefixed_name),
                        ..Default::default()
                    });
                }
            }
        }

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
            ("type", "Type alias declaration"),
            ("new", "Nominal type alias modifier"),
            ("from", "Selective import source"),
            ("slice", "Byte slice operation"),
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
            ("Counter", "Atomic counter for ledger state"),
            ("Map", "Key-value mapping for ledger state"),
            ("Set", "Set collection for ledger state"),
            ("List", "Ordered list for ledger state"),
            ("MerkleTree", "Merkle tree for ledger state"),
            (
                "HistoricMerkleTree",
                "Historic Merkle tree with root history",
            ),
            ("Kernel", "Built-in kernel operations"),
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

        // Type snippets
        let type_snippets = [
            (
                "Uint<>",
                "Uint<${1:32}>",
                "Unsigned integer (e.g., Uint<32>)",
            ),
            ("Bytes<>", "Bytes<${1:32}>", "Byte array (e.g., Bytes<32>)"),
            (
                "Vector<>",
                "Vector<${1:10}, ${2:Field}>",
                "Vector (e.g., Vector<10, Field>)",
            ),
            (
                "Opaque<>",
                "Opaque<\"${1:name}\">",
                "Opaque type (e.g., Opaque<\"mytype\">)",
            ),
            (
                "Map<>",
                "Map<${1:Key}, ${2:Value}>",
                "Map (e.g., Map<Address, Uint<64>>)",
            ),
            ("Set<>", "Set<${1:T}>", "Set (e.g., Set<Address>)"),
            ("List<>", "List<${1:T}>", "List (e.g., List<Field>)"),
            (
                "MerkleTree<>",
                "MerkleTree<${1:32}, ${2:Field}>",
                "Merkle tree (e.g., MerkleTree<32, Bytes<32>>)",
            ),
            (
                "HistoricMerkleTree<>",
                "HistoricMerkleTree<${1:32}, ${2:Field}>",
                "Historic Merkle tree",
            ),
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

        // Stdlib struct types
        for st in stdlib::all_stdlib_structs() {
            // Show type params in the label (e.g. "Maybe<T>"); empty for
            // non-generic structs. insert_text stays the bare name.
            let label = format!("{}{}", st.name, st.type_params);
            items.push(CompletionItem {
                label,
                kind: Some(CompletionItemKind::STRUCT),
                detail: Some(st.description.to_string()),
                insert_text: Some(st.name.to_string()),
                ..Default::default()
            });
        }

        // Stdlib parameterized type snippets
        let stdlib_type_snippets = [
            ("Maybe<>", "Maybe<${1:T}>", "Optional container (Maybe<T>)"),
            (
                "Either<>",
                "Either<${1:A}, ${2:B}>",
                "Union type (Either<A, B>)",
            ),
            (
                "MerkleTreePath<>",
                "MerkleTreePath<${1:n}, ${2:T}>",
                "Merkle tree path proof",
            ),
        ];

        for (label, snippet, detail) in stdlib_type_snippets {
            items.push(CompletionItem {
                label: label.to_string(),
                kind: Some(CompletionItemKind::SNIPPET),
                detail: Some(detail.to_string()),
                insert_text: Some(snippet.to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            });
        }

        // Stdlib circuit functions
        for circ in stdlib::all_stdlib_circuits() {
            items.push(CompletionItem {
                label: circ.name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(circ.signature.to_string()),
                documentation: Some(Documentation::String(circ.doc.to_string())),
                insert_text: Some(circ.snippet.to_string()),
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
            // Circuit snippets
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
                "pure circuit ${1:name}(${2:params}): ${3:ReturnType} {\n\t$0\n}",
                "Pure circuit function template",
            ),
            (
                "export pure circuit",
                "export pure circuit snippet",
                "export pure circuit ${1:name}(${2:params}): ${3:ReturnType} {\n\t$0\n}",
                "Exported pure circuit function template",
            ),
            // Struct snippets
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
            // Enum snippets
            (
                "enum",
                "enum snippet",
                "enum ${1:Name} {\n\t${2:Variant1},\n\t${3:Variant2},\n}",
                "Enum definition template",
            ),
            (
                "export enum",
                "export enum snippet",
                "export enum ${1:Name} {\n\t${2:Variant1},\n\t${3:Variant2},\n}",
                "Exported enum definition template",
            ),
            // Ledger snippets
            (
                "ledger",
                "ledger snippet",
                "ledger ${1:name}: ${2:Type};",
                "Ledger declaration template",
            ),
            (
                "export ledger",
                "export ledger snippet",
                "export ledger ${1:name}: ${2:Type};",
                "Exported ledger declaration template",
            ),
            (
                "sealed ledger",
                "sealed ledger snippet",
                "sealed ledger ${1:name}: ${2:Type};",
                "Sealed ledger declaration template",
            ),
            (
                "export sealed ledger",
                "export sealed ledger snippet",
                "export sealed ledger ${1:name}: ${2:Type};",
                "Exported sealed ledger declaration template",
            ),
            // Witness snippets
            (
                "witness",
                "witness snippet",
                "witness ${1:name}(${2:params}): ${3:ReturnType};",
                "Witness declaration template",
            ),
            (
                "export witness",
                "export witness snippet",
                "export witness ${1:name}(${2:params}): ${3:ReturnType};",
                "Exported witness declaration template",
            ),
            // Contract snippets
            (
                "contract",
                "contract snippet",
                "contract ${1:Name} {\n\tcircuit ${2:fn}(${3:params}): ${4:ReturnType};\n}",
                "External contract declaration template",
            ),
            (
                "export contract",
                "export contract snippet",
                "export contract ${1:Name} {\n\tcircuit ${2:fn}(${3:params}): ${4:ReturnType};\n}",
                "Exported contract declaration template",
            ),
            // Module snippets
            (
                "module",
                "module snippet",
                "module ${1:Name} {\n\t$0\n}",
                "Module definition template",
            ),
            (
                "export module",
                "export module snippet",
                "export module ${1:Name} {\n\t$0\n}",
                "Exported module definition template",
            ),
            // Other declaration snippets
            (
                "constructor",
                "constructor snippet",
                "constructor(${1:params}) {\n\t$0\n}",
                "Constructor template",
            ),
            (
                "const",
                "const snippet",
                "const ${1:name}: ${2:Type} = ${3:value};",
                "Constant declaration template",
            ),
            (
                "include",
                "include snippet",
                "include \"${1:path}\";",
                "File inclusion template",
            ),
            // Import snippets
            (
                "import",
                "import snippet",
                "import ${1:Module};",
                "Import module template",
            ),
            (
                "import file",
                "import file snippet",
                "import \"${1:path}\";",
                "Import file template",
            ),
            (
                "import prefix",
                "import prefix snippet",
                "import ${1:Module} prefix ${2:alias};",
                "Import with prefix alias template",
            ),
            // Statement snippets
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
            // Pragma snippet
            (
                "pragma",
                "pragma snippet",
                "pragma language_version ${1:>=0.14.0};",
                "Pragma declaration template",
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

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.to_string();

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let formatted = match self.formatter_engine.format(&content).await {
            Ok(formatted) => formatted,
            Err(e) => {
                self.client
                    .show_message(MessageType::ERROR, format!("Formatting failed: {}", e))
                    .await;
                return Ok(None);
            }
        };

        if formatted == content {
            return Ok(Some(vec![]));
        }

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

        Ok(Some(vec![TextEdit {
            range,
            new_text: formatted,
        }]))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri.to_string();

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let symbols = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.document_symbols(&content)
        };

        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<WorkspaceSymbolResponse>> {
        let query = params.query.to_lowercase();
        let mut items = Vec::new();

        for entry in self.symbol_cache.iter() {
            let uri = match entry.key().parse::<Uri>() {
                Ok(uri) => uri,
                Err(_) => continue,
            };
            for sym in entry.value() {
                let Some(location) = &sym.location else {
                    continue;
                };
                if !symbol_matches_query(&sym.name, &query) {
                    continue;
                }
                #[allow(deprecated)]
                items.push(SymbolInformation {
                    name: sym.name.clone(),
                    kind: completion_kind_to_symbol_kind(sym.kind),
                    tags: None,
                    deprecated: None,
                    location: Location {
                        uri: uri.clone(),
                        range: symbol_location_to_range(location),
                    },
                    container_name: None,
                });
            }
        }

        Ok(Some(WorkspaceSymbolResponse::Flat(items)))
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri.to_string();

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let ranges = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.folding_ranges(&content)
        };

        Ok(Some(ranges))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let hover_info = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.hover_info(&content, position.line, position.character)
        };

        if let Some(info) = hover_info {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: info.content,
                }),
                range: info.range,
            }));
        }

        // Check for member access (e.g., hovering on "increment" in "round.increment(1)")
        let member_ctx = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_member_access_context(&content, position.line, position.character)
        };
        if let Some(ctx) = member_ctx {
            let var_type = {
                let mut parser = self.parser_engine.lock().unwrap();
                parser.get_variable_type(&content, &ctx.base_name)
            };
            // Fallback: `kernel` is implicitly available without a ledger declaration
            let var_type = var_type.or_else(|| {
                if ctx.base_name == "kernel" {
                    Some("Kernel".to_string())
                } else {
                    None
                }
            });
            if let Some(type_str) = var_type {
                let base_type = builtins::extract_base_type(&type_str);
                if let Some(method) = builtins::find_method_by_name(base_type, &ctx.member_name) {
                    let hover_text = format!(
                        "```compact\n{}.{}  (on {})\n```\n\n{}",
                        ctx.base_name, method.signature, type_str, method.documentation
                    );
                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hover_text,
                        }),
                        range: Some(ctx.member_range),
                    }));
                }
            }
        }

        let word = match utils::get_word_at_position(&content, position.line, position.character) {
            Some(w) => w,
            None => return Ok(None),
        };

        if let Some((_file_uri, symbol)) = self.find_imported_symbol(&uri, &word) {
            let content = symbol.documentation.unwrap_or_else(|| {
                format!(
                    "```compact\n{}{}\n```",
                    symbol.name,
                    symbol.detail.as_deref().unwrap_or("")
                )
            });
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            }));
        }

        // Stdlib circuit hover (e.g., hovering on "sendShielded")
        if let Some(circ) = stdlib::find_stdlib_circuit(&word) {
            let mut hover_text = format!(
                "```compact\ncircuit {}\n```\n\n{}",
                circ.signature, circ.doc
            );
            if !circ.doc_url.is_empty() {
                hover_text.push_str(&format!(
                    "\n\n> [Compact Standard Library]({})",
                    circ.doc_url
                ));
            }
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_text,
                }),
                range: None,
            }));
        }

        // Stdlib struct hover (e.g., hovering on "ShieldedCoinInfo", "Maybe")
        let base_type = builtins::extract_base_type(&word);
        if let Some(st) = stdlib::find_stdlib_struct(base_type) {
            let mut hover_text = if st.type_params.is_empty() {
                format!("**{}**\n\n{}\n\n", st.name, st.description)
            } else {
                format!(
                    "**{}{}**\n\n{}\n\n",
                    st.name, st.type_params, st.description
                )
            };
            hover_text.push_str("**Fields:**\n");
            for field in &st.fields {
                hover_text.push_str(&format!(
                    "- `{}: {}` — {}\n",
                    field.name, field.type_str, field.doc
                ));
            }
            if !st.doc_url.is_empty() {
                hover_text.push_str(&format!("\n> [Compact Standard Library]({})", st.doc_url));
            }
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_text,
                }),
                range: None,
            }));
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        let uri_string = uri.to_string();
        let position = params.text_document_position_params.position;

        let content = match self.documents.get(&uri_string) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let def_location = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.goto_definition(&content, position.line, position.character)
        };

        if let Some(loc) = def_location {
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri,
                range: loc.selection_range,
            })));
        }

        // Check for member access on built-in types (e.g., "increment" in "round.increment()")
        let member_ctx = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_member_access_context(&content, position.line, position.character)
        };
        if let Some(ctx) = member_ctx {
            let var_type = {
                let mut parser = self.parser_engine.lock().unwrap();
                parser.get_variable_type(&content, &ctx.base_name)
            };
            // Fallback: `kernel` is implicitly available without a ledger declaration
            let var_type = var_type.or_else(|| {
                if ctx.base_name == "kernel" {
                    Some("Kernel".to_string())
                } else {
                    None
                }
            });
            if let Some(type_str) = var_type {
                let base_type = builtins::extract_base_type(&type_str);
                if let Some(doc_loc) =
                    builtins::get_builtin_method_doc_location(base_type, &ctx.member_name)
                {
                    let target_uri = match Uri::from_str(&doc_loc.uri) {
                        Ok(u) => u,
                        Err(_) => return Ok(None),
                    };
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri: target_uri,
                        range: Range {
                            start: Position {
                                line: doc_loc.line,
                                character: 0,
                            },
                            end: Position {
                                line: doc_loc.line,
                                character: 0,
                            },
                        },
                    })));
                }
            }
        }

        let word = match utils::get_word_at_position(&content, position.line, position.character) {
            Some(w) => w,
            None => return Ok(None),
        };

        if let Some((file_uri, symbol)) = self.find_imported_symbol(&uri_string, &word) {
            if let Some(loc) = symbol.location {
                let target_uri = match Uri::from_str(&file_uri) {
                    Ok(u) => u,
                    Err(_) => return Ok(None),
                };
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: target_uri,
                    range: Range {
                        start: Position {
                            line: loc.start_line,
                            character: loc.start_char,
                        },
                        end: Position {
                            line: loc.end_line,
                            character: loc.end_char,
                        },
                    },
                })));
            }
        }

        // Built-in type name lookup (e.g., "Counter" in "ledger round: Counter;")
        let base_type = builtins::extract_base_type(&word);
        if let Some(doc_loc) = builtins::get_builtin_type_doc_location(base_type) {
            let target_uri = match Uri::from_str(&doc_loc.uri) {
                Ok(u) => u,
                Err(_) => return Ok(None),
            };
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: target_uri,
                range: Range {
                    start: Position {
                        line: doc_loc.line,
                        character: 0,
                    },
                    end: Position {
                        line: doc_loc.line,
                        character: 0,
                    },
                },
            })));
        }

        // Stdlib struct type lookup (e.g., "ShieldedCoinInfo", "Maybe<T>")
        if let Some(doc_loc) = stdlib::get_stdlib_struct_doc_location(base_type) {
            let target_uri = match Uri::from_str(&doc_loc.uri) {
                Ok(u) => u,
                Err(_) => return Ok(None),
            };
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: target_uri,
                range: Range {
                    start: Position {
                        line: doc_loc.line,
                        character: 0,
                    },
                    end: Position {
                        line: doc_loc.line,
                        character: 0,
                    },
                },
            })));
        }

        // Stdlib circuit function lookup (e.g., "sendShielded", "transientHash")
        if let Some(doc_loc) = stdlib::get_stdlib_circuit_doc_location(&word) {
            let target_uri = match Uri::from_str(&doc_loc.uri) {
                Ok(u) => u,
                Err(_) => return Ok(None),
            };
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: target_uri,
                range: Range {
                    start: Position {
                        line: doc_loc.line,
                        character: 0,
                    },
                    end: Position {
                        line: doc_loc.line,
                        character: 0,
                    },
                },
            })));
        }

        Ok(None)
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let sig_info = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.signature_help(&content, position.line, position.character)
        };

        if let Some(info) = sig_info {
            return Ok(Some(self.build_signature_help_response(info)));
        }

        let func_name =
            match utils::get_function_call_name(&content, position.line, position.character) {
                Some(name) => name,
                None => return Ok(None),
            };

        if let Some((_file_uri, symbol)) = self.find_imported_symbol(&uri, &func_name) {
            if let Some(detail) = &symbol.detail {
                let active_param =
                    utils::count_commas_before_cursor(&content, position.line, position.character);
                let params = utils::parse_params_from_detail(detail);

                let parameters: Vec<ParameterInformation> = params
                    .iter()
                    .map(|p| ParameterInformation {
                        label: ParameterLabel::Simple(p.clone()),
                        documentation: None,
                    })
                    .collect();

                let label = format!("circuit {}{}", func_name, detail);
                let signature = SignatureInformation {
                    label,
                    documentation: symbol.documentation.map(|d| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: d,
                        })
                    }),
                    parameters: Some(parameters),
                    active_parameter: Some(active_param),
                };

                return Ok(Some(SignatureHelp {
                    signatures: vec![signature],
                    active_signature: Some(0),
                    active_parameter: Some(active_param),
                }));
            }
        }

        // Stdlib circuit function signature help
        if let Some(circ) = stdlib::find_stdlib_circuit(&func_name) {
            let active_param =
                utils::count_commas_before_cursor(&content, position.line, position.character);
            // Build detail string from signature: "sendShielded(input: QualifiedShieldedCoinInfo, ...): ShieldedSendResult" → "(input: QualifiedShieldedCoinInfo, ...): ShieldedSendResult"
            let detail = circ
                .signature
                .find('(')
                .map(|i| &circ.signature[i..])
                .unwrap_or(circ.signature);
            let params = utils::parse_params_from_detail(detail);

            let parameters: Vec<ParameterInformation> = params
                .iter()
                .map(|p| ParameterInformation {
                    label: ParameterLabel::Simple(p.clone()),
                    documentation: None,
                })
                .collect();

            let label = format!("circuit {}", circ.signature);
            let signature = SignatureInformation {
                label,
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: circ.doc.to_string(),
                })),
                parameters: Some(parameters),
                active_parameter: Some(active_param),
            };

            return Ok(Some(SignatureHelp {
                signatures: vec![signature],
                active_signature: Some(0),
                active_parameter: Some(active_param),
            }));
        }

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let tokens = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_semantic_tokens(&content)
        };

        let mut data = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_char = 0u32;

        for token in tokens {
            let line = token.range.start.line;
            let char = token.range.start.character;
            let length = token
                .range
                .end
                .character
                .saturating_sub(token.range.start.character);

            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 {
                char - prev_char
            } else {
                char
            };

            let mut modifier_mask = 0u32;
            for modifier in &token.modifiers {
                modifier_mask |= 1 << (*modifier as u32);
            }

            data.push(lsp_types::SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type: token.token_type as u32,
                token_modifiers_bitset: modifier_mask,
            });

            prev_line = line;
            prev_char = char;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let uri_string = uri.to_string();
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        let content = match self.documents.get(&uri_string) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let symbol_name =
            match utils::get_word_at_position(&content, position.line, position.character) {
                Some(name) => name,
                None => return Ok(None),
            };

        let mut all_locations = Vec::new();

        let local_refs = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.find_references(&content, &symbol_name)
        };

        for r in local_refs {
            if r.is_definition && !include_declaration {
                continue;
            }
            all_locations.push(Location {
                uri: uri.clone(),
                range: r.range,
            });
        }

        for entry in self.source_cache.iter() {
            let file_uri = entry.key();
            if file_uri == &uri_string {
                continue;
            }

            let file_content = entry.value();
            let search_names = self.get_search_names_for_file(file_uri, &uri_string, &symbol_name);

            for search_name in search_names {
                let refs = {
                    let mut parser = self.parser_engine.lock().unwrap();
                    parser.find_references(file_content, &search_name)
                };

                for r in refs {
                    if r.is_definition {
                        continue;
                    }
                    if let Ok(loc_uri) = file_uri.parse::<lsp_types::Uri>() {
                        all_locations.push(Location {
                            uri: loc_uri,
                            range: r.range,
                        });
                    }
                }
            }
        }

        if all_locations.is_empty() {
            Ok(None)
        } else {
            Ok(Some(all_locations))
        }
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri.to_string();
        let position = params.position;

        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let symbol_name =
            match utils::get_word_at_position(&content, position.line, position.character) {
                Some(name) => name,
                None => return Ok(None),
            };

        if validation::is_keyword(&symbol_name) || validation::is_builtin_type(&symbol_name) {
            return Ok(None);
        }

        let range =
            match utils::get_word_range_at_position(&content, position.line, position.character) {
                Some(r) => r,
                None => return Ok(None),
            };

        Ok(Some(PrepareRenameResponse::Range(range)))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let uri_string = uri.to_string();
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        let content = match self.documents.get(&uri_string) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        let old_name =
            match utils::get_word_at_position(&content, position.line, position.character) {
                Some(name) => name,
                None => return Ok(None),
            };

        if !validation::is_valid_identifier(&new_name) {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "Invalid identifier: must start with letter or underscore",
            ));
        }

        if validation::is_keyword(&new_name) {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "Cannot rename to a keyword",
            ));
        }

        if validation::is_builtin_type(&new_name) {
            return Err(tower_lsp::jsonrpc::Error::invalid_params(
                "Cannot rename to a built-in type name",
            ));
        }

        let mut changes: std::collections::HashMap<lsp_types::Uri, Vec<TextEdit>> =
            std::collections::HashMap::new();

        let local_refs = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.find_references(&content, &old_name)
        };

        for r in local_refs {
            changes.entry(uri.clone()).or_default().push(TextEdit {
                range: r.range,
                new_text: new_name.clone(),
            });
        }

        for entry in self.source_cache.iter() {
            let file_uri = entry.key();
            if file_uri == &uri_string {
                continue;
            }

            let file_content = entry.value();
            let search_names = self.get_search_names_for_file(file_uri, &uri_string, &old_name);

            for search_name in search_names {
                let refs = {
                    let mut parser = self.parser_engine.lock().unwrap();
                    parser.find_references(file_content, &search_name)
                };

                let new_name_for_file = if search_name != old_name {
                    let prefix = &search_name[..search_name.len() - old_name.len()];
                    format!("{}{}", prefix, new_name)
                } else {
                    new_name.clone()
                };

                for r in refs {
                    if r.is_definition {
                        continue;
                    }
                    if let Ok(loc_uri) = file_uri.parse::<lsp_types::Uri>() {
                        changes.entry(loc_uri).or_default().push(TextEdit {
                            range: r.range,
                            new_text: new_name_for_file.clone(),
                        });
                    }
                }
            }
        }

        if changes.is_empty() {
            return Ok(None);
        }

        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sym(name: &str, kind: CompletionSymbolKind) -> CompletionSymbol {
        CompletionSymbol {
            name: name.to_string(),
            kind,
            detail: None,
            location: Some(SymbolLocation {
                start_line: 1,
                start_char: 2,
                end_line: 1,
                end_char: 2 + name.len() as u32,
            }),
            documentation: None,
        }
    }

    #[test]
    fn test_completion_kind_to_symbol_kind() {
        assert_eq!(
            completion_kind_to_symbol_kind(CompletionSymbolKind::Function),
            SymbolKind::FUNCTION
        );
        assert_eq!(
            completion_kind_to_symbol_kind(CompletionSymbolKind::Struct),
            SymbolKind::STRUCT
        );
        assert_eq!(
            completion_kind_to_symbol_kind(CompletionSymbolKind::Enum),
            SymbolKind::ENUM
        );
        assert_eq!(
            completion_kind_to_symbol_kind(CompletionSymbolKind::Variable),
            SymbolKind::VARIABLE
        );
        assert_eq!(
            completion_kind_to_symbol_kind(CompletionSymbolKind::Module),
            SymbolKind::MODULE
        );
    }

    #[test]
    fn test_symbol_location_to_range() {
        let range = symbol_location_to_range(&SymbolLocation {
            start_line: 3,
            start_char: 4,
            end_line: 3,
            end_char: 9,
        });
        assert_eq!(
            range.start,
            Position {
                line: 3,
                character: 4
            }
        );
        assert_eq!(
            range.end,
            Position {
                line: 3,
                character: 9
            }
        );
    }

    #[test]
    fn test_symbol_matches_query() {
        assert!(symbol_matches_query("transferTokens", "")); // empty query matches all
        assert!(symbol_matches_query("transferTokens", "transfer"));
        assert!(symbol_matches_query("transferTokens", "tokens")); // case-insensitive substring
        assert!(!symbol_matches_query("transferTokens", "withdraw"));
    }

    #[test]
    fn test_workspace_symbol_filter_and_map() {
        let symbols = [
            sym("addFunds", CompletionSymbolKind::Function),
            sym("Point", CompletionSymbolKind::Struct),
            sym("Color", CompletionSymbolKind::Enum),
        ];

        // Replicates the handler's per-symbol filter + map for query "add".
        let mapped: Vec<_> = symbols
            .iter()
            .filter(|s| symbol_matches_query(&s.name, "add"))
            .filter_map(|s| {
                s.location.as_ref().map(|loc| {
                    (
                        s.name.clone(),
                        completion_kind_to_symbol_kind(s.kind),
                        symbol_location_to_range(loc),
                    )
                })
            })
            .collect();

        assert_eq!(mapped.len(), 1);
        assert_eq!(mapped[0].0, "addFunds");
        assert_eq!(mapped[0].1, SymbolKind::FUNCTION);
    }
}
