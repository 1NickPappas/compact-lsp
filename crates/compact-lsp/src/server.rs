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

use std::str::FromStr;
use std::sync::Arc;
use std::sync::Mutex;

use dashmap::DashMap;
use ropey::Rope;
use lsp_types::*;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LanguageServer};

use compact_analyzer::{CompletionSymbol, DiagnosticEngine, FormatterEngine, ParserEngine};

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

    /// Workspace root URI (captured from initialize params).
    workspace_root: Arc<Mutex<Option<String>>>,

    /// Symbol cache for cross-file completion.
    /// Maps file URI -> symbols defined in that file.
    symbol_cache: Arc<DashMap<String, Vec<CompletionSymbol>>>,

    /// Source cache for cross-file hover and definition.
    /// Maps file URI -> source content.
    source_cache: Arc<DashMap<String, String>>,

    /// Pending semantic diagnostics tasks.
    /// Maps file URI -> task handle for debounced compiler diagnostics.
    /// When a new change comes in, we cancel the pending task and schedule a new one.
    pending_diagnostics: Arc<DashMap<String, tokio::task::JoinHandle<()>>>,

    /// Reverse dependency map for cross-file error propagation.
    /// Maps file URI -> list of files that import it.
    /// When a file changes, we re-diagnose all files that depend on it.
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
    ///
    /// This is called after file save to show compiler errors in the editor.
    /// Merges syntax errors (tree-sitter) with compiler errors.
    async fn publish_diagnostics(&self, uri: Uri) {
        // Get the document content
        let content = match self.documents.get(&uri.to_string()) {
            Some(doc) => doc.content.to_string(),
            None => {
                // For dependent files that aren't open, read from disk
                let path = uri.path().as_str();
                match std::fs::read_to_string(path) {
                    Ok(content) => content,
                    Err(_) => return,
                }
            }
        };

        // Get syntax errors from tree-sitter
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

        // Run the compiler and get diagnostics
        let compiler_diagnostics = self
            .diagnostic_engine
            .diagnose(&uri.to_string(), &content)
            .await;

        // Merge both: syntax errors + compiler errors
        let mut all_diagnostics = syntax_diagnostics;
        all_diagnostics.extend(compiler_diagnostics);

        // Send diagnostics to the editor
        // The third parameter is the document version (None = latest)
        self.client
            .publish_diagnostics(uri, all_diagnostics, None)
            .await;
    }

    /// Publish syntax diagnostics for a document (instant, on every keystroke).
    ///
    /// Uses tree-sitter to detect syntax errors immediately, without needing
    /// to save the file or run the compiler.
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

        tracing::trace!(
            "Publishing {} syntax diagnostics for {}",
            diagnostics.len(),
            uri.to_string()
        );

        self.client.publish_diagnostics(uri, diagnostics, None).await;
    }

    /// Schedule semantic diagnostics with debounce.
    ///
    /// This schedules a compiler run after a 500ms delay. If another change
    /// comes in before the delay expires, the pending task is cancelled and
    /// a new one is scheduled. This prevents excessive compiler runs while typing.
    ///
    /// The published diagnostics MERGE syntax errors (from tree-sitter) with
    /// compiler errors, so both are visible.
    async fn schedule_semantic_diagnostics(&self, uri: Uri, content: String) {
        let uri_string = uri.to_string();

        // Cancel any pending task for this file
        if let Some((_, handle)) = self.pending_diagnostics.remove(&uri_string) {
            handle.abort();
            tracing::trace!("Cancelled pending diagnostics for {}", uri_string);
        }

        // Clone what we need for the spawned task
        let client = self.client.clone();
        let diagnostic_engine = self.diagnostic_engine.clone();
        let parser_engine = self.parser_engine.clone();
        let pending = self.pending_diagnostics.clone();
        let uri_clone = uri_string.clone();

        // Schedule new task with 500ms delay
        let handle = tokio::spawn(async move {
            // Wait before running compiler
            tokio::time::sleep(std::time::Duration::from_millis(500)).await;

            // Get syntax errors from tree-sitter (re-parse to get latest state)
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

            // Get compiler diagnostics
            let compiler_diagnostics = diagnostic_engine.diagnose_content(&uri_clone, &content).await;

            // Merge both: syntax errors + compiler errors
            let mut all_diagnostics = syntax_diagnostics;
            all_diagnostics.extend(compiler_diagnostics);

            // Publish merged results
            client.publish_diagnostics(uri, all_diagnostics, None).await;

            // Remove ourselves from pending map
            pending.remove(&uri_clone);
        });

        self.pending_diagnostics.insert(uri_string, handle);
    }

    /// Scan workspace for all .compact files and cache their symbols.
    ///
    /// This is called during initialization to populate the symbol cache
    /// for cross-file completion support.
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

        // Convert file:// URI to path
        let root_path = match root.strip_prefix("file://") {
            Some(path) => path,
            None => {
                tracing::warn!("Workspace root is not a file URI: {}", root);
                return;
            }
        };

        tracing::info!("Scanning workspace for .compact files: {}", root_path);

        // Walk directory recursively to find .compact files
        let mut files_found = 0;
        let mut symbols_found = 0;

        if let Ok(entries) = Self::find_compact_files(root_path) {
            for file_path in entries {
                // Read file content
                let content = match std::fs::read_to_string(&file_path) {
                    Ok(content) => content,
                    Err(e) => {
                        tracing::warn!("Failed to read {}: {}", file_path, e);
                        continue;
                    }
                };

                // Parse and extract symbols
                let symbols = {
                    let mut parser = self.parser_engine.lock().unwrap();
                    parser.get_completion_symbols(&content)
                };

                // Store in cache with file:// URI using canonical path
                let canonical_path = std::path::Path::new(&file_path)
                    .canonicalize()
                    .ok()
                    .and_then(|p| p.to_str().map(|s| s.to_string()))
                    .unwrap_or(file_path.clone());
                let uri = format!("file://{}", canonical_path);
                tracing::debug!("Caching file: {}", uri);

                // Always cache source content for hover/goto_definition
                self.source_cache.insert(uri.clone(), content.clone());

                let symbol_count = symbols.len();
                if symbol_count > 0 {
                    self.symbol_cache.insert(uri.clone(), symbols);
                    symbols_found += symbol_count;
                }

                // Build reverse dependency map: parse imports and record dependencies
                self.update_reverse_dependencies(&uri, &content);

                files_found += 1;
            }
        }

        tracing::info!(
            "Workspace scan complete: {} files, {} symbols cached",
            files_found,
            symbols_found
        );

        // Log reverse dependency stats
        let dep_count: usize = self.reverse_dependencies.iter().map(|e| e.value().len()).sum();
        tracing::info!("Reverse dependencies: {} entries", dep_count);
    }

    /// Recursively find all .compact files in a directory.
    fn find_compact_files(root: &str) -> std::io::Result<Vec<String>> {
        let mut files = Vec::new();
        Self::find_compact_files_recursive(root, &mut files)?;
        Ok(files)
    }

    fn find_compact_files_recursive(dir: &str, files: &mut Vec<String>) -> std::io::Result<()> {
        let entries = std::fs::read_dir(dir)?;

        for entry in entries {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                // Skip hidden directories and common non-source directories
                let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
                if !name.starts_with('.') && name != "node_modules" && name != "target" {
                    Self::find_compact_files_recursive(path.to_str().unwrap_or(""), files)?;
                }
            } else if path.extension().map(|e| e == "compact").unwrap_or(false) {
                if let Some(path_str) = path.to_str() {
                    files.push(path_str.to_string());
                }
            }
        }

        Ok(())
    }

    /// Update the symbol and source cache for a specific file.
    fn update_symbol_cache(&self, uri: &str, content: &str) {
        let symbols = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_completion_symbols(content)
        };

        // Always update source cache for hover/goto_definition
        self.source_cache.insert(uri.to_string(), content.to_string());

        if symbols.is_empty() {
            self.symbol_cache.remove(uri);
        } else {
            self.symbol_cache.insert(uri.to_string(), symbols);
        }
    }

    /// Update reverse dependencies for a file based on its imports.
    ///
    /// This parses the file's imports and records this file as a dependent
    /// of each imported file. Used for cross-file error propagation.
    fn update_reverse_dependencies(&self, uri: &str, content: &str) {
        // First, remove old dependencies for this file
        self.remove_reverse_dependencies(uri);

        // Parse imports from the content
        let imports = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_imports(content)
        };

        // For each import, add this file as a dependent
        for import in imports {
            // Only track file imports (not standard library imports)
            if import.is_file {
                if let Some(imported_uri) = self.resolve_import_path(uri, &import.path) {
                    self.reverse_dependencies
                        .entry(imported_uri)
                        .or_insert_with(Vec::new)
                        .push(uri.to_string());
                }
            }
        }
    }

    /// Remove a file from all reverse dependency lists.
    ///
    /// Called before updating dependencies when a file's imports change.
    fn remove_reverse_dependencies(&self, uri: &str) {
        // Iterate over all entries and remove this file from their dependent lists
        for mut entry in self.reverse_dependencies.iter_mut() {
            entry.value_mut().retain(|dep| dep != uri);
        }
        // Clean up empty entries
        self.reverse_dependencies.retain(|_, deps| !deps.is_empty());
    }

    /// Get all files that depend on (import) the given file.
    fn get_dependents(&self, uri: &str) -> Vec<String> {
        self.reverse_dependencies
            .get(uri)
            .map(|deps| deps.value().clone())
            .unwrap_or_default()
    }

    /// Resolve an import path relative to the current file.
    ///
    /// Converts relative import paths like "../utils/Utils" to absolute file URIs.
    fn resolve_import_path(&self, current_uri: &str, import_path: &str) -> Option<String> {
        // Get the directory of the current file
        let current_path = current_uri.strip_prefix("file://")?;
        let current_dir = std::path::Path::new(current_path).parent()?;

        // Resolve the relative import path
        let import_with_ext = if import_path.ends_with(".compact") {
            import_path.to_string()
        } else {
            format!("{}.compact", import_path)
        };

        let resolved = current_dir.join(&import_with_ext);
        let normalized = self.normalize_path(&resolved)?;

        // Return as file:// URI
        Some(format!("file://{}", normalized))
    }

    /// Normalize a path by resolving .. and . components.
    fn normalize_path(&self, path: &std::path::Path) -> Option<String> {
        // Use canonicalize if the file exists, otherwise do manual normalization
        if path.exists() {
            path.canonicalize().ok()?.to_str().map(|s| s.to_string())
        } else {
            // Manual normalization for non-existent paths
            let mut components = Vec::new();
            for component in path.components() {
                match component {
                    std::path::Component::ParentDir => {
                        components.pop();
                    }
                    std::path::Component::CurDir => {}
                    _ => {
                        components.push(component);
                    }
                }
            }
            let normalized: std::path::PathBuf = components.iter().collect();
            normalized.to_str().map(|s| s.to_string())
        }
    }

    /// Get the word (identifier) at a given position in the source.
    fn get_word_at_position(&self, content: &str, line: u32, character: u32) -> Option<String> {
        let lines: Vec<&str> = content.lines().collect();
        let line_content = lines.get(line as usize)?;
        let char_idx = character as usize;

        // Check if position is within bounds
        if char_idx > line_content.len() {
            return None;
        }

        // Find word boundaries (identifiers can contain a-z, A-Z, 0-9, _)
        let chars: Vec<char> = line_content.chars().collect();

        // Find start of word
        let mut start = char_idx;
        while start > 0 {
            let c = chars.get(start - 1)?;
            if c.is_alphanumeric() || *c == '_' {
                start -= 1;
            } else {
                break;
            }
        }

        // Find end of word
        let mut end = char_idx;
        while end < chars.len() {
            let c = chars.get(end)?;
            if c.is_alphanumeric() || *c == '_' {
                end += 1;
            } else {
                break;
            }
        }

        if start == end {
            return None;
        }

        Some(chars[start..end].iter().collect())
    }

    /// Find an imported symbol by name (with prefix handling).
    ///
    /// Given a name like "Utils_add", this checks if it matches any imported symbol
    /// and returns the file URI and symbol if found.
    fn find_imported_symbol(&self, current_uri: &str, name: &str) -> Option<(String, CompletionSymbol)> {
        // Get imports from current file
        let imports = {
            let content = self.documents.get(current_uri)?.content.to_string();
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_imports(&content)
        };

        // Check each import to see if name matches prefix + symbol
        for import in imports {
            // Skip non-file imports
            if !import.is_file {
                continue;
            }

            // Must have a prefix to match
            let prefix = match &import.prefix {
                Some(p) => p,
                None => continue,
            };

            // Check if name starts with the prefix
            if !name.starts_with(prefix) {
                continue;
            }

            // Strip prefix to get the actual symbol name
            let symbol_name = &name[prefix.len()..];
            if symbol_name.is_empty() {
                continue;
            }

            // Resolve import path to file URI
            let resolved_uri = match self.resolve_import_path(current_uri, &import.path) {
                Some(uri) => uri,
                None => continue,
            };

            // Look up symbol in cache
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
    fn build_signature_help_response(&self, info: compact_analyzer::SignatureInfo) -> SignatureHelp {
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

    /// Get the function name from a call context (e.g., "func_name(" before cursor).
    fn get_function_call_name(&self, content: &str, line: u32, character: u32) -> Option<String> {
        let lines: Vec<&str> = content.lines().collect();
        let line_content = lines.get(line as usize)?;

        // Search backwards from cursor to find the opening paren
        let chars: Vec<char> = line_content.chars().collect();
        let mut idx = character as usize;

        // First, find the opening paren of our call (handling nested parens)
        let mut paren_depth = 0;
        while idx > 0 {
            idx -= 1;
            match chars.get(idx) {
                Some(')') => paren_depth += 1,
                Some('(') => {
                    if paren_depth == 0 {
                        // Found our opening paren - now get the identifier before it
                        break;
                    }
                    paren_depth -= 1;
                }
                _ => {}
            }
        }

        // Now idx points to '(' - search backwards for the function name
        if idx == 0 {
            return None;
        }

        // Skip any whitespace
        while idx > 0 && chars.get(idx - 1).map(|c| c.is_whitespace()).unwrap_or(false) {
            idx -= 1;
        }

        // Now find the identifier
        let end = idx;
        while idx > 0 && chars.get(idx - 1).map(|c| c.is_alphanumeric() || *c == '_').unwrap_or(false) {
            idx -= 1;
        }

        if idx == end {
            return None;
        }

        Some(chars[idx..end].iter().collect())
    }

    /// Count commas before cursor position in a function call.
    fn count_commas_before_cursor(&self, content: &str, line: u32, character: u32) -> u32 {
        let lines: Vec<&str> = content.lines().collect();
        let line_content = match lines.get(line as usize) {
            Some(l) => *l,
            None => return 0,
        };

        let chars: Vec<char> = line_content.chars().collect();
        let cursor_idx = character as usize;
        let mut comma_count = 0;
        let mut paren_depth = 0;

        // Search backwards from cursor to find commas (not inside nested parens)
        let mut idx = cursor_idx;
        while idx > 0 {
            idx -= 1;
            match chars.get(idx) {
                Some(')') => paren_depth += 1,
                Some('(') => {
                    if paren_depth == 0 {
                        // Found our function call's opening paren
                        break;
                    }
                    paren_depth -= 1;
                }
                Some(',') if paren_depth == 0 => comma_count += 1,
                _ => {}
            }
        }

        comma_count
    }

    /// Parse parameter strings from a detail string like "(a: Field, b: Field): ReturnType".
    fn parse_params_from_detail(&self, detail: &str) -> Vec<String> {
        // Extract content between first '(' and matching ')'
        let start = match detail.find('(') {
            Some(i) => i + 1,
            None => return vec![],
        };

        let end = match detail.find(')') {
            Some(i) => i,
            None => return vec![],
        };

        if start >= end {
            return vec![];
        }

        let params_str = &detail[start..end];
        if params_str.trim().is_empty() {
            return vec![];
        }

        // Split by comma, handling potential nested types
        params_str
            .split(',')
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect()
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
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        tracing::info!("Received initialize request");

        // Capture workspace root for cross-file features
        // Prefer workspace_folders (LSP 3.6+), fallback to root_uri
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
                // Hover provider for documentation and type info
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                // Go to definition provider
                definition_provider: Some(OneOf::Left(true)),
                // Signature help provider
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),
                // Semantic tokens provider for rich syntax highlighting
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                // Token types - order must match SemanticTokenType enum in parser.rs
                                token_types: vec![
                                    SemanticTokenType::FUNCTION,      // 0
                                    SemanticTokenType::TYPE,          // 1
                                    SemanticTokenType::STRUCT,        // 2
                                    SemanticTokenType::ENUM,          // 3
                                    SemanticTokenType::ENUM_MEMBER,   // 4
                                    SemanticTokenType::PARAMETER,     // 5
                                    SemanticTokenType::PROPERTY,      // 6
                                    SemanticTokenType::VARIABLE,      // 7
                                    SemanticTokenType::NAMESPACE,     // 8
                                    SemanticTokenType::TYPE_PARAMETER, // 9
                                ],
                                // Token modifiers - order must match SemanticTokenModifier enum
                                token_modifiers: vec![
                                    SemanticTokenModifier::DECLARATION,      // bit 0
                                    SemanticTokenModifier::READONLY,         // bit 1
                                    SemanticTokenModifier::DEFAULT_LIBRARY,  // bit 2
                                ],
                            },
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: Some(false), // Not implementing range-based tokens yet
                            ..Default::default()
                        },
                    ),
                ),
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

        // Scan workspace for .compact files and cache symbols
        self.scan_workspace().await;

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
        let uri = params.text_document.uri.clone();
        let uri_string = uri.to_string();
        tracing::trace!("Document changed: {}", uri_string);

        // Apply changes to our stored document
        if let Some(mut doc) = self.documents.get_mut(&uri_string) {
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

        // Publish syntax diagnostics immediately (live, on every keystroke)
        self.publish_syntax_diagnostics(uri.clone()).await;

        // Schedule semantic diagnostics (debounced compiler run)
        let content = match self.documents.get(&uri_string) {
            Some(doc) => doc.content.to_string(),
            None => return,
        };
        self.schedule_semantic_diagnostics(uri, content).await;
    }

    /// Handle `textDocument/didSave` notification.
    ///
    /// The user saved the file. This is when we run the compiler
    /// and publish any diagnostics (errors/warnings).
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let uri_str = uri.to_string();
        tracing::debug!("Document saved: {:?}", uri);

        // Update symbol cache and reverse dependencies for this file
        if let Some(doc) = self.documents.get(&uri_str) {
            let content = doc.content.to_string();
            self.update_symbol_cache(&uri_str, &content);
            self.update_reverse_dependencies(&uri_str, &content);
            tracing::debug!("Symbol cache and dependencies updated for: {}", uri_str);
        }

        // Run diagnostics for this file
        self.publish_diagnostics(uri).await;

        // Re-diagnose all files that import this file (cross-file error propagation)
        let dependents = self.get_dependents(&uri_str);
        if !dependents.is_empty() {
            tracing::debug!(
                "Re-diagnosing {} dependent files for: {}",
                dependents.len(),
                uri_str
            );
            for dependent_uri in dependents {
                if let Ok(dep_uri) = dependent_uri.parse::<Uri>() {
                    self.publish_diagnostics(dep_uri).await;
                }
            }
        }
    }

    /// Handle `textDocument/didClose` notification.
    ///
    /// The editor closed the file. We should:
    /// - Remove it from our document store
    /// - Cancel any pending diagnostics
    /// - Clear any diagnostics for this file
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        tracing::debug!("Document closed: {}", uri);

        // Cancel any pending diagnostics for this file
        if let Some((_, handle)) = self.pending_diagnostics.remove(&uri) {
            handle.abort();
        }

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
    /// - Symbols defined in the current file (circuits, structs, etc.)
    /// - Keywords (circuit, ledger, struct, etc.)
    /// - Built-in types (Boolean, Field, Uint, etc.)
    /// - Snippets (circuit template, struct template, etc.)
    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let mut items = Vec::new();
        let uri = params.text_document_position.text_document.uri.to_string();

        // Helper to convert symbol kind to LSP kind
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

        // Get symbols from the current document (highest priority - parsed fresh)
        // Also parse imports to know what's available with prefixes
        let imports = if let Some(doc) = self.documents.get(&uri) {
            let content = doc.content.to_string();

            // Get local symbols (no prefix)
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

            // Parse imports for cross-file completion
            let imports = {
                let mut parser = self.parser_engine.lock().unwrap();
                parser.get_imports(&content)
            };
            imports
        } else {
            vec![]
        };

        // Debug: log imports found
        tracing::debug!("Found {} imports in current file", imports.len());
        for import in &imports {
            tracing::debug!(
                "  Import: path={:?}, is_file={}, prefix={:?}",
                import.path,
                import.is_file,
                import.prefix
            );
        }

        // Debug: log cache keys
        tracing::debug!("Symbol cache has {} entries:", self.symbol_cache.len());
        for entry in self.symbol_cache.iter() {
            tracing::debug!("  Cache key: {}", entry.key());
        }

        // Get symbols from imported files (with proper prefix)
        for import in &imports {
            // Skip non-file imports (e.g., CompactStandardLibrary)
            if !import.is_file {
                tracing::debug!("Skipping non-file import: {}", import.path);
                continue;
            }

            // Resolve the import path to a file URI
            let resolved_uri = match self.resolve_import_path(&uri, &import.path) {
                Some(uri) => uri,
                None => {
                    tracing::warn!("Could not resolve import: {} (from {})", import.path, uri);
                    continue;
                }
            };

            tracing::debug!("Resolved import {} -> {}", import.path, resolved_uri);

            // Get symbols from the imported file's cache
            if let Some(entry) = self.symbol_cache.get(&resolved_uri) {
                tracing::debug!("Found {} symbols in cache for {}", entry.value().len(), resolved_uri);
                let prefix = import.prefix.as_deref().unwrap_or("");

                for sym in entry.value().iter() {
                    // Apply prefix to symbol name
                    let prefixed_name = if prefix.is_empty() {
                        sym.name.clone()
                    } else {
                        format!("{}{}", prefix, sym.name)
                    };

                    // Extract source filename for detail
                    let source_file = import
                        .path
                        .rsplit('/')
                        .next()
                        .unwrap_or(&import.path);

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
            } else {
                tracing::warn!("Cache miss for resolved URI: {}", resolved_uri);
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

    /// Handle `textDocument/hover` request.
    ///
    /// Returns hover information for the element at the cursor position.
    /// This includes:
    /// - Documentation for keywords and built-in types
    /// - Signatures for circuits, witnesses, structs, etc.
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri.to_string();
        let position = params.text_document_position_params.position;
        tracing::debug!("Hover requested for: {} at {:?}", uri, position);

        // Get the document content
        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => {
                tracing::warn!("Document not found: {}", uri);
                return Ok(None);
            }
        };

        // First, try local hover info from parser
        let hover_info = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.hover_info(&content, position.line, position.character)
        };

        if let Some(info) = hover_info {
            tracing::debug!("Local hover info found");
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: info.content,
                }),
                range: info.range,
            }));
        }

        // No local hover - check if it's an imported symbol
        let word = match self.get_word_at_position(&content, position.line, position.character) {
            Some(w) => w,
            None => {
                tracing::debug!("No word at position");
                return Ok(None);
            }
        };

        tracing::debug!("Checking imported symbols for: {}", word);

        // Try to find the symbol in imported files
        if let Some((_file_uri, symbol)) = self.find_imported_symbol(&uri, &word) {
            tracing::debug!("Found imported symbol: {} from {:?}", symbol.name, _file_uri);
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

        tracing::debug!("No hover info at position");
        Ok(None)
    }

    /// Handle `textDocument/definition` request.
    ///
    /// Returns the location of the definition for the symbol at the cursor.
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri.clone();
        let uri_string = uri.to_string();
        let position = params.text_document_position_params.position;
        tracing::debug!("Go to definition requested for: {:?} at {:?}", uri, position);

        // Get the document content
        let content = match self.documents.get(&uri_string) {
            Some(doc) => doc.content.to_string(),
            None => {
                tracing::warn!("Document not found: {:?}", uri);
                return Ok(None);
            }
        };

        // First, try local definition from parser
        let def_location = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.goto_definition(&content, position.line, position.character)
        };

        if let Some(loc) = def_location {
            tracing::debug!("Local definition found at {:?}", loc.selection_range);
            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri,
                range: loc.selection_range,
            })));
        }

        // No local definition - check if it's an imported symbol
        let word = match self.get_word_at_position(&content, position.line, position.character) {
            Some(w) => w,
            None => {
                tracing::debug!("No word at position");
                return Ok(None);
            }
        };

        tracing::debug!("Checking imported symbols for: {}", word);

        // Try to find the symbol in imported files
        if let Some((file_uri, symbol)) = self.find_imported_symbol(&uri_string, &word) {
            if let Some(loc) = symbol.location {
                tracing::debug!("Found imported symbol: {} in {}", symbol.name, file_uri);
                let target_uri = match Uri::from_str(&file_uri) {
                    Ok(u) => u,
                    Err(_) => {
                        tracing::warn!("Invalid URI: {}", file_uri);
                        return Ok(None);
                    }
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

        tracing::debug!("No definition found");
        Ok(None)
    }

    /// Handle `textDocument/signatureHelp` request.
    ///
    /// Shows function signature while typing arguments.
    async fn signature_help(
        &self,
        params: SignatureHelpParams,
    ) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri.to_string();
        let position = params.text_document_position_params.position;
        tracing::debug!("Signature help requested for: {} at {:?}", uri, position);

        // Get the document content
        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => {
                tracing::warn!("Document not found: {}", uri);
                return Ok(None);
            }
        };

        // First, try local signature help from parser
        let sig_info = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.signature_help(&content, position.line, position.character)
        };

        if let Some(info) = sig_info {
            tracing::debug!("Local signature help found: {}", info.label);
            return Ok(Some(self.build_signature_help_response(info)));
        }

        // No local signature help - check if we're inside an imported function call
        // Try to find the function name from the call context
        let func_name = match self.get_function_call_name(&content, position.line, position.character) {
            Some(name) => name,
            None => {
                tracing::debug!("No function call context at position");
                return Ok(None);
            }
        };

        tracing::debug!("Checking imported symbols for function: {}", func_name);

        // Try to find the symbol in imported files
        if let Some((_file_uri, symbol)) = self.find_imported_symbol(&uri, &func_name) {
            tracing::debug!("Found imported symbol: {} from {:?}", symbol.name, _file_uri);

            // Build signature from the cached symbol's detail
            if let Some(detail) = &symbol.detail {
                // Count commas before cursor to determine active parameter
                let active_param = self.count_commas_before_cursor(&content, position.line, position.character);

                // Parse parameters from detail string like "(a: Field, b: Field): ReturnType"
                let params = self.parse_params_from_detail(detail);

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

        tracing::debug!("No signature help at position");
        Ok(None)
    }

    /// Handle `textDocument/semanticTokens/full` request.
    ///
    /// Returns semantic tokens for the entire document.
    /// These enable rich, semantic-aware syntax highlighting.
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        tracing::debug!("Semantic tokens requested for: {}", uri);

        // Get the document content
        let content = match self.documents.get(&uri) {
            Some(doc) => doc.content.to_string(),
            None => {
                tracing::warn!("Document not found: {}", uri);
                return Ok(None);
            }
        };

        // Get semantic tokens from parser
        let tokens = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_semantic_tokens(&content)
        };

        tracing::debug!("Found {} semantic tokens", tokens.len());

        // Encode tokens to LSP format (relative positioning)
        // Each SemanticToken contains: delta_line, delta_start, length, token_type, token_modifiers_bitset
        let mut data = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_char = 0u32;

        for token in tokens {
            let line = token.range.start.line;
            let char = token.range.start.character;
            let length = token.range.end.character.saturating_sub(token.range.start.character);

            // Calculate deltas (LSP uses relative positioning)
            let delta_line = line - prev_line;
            let delta_start = if delta_line == 0 {
                char - prev_char
            } else {
                char // Reset to absolute on new line
            };

            // Encode modifiers as bitmask
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
            result_id: None, // No incremental updates yet
            data,
        })))
    }
}
