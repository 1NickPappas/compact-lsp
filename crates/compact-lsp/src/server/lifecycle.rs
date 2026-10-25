//! Lifecycle helpers for the server.

use super::*;

impl CompactLanguageServer {
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
    pub(super) fn update_symbol_cache(&self, uri: &str, content: &str) {
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
    pub(super) fn update_reverse_dependencies(&self, uri: &str, content: &str) {
        self.remove_reverse_dependencies(uri);

        let file_imports = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_imports(content)
        };

        for import in file_imports {
            if import.is_file
                && let Some(imported_uri) = imports::resolve_import_path(uri, &import.path)
            {
                self.reverse_dependencies
                    .entry(imported_uri)
                    .or_default()
                    .push(uri.to_string());
            }
        }
    }

    /// Remove a file from all reverse dependency lists.
    pub(super) fn remove_reverse_dependencies(&self, uri: &str) {
        for mut entry in self.reverse_dependencies.iter_mut() {
            entry.value_mut().retain(|dep| dep != uri);
        }
        self.reverse_dependencies.retain(|_, deps| !deps.is_empty());
    }

    /// Get all files that depend on (import) the given file.
    pub(super) fn get_dependents(&self, uri: &str) -> Vec<String> {
        self.reverse_dependencies
            .get(uri)
            .map(|deps| deps.value().clone())
            .unwrap_or_default()
    }
}

impl CompactLanguageServer {
    #[allow(deprecated)]
    pub(super) async fn handle_initialize(
        &self,
        params: InitializeParams,
    ) -> Result<InitializeResult> {
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
                document_range_formatting_provider: Some(OneOf::Left(true)),
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
                                    SemanticTokenType::KEYWORD,
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
            // Keep the client's default offset encoding (UTF-16); we do not
            // implement the clangd UTF-8 offsets extension.
            offset_encoding: None,
        })
    }

    pub(super) async fn handle_initialized(&self, _params: InitializedParams) {
        tracing::info!("Server initialized - handshake complete");

        // Surface the detected compiler/language version so registry-vs-compiler
        // drift (e.g. the LSP registry being ahead of the installed compiler) is
        // visible in the logs.
        let version_msg = match self.diagnostic_engine.version_info() {
            Some((compiler, language)) if !language.is_empty() => {
                let ledger = self.diagnostic_engine.ledger_version();
                let runtime = self.diagnostic_engine.runtime_version();
                match (ledger, runtime) {
                    (Some(ledger), Some(runtime)) => format!(
                        "Compact compiler {compiler} (language {language}, {ledger}, runtime {runtime})"
                    ),
                    _ => format!("Compact compiler {compiler} (language {language})"),
                }
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

    pub(super) async fn handle_shutdown(&self) -> Result<()> {
        tracing::info!("Shutdown requested");
        Ok(())
    }

    pub(super) async fn handle_did_open(&self, params: DidOpenTextDocumentParams) {
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

    pub(super) async fn handle_did_change(&self, params: DidChangeTextDocumentParams) {
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

    pub(super) async fn handle_did_save(&self, params: DidSaveTextDocumentParams) {
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

    pub(super) async fn handle_did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();

        if let Some((_, handle)) = self.pending_diagnostics.remove(&uri) {
            handle.abort();
        }

        self.documents.remove(&uri);

        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }
}
