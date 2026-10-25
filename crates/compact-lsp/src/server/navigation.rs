//! Navigation handlers for the server.

use super::*;

impl CompactLanguageServer {
    pub(super) async fn handle_hover(&self, params: HoverParams) -> Result<Option<Hover>> {
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
            // Contract-type circuit? (annotation / alias / inference / struct-field chain.)
            if let Some((_file, contract)) =
                self.resolve_base_contract_type(&uri, &content, &ctx.base)
                && let Some(circuit) = contract.circuits.iter().find(|c| c.name == ctx.member_name)
            {
                let signature = circuit
                    .signature
                    .strip_prefix("circuit ")
                    .unwrap_or(&circuit.signature);
                let hover_text = format!(
                    "```compact\n{}.{}\n```\n\nCircuit on contract type `{}`{}",
                    ctx.base_name,
                    signature,
                    contract.name,
                    circuit
                        .documentation
                        .as_deref()
                        .map(|d| format!("\n\n---\n\n{}", d))
                        .unwrap_or_default()
                );
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: hover_text,
                    }),
                    range: Some(ctx.member_range),
                }));
            }
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
                if let Some(method) = builtins::find_method_by_name_for(
                    base_type,
                    &ctx.member_name,
                    self.language_version,
                ) {
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
        if let Some(value) = stdlib_circuit_hover_text(&word, self.language_version) {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: None,
            }));
        }

        // Stdlib struct hover (e.g., hovering on "ShieldedCoinInfo", "Maybe")
        let base_type = builtins::extract_base_type(&word);
        if let Some(value) = stdlib_struct_hover_text(base_type, self.language_version) {
            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: None,
            }));
        }

        Ok(None)
    }

    pub(super) async fn handle_goto_definition(
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
            // Contract-type circuit? (annotation / alias / inference, same or cross file.)
            if let Some((file_uri, contract)) =
                self.resolve_base_contract_type(&uri_string, &content, &ctx.base)
                && let Some(circuit) = contract.circuits.iter().find(|c| c.name == ctx.member_name)
            {
                let target_uri = match Uri::from_str(&file_uri) {
                    Ok(u) => u,
                    Err(_) => return Ok(None),
                };
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: target_uri,
                    range: Range {
                        start: Position {
                            line: circuit.location.start_line,
                            character: circuit.location.start_char,
                        },
                        end: Position {
                            line: circuit.location.end_line,
                            character: circuit.location.end_char,
                        },
                    },
                })));
            }
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

        if let Some((file_uri, symbol)) = self.find_imported_symbol(&uri_string, &word)
            && let Some(loc) = symbol.location
        {
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
}
