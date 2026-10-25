//! Symbols handlers for the server.

use super::*;

impl CompactLanguageServer {
    pub(super) async fn handle_document_symbol(
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

    #[allow(deprecated)]
    pub(super) async fn handle_symbol(
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

    pub(super) async fn handle_folding_range(
        &self,
        params: FoldingRangeParams,
    ) -> Result<Option<Vec<FoldingRange>>> {
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

    pub(super) async fn handle_semantic_tokens_full(
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
}
