//! References helpers for the server.

use super::*;

/// A resolved struct-field or enum-variant symbol target (owner type + member).
pub(super) struct MemberTarget {
    owner_uri: String,
    owner_name: String,
    member_name: String,
    kind: TypeMemberKind,
}

impl CompactLanguageServer {
    /// If `position` targets a contract-type circuit (its declaration or a
    /// `base.circuit(...)` use), return the defining file, contract, and name.
    pub(super) fn contract_circuit_target(
        &self,
        uri: &str,
        content: &str,
        line: u32,
        character: u32,
    ) -> Option<(String, ContractTypeInfo, String)> {
        // (a) Cursor on an `ecdecl_circuit` declaration.
        let at_decl = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.circuit_at_position(content, line, character)
        };
        if let Some((contract_name, circuit_name)) = at_decl
            && let Some((file_uri, contract)) = self.resolve_contract_type(uri, &contract_name)
        {
            return Some((file_uri, contract, circuit_name));
        }

        // (b) Cursor on a `base.circuit` member whose base is a contract value.
        let member = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_member_access_context(content, line, character)
        }?;
        let (file_uri, contract) = self.resolve_base_contract_type(uri, content, &member.base)?;
        if contract
            .circuits
            .iter()
            .any(|c| c.name == member.member_name)
        {
            return Some((file_uri, contract, member.member_name));
        }
        None
    }

    /// Collect references (and optionally the declaration) of a contract circuit
    /// across all cached files, gated by each base's resolved contract type.
    pub(super) fn collect_contract_circuit_refs(
        &self,
        contract_uri: &str,
        contract: &ContractTypeInfo,
        circuit_name: &str,
        include_definition: bool,
    ) -> Vec<Location> {
        let mut locations = Vec::new();

        if include_definition
            && let Some(circuit) = contract.circuits.iter().find(|c| c.name == circuit_name)
            && let Ok(def_uri) = contract_uri.parse::<Uri>()
        {
            locations.push(Location {
                uri: def_uri,
                range: Range {
                    start: Position {
                        line: circuit.name_location.start_line,
                        character: circuit.name_location.start_char,
                    },
                    end: Position {
                        line: circuit.name_location.end_line,
                        character: circuit.name_location.end_char,
                    },
                },
            });
        }

        for entry in self.source_cache.iter() {
            let file_uri = entry.key();
            let file_content = entry.value();
            if !file_content.contains(circuit_name) {
                continue;
            }
            let uses = {
                let mut parser = self.parser_engine.lock().unwrap();
                parser.collect_member_accesses(file_content, circuit_name)
            };
            for use_ctx in uses {
                let Some((resolved_uri, resolved_contract)) =
                    self.resolve_base_contract_type(file_uri, file_content, &use_ctx.base)
                else {
                    continue;
                };
                if same_file(&resolved_uri, contract_uri)
                    && resolved_contract.name == contract.name
                    && let Ok(loc_uri) = file_uri.parse::<Uri>()
                {
                    locations.push(Location {
                        uri: loc_uri,
                        range: use_ctx.member_range,
                    });
                }
            }
        }

        // Prefer open-buffer URIs and drop duplicates (the same file can appear
        // under canonical and non-canonical URIs, e.g. macOS `/var`).
        let mut seen = std::collections::HashSet::new();
        let mut unique = Vec::new();
        for mut loc in locations {
            let uri_str = self.preferred_uri(&loc.uri.to_string());
            if !seen.insert((
                canonical_key(&uri_str),
                loc.range.start.line,
                loc.range.start.character,
                loc.range.end.line,
                loc.range.end.character,
            )) {
                continue;
            }
            if let Ok(u) = uri_str.parse::<Uri>() {
                loc.uri = u;
                unique.push(loc);
            }
        }
        unique
    }

    /// If `position` targets a struct-field or enum-variant symbol (its
    /// declaration or a `base.member` use gated by the owner type), return the
    /// resolved target.
    pub(super) fn type_member_target(
        &self,
        uri: &str,
        content: &str,
        line: u32,
        character: u32,
    ) -> Option<MemberTarget> {
        // (a) Cursor on a field/variant declaration.
        let at_decl = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.type_member_at_position(content, line, character)
        };
        if let Some((owner_name, member_name, kind)) = at_decl {
            let home = match kind {
                TypeMemberKind::Field => self.find_struct_home(uri, content, &owner_name),
                TypeMemberKind::Variant => self.find_enum_home(uri, content, &owner_name),
            };
            if let Some((owner_uri, _owner_content, resolved_name)) = home {
                return Some(MemberTarget {
                    owner_uri,
                    owner_name: resolved_name,
                    member_name,
                    kind,
                });
            }
        }

        // (b) Cursor on a `base.member` usage.
        let member = {
            let mut parser = self.parser_engine.lock().unwrap();
            parser.get_member_access_context(content, line, character)
        }?;
        // Struct field: resolve the base's struct type.
        if let Some((raw, scope_uri)) = self.resolve_base_raw_type(uri, content, &member.base) {
            let scope_content = self
                .source_for(&scope_uri)
                .unwrap_or_else(|| content.to_string());
            if let Some((owner_uri, owner_content, owner_name)) =
                self.find_struct_home(&scope_uri, &scope_content, &raw)
                && {
                    let mut parser = self.parser_engine.lock().unwrap();
                    parser
                        .get_struct_field_type(&owner_content, &owner_name, &member.member_name)
                        .is_some()
                }
            {
                return Some(MemberTarget {
                    owner_uri,
                    owner_name,
                    member_name: member.member_name,
                    kind: TypeMemberKind::Field,
                });
            }
        }
        // Enum variant: the base is the enum type name itself.
        if let MemberBase::Path(chain) = &member.base
            && chain.len() == 1
            && let Some((owner_uri, owner_content, owner_name)) =
                self.find_enum_home(uri, content, &chain[0])
            && {
                let mut parser = self.parser_engine.lock().unwrap();
                parser
                    .find_member_definition_location(
                        &owner_content,
                        &owner_name,
                        &member.member_name,
                        TypeMemberKind::Variant,
                    )
                    .is_some()
            }
        {
            return Some(MemberTarget {
                owner_uri,
                owner_name,
                member_name: member.member_name,
                kind: TypeMemberKind::Variant,
            });
        }
        None
    }

    /// Collect references (and optionally the declaration) of a struct field or
    /// enum variant across all cached files, gated by each base's owner type.
    pub(super) fn collect_type_member_refs(
        &self,
        target: &MemberTarget,
        include_declaration: bool,
    ) -> Vec<Location> {
        let mut locations = Vec::new();

        if include_declaration
            && let Some(owner_content) = self.source_for(&target.owner_uri)
            && let Some(range) = {
                let mut parser = self.parser_engine.lock().unwrap();
                parser.find_member_definition_location(
                    &owner_content,
                    &target.owner_name,
                    &target.member_name,
                    target.kind,
                )
            }
            && let Ok(def_uri) = target.owner_uri.parse::<Uri>()
        {
            locations.push(Location {
                uri: def_uri,
                range,
            });
        }

        for entry in self.source_cache.iter() {
            let file_uri = entry.key();
            let file_content = entry.value();
            if !file_content.contains(&target.member_name) {
                continue;
            }
            let uses = {
                let mut parser = self.parser_engine.lock().unwrap();
                parser.collect_member_accesses(file_content, &target.member_name)
            };
            for use_ctx in uses {
                let matches = match target.kind {
                    TypeMemberKind::Field => {
                        let Some((raw, scope_uri)) =
                            self.resolve_base_raw_type(file_uri, file_content, &use_ctx.base)
                        else {
                            continue;
                        };
                        let scope_content = self
                            .source_for(&scope_uri)
                            .unwrap_or_else(|| file_content.to_string());
                        match self.find_struct_home(&scope_uri, &scope_content, &raw) {
                            Some((owner_uri, _c, owner_name)) => {
                                same_file(&owner_uri, &target.owner_uri)
                                    && owner_name == target.owner_name
                            }
                            None => false,
                        }
                    }
                    TypeMemberKind::Variant => {
                        if let MemberBase::Path(chain) = &use_ctx.base
                            && chain.len() == 1
                            && let Some((owner_uri, _c, owner_name)) =
                                self.find_enum_home(file_uri, file_content, &chain[0])
                        {
                            same_file(&owner_uri, &target.owner_uri)
                                && owner_name == target.owner_name
                        } else {
                            false
                        }
                    }
                };
                if matches && let Ok(loc_uri) = file_uri.parse::<Uri>() {
                    locations.push(Location {
                        uri: loc_uri,
                        range: use_ctx.member_range,
                    });
                }
            }
        }

        // Prefer open-buffer URIs and drop duplicates (the same file can appear
        // under canonical and non-canonical URIs, e.g. macOS `/var`).
        let mut seen = std::collections::HashSet::new();
        let mut unique = Vec::new();
        for mut loc in locations {
            let uri_str = self.preferred_uri(&loc.uri.to_string());
            if !seen.insert((
                canonical_key(&uri_str),
                loc.range.start.line,
                loc.range.start.character,
                loc.range.end.line,
                loc.range.end.character,
            )) {
                continue;
            }
            if let Ok(u) = uri_str.parse::<Uri>() {
                loc.uri = u;
                unique.push(loc);
            }
        }
        unique
    }
}

impl CompactLanguageServer {
    pub(super) async fn handle_references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let uri_string = uri.to_string();
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        let content = match self.documents.get(&uri_string) {
            Some(doc) => doc.content.to_string(),
            None => return Ok(None),
        };

        // Contract-type circuit references: declaration or `base.circuit(...)`.
        if let Some((contract_uri, contract, circuit_name)) =
            self.contract_circuit_target(&uri_string, &content, position.line, position.character)
        {
            let locations = self.collect_contract_circuit_refs(
                &contract_uri,
                &contract,
                &circuit_name,
                include_declaration,
            );
            return Ok(if locations.is_empty() {
                None
            } else {
                Some(locations)
            });
        }

        // Struct-field / enum-variant references, gated by the owner type.
        if let Some(target) =
            self.type_member_target(&uri_string, &content, position.line, position.character)
        {
            let locations = self.collect_type_member_refs(&target, include_declaration);
            return Ok(if locations.is_empty() {
                None
            } else {
                Some(locations)
            });
        }

        let symbol_name =
            match utils::get_word_at_position(&content, position.line, position.character) {
                Some(name) => name,
                None => return Ok(None),
            };

        let mut all_locations = Vec::new();
        // Dedupe by physical file: the open document is keyed by the client's
        // URI, while the workspace scan caches canonical paths, so the same
        // file can appear under two spellings (e.g. /var vs /private/var).
        let mut seen_files: std::collections::HashSet<String> = std::collections::HashSet::new();
        seen_files.insert(canonical_key(&uri_string));

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
            if !seen_files.insert(canonical_key(file_uri)) {
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
    pub(super) async fn handle_prepare_rename(
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
    pub(super) async fn handle_rename(
        &self,
        params: RenameParams,
    ) -> Result<Option<WorkspaceEdit>> {
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

        // Contract-type circuit: rename its declaration and `base.circuit(...)` uses.
        if let Some((contract_uri, contract, circuit_name)) =
            self.contract_circuit_target(&uri_string, &content, position.line, position.character)
        {
            let locations =
                self.collect_contract_circuit_refs(&contract_uri, &contract, &circuit_name, true);
            if locations.is_empty() {
                return Ok(None);
            }
            let mut edits: std::collections::HashMap<lsp_types::Uri, Vec<TextEdit>> =
                std::collections::HashMap::new();
            for loc in locations {
                edits.entry(loc.uri).or_default().push(TextEdit {
                    range: loc.range,
                    new_text: new_name.clone(),
                });
            }
            return Ok(Some(WorkspaceEdit {
                changes: Some(edits),
                document_changes: None,
                change_annotations: None,
            }));
        }

        // Struct-field / enum-variant rename, gated by the owner type.
        if let Some(target) =
            self.type_member_target(&uri_string, &content, position.line, position.character)
        {
            let locations = self.collect_type_member_refs(&target, true);
            if locations.is_empty() {
                return Ok(None);
            }
            let mut edits: std::collections::HashMap<lsp_types::Uri, Vec<TextEdit>> =
                std::collections::HashMap::new();
            for loc in locations {
                edits.entry(loc.uri).or_default().push(TextEdit {
                    range: loc.range,
                    new_text: new_name.clone(),
                });
            }
            return Ok(Some(WorkspaceEdit {
                changes: Some(edits),
                document_changes: None,
                change_annotations: None,
            }));
        }

        let mut changes: std::collections::HashMap<lsp_types::Uri, Vec<TextEdit>> =
            std::collections::HashMap::new();
        // Dedupe by physical file; see `handle_references`.
        let mut seen_files: std::collections::HashSet<String> = std::collections::HashSet::new();
        seen_files.insert(canonical_key(&uri_string));

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
            if !seen_files.insert(canonical_key(file_uri)) {
                continue;
            }

            let file_content = entry.value();
            let search_names = self.get_search_names_for_file(file_uri, &uri_string, &old_name);

            for search_name in search_names {
                let refs = {
                    let mut parser = self.parser_engine.lock().unwrap();
                    parser.find_references(file_content, &search_name)
                };

                // Search names are either the plain symbol, `<prefix><symbol>`
                // for prefix imports, or an alias. Only the prefixed form is
                // rewritten; an alias binding (and its usages) must stay as-is,
                // since the renamed export is referenced through the alias.
                let new_name_for_file = if search_name == old_name {
                    new_name.clone()
                } else if let Some(prefix) = search_name.strip_suffix(&old_name) {
                    format!("{}{}", prefix, new_name)
                } else {
                    continue;
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
