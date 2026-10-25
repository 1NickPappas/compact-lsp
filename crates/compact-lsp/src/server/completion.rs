//! Completion helpers for the server.

use super::*;

impl CompactLanguageServer {
    pub(super) async fn handle_completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
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

            // `contract implements <Type>;` — offer contract types.
            if utils::is_after_contract_implements(&content, position.line, position.character) {
                let mut names = std::collections::BTreeSet::new();
                {
                    let mut parser = self.parser_engine.lock().unwrap();
                    for sym in parser
                        .get_completion_symbols(&content)
                        .into_iter()
                        .filter(|s| s.kind == CompletionSymbolKind::Contract)
                    {
                        names.insert(sym.name);
                    }
                }
                for entry in self.symbol_cache.iter() {
                    for sym in entry
                        .value()
                        .iter()
                        .filter(|s| s.kind == CompletionSymbolKind::Contract)
                    {
                        names.insert(sym.name.clone());
                    }
                }
                let items: Vec<CompletionItem> = names
                    .into_iter()
                    .map(|name| CompletionItem {
                        label: name,
                        kind: Some(CompletionItemKind::INTERFACE),
                        detail: Some("contract type".to_string()),
                        ..Default::default()
                    })
                    .collect();
                if !items.is_empty() {
                    return Ok(Some(CompletionResponse::Array(items)));
                }
            }

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
                let base = parse_base_text(&var_name);

                // Contract type? Offer its declared circuits (cross-contract calls).
                // Covers annotations, aliases, inference, and struct-field chains.
                if let Some((_file, contract)) =
                    self.resolve_base_contract_type(&uri, &content, &base)
                    && !contract.circuits.is_empty()
                {
                    let items: Vec<CompletionItem> = contract
                        .circuits
                        .iter()
                        .map(|c| CompletionItem {
                            label: c.name.clone(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some(format!("{}{}", contract.name, c.detail)),
                            documentation: c.documentation.clone().map(|d| {
                                Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: d,
                                })
                            }),
                            insert_text: Some(format!("{}({})", c.name, "$0")),
                            insert_text_format: Some(InsertTextFormat::SNIPPET),
                            ..Default::default()
                        })
                        .collect();
                    return Ok(Some(CompletionResponse::Array(items)));
                }

                let var_type = self
                    .resolve_base_raw_type(&uri, &content, &base)
                    .map(|(raw, scope_uri)| self.resolve_alias_in(&scope_uri, &raw))
                    .or_else(|| {
                        // `kernel` is implicitly available without a ledger declaration.
                        if var_name == "kernel" {
                            Some("Kernel".to_string())
                        } else {
                            None
                        }
                    });
                if let Some(type_str) = var_type {
                    let base_type = builtins::extract_base_type(&type_str);
                    tracing::debug!("Dot completion: resolved type = {}", base_type);
                    let methods = builtins::methods_for_type_for(base_type, self.language_version);
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
                CompletionSymbolKind::Contract => CompletionItemKind::INTERFACE,
            }
        }

        let file_imports = match self.documents.get(&uri) {
            Some(doc) => {
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

                {
                    let mut parser = self.parser_engine.lock().unwrap();
                    parser.get_imports(&content)
                }
            }
            _ => {
                vec![]
            }
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
                    // Selective imports expose only the listed symbols, under
                    // their alias when one is given.
                    let prefixed_name = if !import.elements.is_empty() {
                        match import.elements.iter().find(|(e, _)| e == &sym.name) {
                            Some((_, Some(alias))) => alias.clone(),
                            Some((e, None)) => e.clone(),
                            None => continue,
                        }
                    } else if prefix.is_empty() {
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
            ("implements", "Assert a contract implements a contract type"),
            ("emit", "Emit a Zswap effect"),
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

        // Standard-library structs and circuits, gated by language version.
        items.extend(stdlib_completion_items(self.language_version));

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
            (
                "contract implements",
                "contract implements snippet",
                "contract implements ${1:ContractType};",
                "Assert this contract implements a contract type",
            ),
            (
                "emit",
                "emit snippet",
                "emit(${1:effect});",
                "Emit a Zswap effect from a circuit",
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
}
