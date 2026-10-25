//! Completion concerns for the parser engine.

use super::*;

impl ParserEngine {
    /// Get all symbols in the source for completion.
    ///
    /// Returns symbols defined in the file (circuits, structs, enums, etc.)
    pub fn get_completion_symbols(&mut self, source: &str) -> Vec<CompletionSymbol> {
        let tree = match self.parse(source) {
            Some(tree) => tree,
            None => return vec![],
        };

        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let mut symbols = Vec::new();

        self.collect_completion_symbols(root, source_bytes, &mut symbols);

        symbols
    }

    /// Recursively collect completion symbols from the AST.
    pub(super) fn collect_completion_symbols(
        &self,
        node: Node,
        source: &[u8],
        symbols: &mut Vec<CompletionSymbol>,
    ) {
        let kind = node.kind();

        // Extract any doc comments preceding the definition (for relevant node types)
        let doc_comment = match kind {
            "cdefn" | "wdecl" | "struct" | "enumdef" | "ldecl" | "mdefn" | "ecdecl" => self
                .extract_doc_comment(node, source)
                .map(|doc| format!("\n\n---\n\n{}", doc))
                .unwrap_or_default(),
            _ => String::new(),
        };

        match kind {
            // Circuit definitions
            "cdefn" => {
                if let Some(name) = self.get_function_name(node, source) {
                    let params = self.extract_params(node, source);
                    let return_type = self.get_type_text(node, source).unwrap_or_default();
                    let detail = format!("({}): {}", params, return_type);
                    let location = self.node_to_symbol_location(node);
                    let doc = format!(
                        "Circuit function\n\n```compact\ncircuit {}{}\n```{}",
                        name, detail, doc_comment
                    );
                    symbols.push(CompletionSymbol {
                        name,
                        kind: CompletionSymbolKind::Function,
                        detail: Some(detail),
                        location: Some(location),
                        documentation: Some(doc),
                    });
                }
            }
            // Witness declarations
            "wdecl" => {
                if let Some(name) = self.get_function_name(node, source) {
                    let params = self.extract_params(node, source);
                    let return_type = self.get_type_text(node, source).unwrap_or_default();
                    let detail = format!("({}): {}", params, return_type);
                    let location = self.node_to_symbol_location(node);
                    let doc = format!(
                        "Witness function\n\n```compact\nwitness {}{}\n```{}",
                        name, detail, doc_comment
                    );
                    symbols.push(CompletionSymbol {
                        name,
                        kind: CompletionSymbolKind::Function,
                        detail: Some(detail),
                        location: Some(location),
                        documentation: Some(doc),
                    });
                }
            }
            // Contract types (contract Name { circuit … })
            "ecdecl" => {
                if let Some(name) = self.get_field_text(node, "name", source) {
                    let info = self.contract_type_info(node, source);
                    let location = self.node_to_symbol_location(node);
                    let circuits = info
                        .circuits
                        .iter()
                        .map(|c| format!("- `{}`", c.signature))
                        .collect::<Vec<_>>()
                        .join("\n");
                    let doc = format!(
                        "Contract type\n\n```compact\ncontract {}\n```\n\nCircuits:\n{}{}",
                        name, circuits, doc_comment
                    );
                    symbols.push(CompletionSymbol {
                        name,
                        kind: CompletionSymbolKind::Contract,
                        detail: Some("contract".to_string()),
                        location: Some(location),
                        documentation: Some(doc),
                    });
                }
            }
            // Struct definitions
            "struct" => {
                if let Some(name) = self.get_field_text(node, "name", source) {
                    let location = self.node_to_symbol_location(node);
                    let fields = self.extract_struct_fields(node, source);
                    let doc = if fields.is_empty() {
                        format!(
                            "Struct type\n\n```compact\nstruct {}\n```{}",
                            name, doc_comment
                        )
                    } else {
                        format!(
                            "Struct type\n\n```compact\nstruct {}\n```\n\nFields:\n{}{}",
                            name,
                            fields.join("\n"),
                            doc_comment
                        )
                    };
                    symbols.push(CompletionSymbol {
                        name,
                        kind: CompletionSymbolKind::Struct,
                        detail: Some("struct".to_string()),
                        location: Some(location),
                        documentation: Some(doc),
                    });
                }
            }
            // Enum definitions
            "enumdef" => {
                if let Some(name) = self.get_field_text(node, "name", source) {
                    let location = self.node_to_symbol_location(node);
                    let variants = self.extract_enum_variants(node, source);
                    let doc = if variants.is_empty() {
                        format!("Enum type\n\n```compact\nenum {}\n```{}", name, doc_comment)
                    } else {
                        format!(
                            "Enum type\n\n```compact\nenum {}\n```\n\nVariants: {}{}",
                            name,
                            variants.join(", "),
                            doc_comment
                        )
                    };
                    symbols.push(CompletionSymbol {
                        name,
                        kind: CompletionSymbolKind::Enum,
                        detail: Some("enum".to_string()),
                        location: Some(location),
                        documentation: Some(doc),
                    });
                }
            }
            // Ledger declarations
            "ldecl" => {
                if let Some(name) = self.get_field_text(node, "name", source) {
                    let type_text = self.get_field_text(node, "type", source);
                    let location = self.node_to_symbol_location(node);
                    let detail = type_text.as_ref().map(|t| format!("ledger: {}", t));
                    let doc = format!(
                        "Ledger state\n\n```compact\nledger {}: {}\n```{}",
                        name,
                        type_text.as_deref().unwrap_or("unknown"),
                        doc_comment
                    );
                    symbols.push(CompletionSymbol {
                        name,
                        kind: CompletionSymbolKind::Variable,
                        detail,
                        location: Some(location),
                        documentation: Some(doc),
                    });
                }
            }
            // Module definitions
            "mdefn" => {
                if let Some(name) = self.get_field_text(node, "name", source) {
                    let location = self.node_to_symbol_location(node);
                    let doc = format!(
                        "Module namespace\n\n```compact\nmodule {}\n```{}",
                        name, doc_comment
                    );
                    symbols.push(CompletionSymbol {
                        name,
                        kind: CompletionSymbolKind::Module,
                        detail: Some("module".to_string()),
                        location: Some(location),
                        documentation: Some(doc),
                    });
                }
            }
            _ => {}
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_completion_symbols(child, source, symbols);
        }
    }
}
