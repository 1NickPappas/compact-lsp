// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Tree-sitter parser wrapper for Compact language.
//!
//! Provides parsing capabilities for:
//! - Document symbols (outline view)
//! - Folding ranges
//! - Hover information

use lsp_types::{DocumentSymbol, FoldingRange, FoldingRangeKind, Position, Range, SymbolKind};

/// Hover information result.
#[derive(Debug, Clone)]
pub struct HoverInfo {
    /// The content to display (markdown).
    pub content: String,
    /// The range of the hovered element.
    pub range: Option<Range>,
}

/// Definition location result.
#[derive(Debug, Clone)]
pub struct DefinitionLocation {
    /// The range where the definition is located.
    pub range: Range,
    /// The range of just the symbol name (for selection).
    pub selection_range: Range,
}
use tree_sitter::{Node, Parser, Tree};

/// Parser engine wrapping tree-sitter-compact.
pub struct ParserEngine {
    parser: Parser,
}

impl ParserEngine {
    /// Create a new parser engine.
    pub fn new() -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_compact::LANGUAGE.into())
            .expect("Failed to load Compact grammar");
        Self { parser }
    }

    /// Parse source code and return the syntax tree.
    pub fn parse(&mut self, source: &str) -> Option<Tree> {
        self.parser.parse(source, None)
    }

    /// Extract document symbols from source code.
    ///
    /// Returns a hierarchical list of symbols (functions, types, etc.)
    pub fn document_symbols(&mut self, source: &str) -> Vec<DocumentSymbol> {
        let tree = match self.parse(source) {
            Some(tree) => tree,
            None => return vec![],
        };

        let root = tree.root_node();
        let source_bytes = source.as_bytes();

        self.extract_symbols(root, source_bytes)
    }

    /// Recursively extract symbols from a node.
    fn extract_symbols(&self, node: Node, source: &[u8]) -> Vec<DocumentSymbol> {
        let mut symbols = Vec::new();

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(symbol) = self.node_to_symbol(child, source) {
                symbols.push(symbol);
            } else {
                // Recurse into children that aren't symbols themselves
                symbols.extend(self.extract_symbols(child, source));
            }
        }

        symbols
    }

    /// Convert a tree-sitter node to an LSP DocumentSymbol if applicable.
    fn node_to_symbol(&self, node: Node, source: &[u8]) -> Option<DocumentSymbol> {
        let kind = node.kind();

        let (name, symbol_kind, detail) = match kind {
            // Circuit definition: circuit name(...): Type { ... }
            "cdefn" => {
                let name = self.get_field_text(node, "id", source)?;
                (name, SymbolKind::FUNCTION, Some("circuit".to_string()))
            }
            // External circuit declaration: circuit name(...): Type;
            "edecl" => {
                let name = self.get_field_text(node, "id", source)?;
                (name, SymbolKind::FUNCTION, Some("external circuit".to_string()))
            }
            // Witness declaration: witness name(...): Type;
            "wdecl" => {
                let name = self.get_field_text(node, "id", source)?;
                (name, SymbolKind::FUNCTION, Some("witness".to_string()))
            }
            // Ledger declaration: ledger name: Type;
            "ldecl" => {
                let name = self.get_field_text(node, "name", source)?;
                (name, SymbolKind::VARIABLE, Some("ledger".to_string()))
            }
            // Struct definition: struct Name { ... }
            "struct" => {
                let name = self.get_field_text(node, "name", source)?;
                (name, SymbolKind::STRUCT, None)
            }
            // Enum definition: enum Name { ... }
            "enumdef" => {
                let name = self.get_field_text(node, "name", source)?;
                (name, SymbolKind::ENUM, None)
            }
            // Module definition: module Name { ... }
            "mdefn" => {
                let name = self.get_field_text(node, "name", source)?;
                (name, SymbolKind::MODULE, None)
            }
            // External contract: contract Name { ... }
            "ecdecl" => {
                let name = self.get_field_text(node, "name", source)?;
                (name, SymbolKind::CLASS, Some("contract".to_string()))
            }
            // Constructor
            "lconstructor" => {
                ("constructor".to_string(), SymbolKind::CONSTRUCTOR, None)
            }
            _ => return None,
        };

        let range = self.node_range(node);
        let selection_range = range; // Could be refined to just the name

        // Get children symbols (e.g., struct fields, enum variants)
        let children = self.extract_child_symbols(node, source);

        #[allow(deprecated)]
        Some(DocumentSymbol {
            name,
            detail,
            kind: symbol_kind,
            tags: None,
            deprecated: None,
            range,
            selection_range,
            children: if children.is_empty() {
                None
            } else {
                Some(children)
            },
        })
    }

    /// Extract child symbols (struct fields, enum variants, etc.)
    fn extract_child_symbols(&self, node: Node, source: &[u8]) -> Vec<DocumentSymbol> {
        let mut children = Vec::new();
        let kind = node.kind();

        match kind {
            // For structs, extract fields
            "struct" => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if child.kind() == "arg" {
                        if let Some(name) = self.get_field_text(child, "id", source) {
                            let range = self.node_range(child);
                            #[allow(deprecated)]
                            children.push(DocumentSymbol {
                                name,
                                detail: None,
                                kind: SymbolKind::FIELD,
                                tags: None,
                                deprecated: None,
                                range,
                                selection_range: range,
                                children: None,
                            });
                        }
                    }
                }
            }
            // For enums, extract variants
            "enumdef" => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if child.kind() == "id" {
                        let text = self.node_text(child, source);
                        // Skip the enum name itself
                        if Some(&text) != self.get_field_text(node, "name", source).as_ref() {
                            let range = self.node_range(child);
                            #[allow(deprecated)]
                            children.push(DocumentSymbol {
                                name: text,
                                detail: None,
                                kind: SymbolKind::ENUM_MEMBER,
                                tags: None,
                                deprecated: None,
                                range,
                                selection_range: range,
                                children: None,
                            });
                        }
                    }
                }
            }
            // For modules, recurse into module elements
            "mdefn" => {
                children = self.extract_symbols(node, source);
            }
            // For contracts, extract circuit declarations
            "ecdecl" => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if child.kind() == "ecdecl_circuit" {
                        if let Some(name) = self.get_field_text(child, "id", source) {
                            let range = self.node_range(child);
                            #[allow(deprecated)]
                            children.push(DocumentSymbol {
                                name,
                                detail: Some("circuit".to_string()),
                                kind: SymbolKind::METHOD,
                                tags: None,
                                deprecated: None,
                                range,
                                selection_range: range,
                                children: None,
                            });
                        }
                    }
                }
            }
            _ => {}
        }

        children
    }

    /// Get text from a named field of a node.
    fn get_field_text(&self, node: Node, field: &str, source: &[u8]) -> Option<String> {
        let child = node.child_by_field_name(field)?;
        Some(self.node_text(child, source))
    }

    /// Get text content of a node.
    fn node_text(&self, node: Node, source: &[u8]) -> String {
        node.utf8_text(source).unwrap_or("").to_string()
    }

    /// Convert tree-sitter node position to LSP Range.
    fn node_range(&self, node: Node) -> Range {
        let start = node.start_position();
        let end = node.end_position();
        Range {
            start: Position {
                line: start.row as u32,
                character: start.column as u32,
            },
            end: Position {
                line: end.row as u32,
                character: end.column as u32,
            },
        }
    }

    /// Extract folding ranges from source code.
    pub fn folding_ranges(&mut self, source: &str) -> Vec<FoldingRange> {
        let tree = match self.parse(source) {
            Some(tree) => tree,
            None => return vec![],
        };

        let root = tree.root_node();
        let mut ranges = Vec::new();

        self.collect_folding_ranges(root, &mut ranges);

        ranges
    }

    /// Recursively collect folding ranges.
    fn collect_folding_ranges(&self, node: Node, ranges: &mut Vec<FoldingRange>) {
        let kind = node.kind();

        // Determine if this node should be foldable
        let fold_kind = match kind {
            // Code blocks
            "block" | "cdefn" | "struct" | "enumdef" | "mdefn" | "ecdecl" | "lconstructor" => {
                Some(FoldingRangeKind::Region)
            }
            // Comments
            "comment" => Some(FoldingRangeKind::Comment),
            // Control flow
            "if_stmt" | "for_stmt" => Some(FoldingRangeKind::Region),
            _ => None,
        };

        if let Some(fold_kind) = fold_kind {
            let start = node.start_position();
            let end = node.end_position();

            // Only fold if spans multiple lines
            if end.row > start.row {
                ranges.push(FoldingRange {
                    start_line: start.row as u32,
                    start_character: Some(start.column as u32),
                    end_line: end.row as u32,
                    end_character: Some(end.column as u32),
                    kind: Some(fold_kind),
                    collapsed_text: None,
                });
            }
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_folding_ranges(child, ranges);
        }
    }

    /// Get hover information for a position in the source code.
    pub fn hover_info(&mut self, source: &str, line: u32, character: u32) -> Option<HoverInfo> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();

        // Convert LSP position to tree-sitter point
        let point = tree_sitter::Point {
            row: line as usize,
            column: character as usize,
        };

        // Find the deepest node at this position
        let node = root.descendant_for_point_range(point, point)?;

        self.hover_for_node(node, source_bytes, &root)
    }

    /// Get hover info for a specific node.
    fn hover_for_node(&self, node: Node, source: &[u8], root: &Node) -> Option<HoverInfo> {
        let kind = node.kind();
        let text = self.node_text(node, source);

        // Check if this is a keyword
        if let Some(doc) = self.keyword_docs(&text) {
            return Some(HoverInfo {
                content: doc,
                range: Some(self.node_range(node)),
            });
        }

        // Check if this is a built-in type
        if let Some(doc) = self.builtin_type_docs(&text) {
            return Some(HoverInfo {
                content: doc,
                range: Some(self.node_range(node)),
            });
        }

        // Check if hovering on a definition
        if let Some(parent) = node.parent() {
            if let Some(info) = self.definition_hover(parent, source) {
                return Some(info);
            }
        }

        // Check if hovering on an identifier - try to find its definition
        if kind == "id" {
            if let Some(info) = self.find_definition_hover(&text, root, source) {
                return Some(info);
            }
        }

        None
    }

    /// Get documentation for Compact keywords.
    fn keyword_docs(&self, text: &str) -> Option<String> {
        let doc = match text {
            "pragma" => "**pragma**\n\nDeclares compiler version requirements.\n\n```compact\npragma compact >=0.1.0;\n```",
            "import" => "**import**\n\nImports a module or specific symbols.\n\n```compact\nimport MyModule;\nimport { symbol1, symbol2 } from OtherModule;\n```",
            "export" => "**export**\n\nExports a declaration for use by other modules.\n\n```compact\nexport circuit myCircuit(): Field { ... }\nexport struct MyStruct { ... }\n```",
            "module" => "**module**\n\nDefines a module namespace.\n\n```compact\nmodule MyModule {\n  // declarations\n}\n```",
            "circuit" => "**circuit**\n\nDefines a circuit function that executes in zero-knowledge.\n\n```compact\ncircuit add(a: Field, b: Field): Field {\n  return a + b;\n}\n```",
            "witness" => "**witness**\n\nDeclares a witness function that provides private inputs.\n\n```compact\nwitness get_secret(): Field;\n```",
            "ledger" => "**ledger**\n\nDeclares on-chain state storage.\n\n```compact\nledger balance: Map<Address, Uint<64>>;\n```",
            "struct" => "**struct**\n\nDefines a composite data type.\n\n```compact\nstruct Point {\n  x: Field;\n  y: Field;\n}\n```",
            "enum" => "**enum**\n\nDefines an enumeration type.\n\n```compact\nenum Color {\n  Red,\n  Green,\n  Blue,\n}\n```",
            "contract" => "**contract**\n\nDeclares an external contract interface.\n\n```compact\ncontract Token {\n  circuit transfer(to: Address, amount: Uint<64>): Boolean;\n}\n```",
            "constructor" => "**constructor**\n\nDefines the contract initialization function.\n\n```compact\nconstructor(initial_value: Field) {\n  ledger.value = initial_value;\n}\n```",
            "return" => "**return**\n\nReturns a value from a circuit.\n\n```compact\nreturn result;\n```",
            "if" => "**if**\n\nConditional statement.\n\n```compact\nif (condition) {\n  // then branch\n} else {\n  // else branch\n}\n```",
            "else" => "**else**\n\nElse branch of a conditional.\n\n```compact\nif (condition) {\n  // then\n} else {\n  // else\n}\n```",
            "for" => "**for**\n\nLoop over a range.\n\n```compact\nfor (const i of 0..10) {\n  // loop body\n}\n```",
            "const" => "**const**\n\nDeclares a constant value.\n\n```compact\nconst x = 42;\nconst PI: Field = 3;\n```",
            "assert" => "**assert**\n\nAsserts a condition with an error message.\n\n```compact\nassert balance >= amount \"Insufficient balance\";\n```",
            "map" => "**map**\n\nMaps a function over elements.\n\n```compact\nconst doubled = map(values, |x| x * 2);\n```",
            "fold" => "**fold**\n\nReduces elements to a single value.\n\n```compact\nconst sum = fold(values, 0, |acc, x| acc + x);\n```",
            "disclose" => "**disclose**\n\nDiscloses a private value publicly.\n\n```compact\ndisclose(secret_value);\n```",
            "pure" => "**pure**\n\nMarks a circuit as having no side effects.\n\n```compact\nexport pure circuit add(a: Field, b: Field): Field { ... }\n```",
            "sealed" => "**sealed**\n\nMarks a ledger state as immutable after initialization.\n\n```compact\nsealed ledger config: Config;\n```",
            "true" => "**true**\n\nBoolean true literal.",
            "false" => "**false**\n\nBoolean false literal.",
            _ => return None,
        };
        Some(doc.to_string())
    }

    /// Get documentation for built-in types.
    fn builtin_type_docs(&self, text: &str) -> Option<String> {
        let doc = match text {
            "Boolean" => "**Boolean**\n\nBoolean type with values `true` and `false`.\n\n```compact\nconst flag: Boolean = true;\n```",
            "Field" => "**Field**\n\nField element - the native arithmetic type for ZK circuits.\n\nSupports addition, subtraction, multiplication, and division.\n\n```compact\nconst x: Field = 42;\nconst y = x * 2 + 1;\n```",
            "Uint" => "**Uint<N>**\n\nUnsigned integer with `N` bits.\n\n```compact\nconst amount: Uint<64> = 1000;\nconst small: Uint<8> = 255;\n```",
            "Bytes" => "**Bytes<N>**\n\nFixed-size byte array with `N` bytes.\n\n```compact\nconst hash: Bytes<32> = ...;\nconst data: Bytes<64> = ...;\n```",
            "Vector" => "**Vector<N, T>**\n\nFixed-size array of `N` elements of type `T`.\n\n```compact\nconst values: Vector<10, Field> = ...;\nconst flags: Vector<8, Boolean> = ...;\n```",
            "Opaque" => "**Opaque<\"name\">**\n\nOpaque type wrapper for external data.\n\n```compact\nconst external: Opaque<\"commitment\"> = ...;\n```",
            "Map" => "**Map<K, V>**\n\nKey-value mapping for ledger state.\n\n```compact\nledger balances: Map<Address, Uint<64>>;\n```",
            "Set" => "**Set<T>**\n\nSet collection for ledger state.\n\n```compact\nledger members: Set<Address>;\n```",
            "Counter" => "**Counter**\n\nAtomic counter for ledger state.\n\n```compact\nledger nonce: Counter;\n```",
            "Address" => "**Address**\n\nBlockchain address type.\n\n```compact\nconst recipient: Address = ...;\n```",
            "Cell" => "**Cell<T>**\n\nMutable cell for ledger state.\n\n```compact\nledger value: Cell<Field>;\n```",
            _ => return None,
        };
        Some(doc.to_string())
    }

    /// Get hover info for a definition node.
    fn definition_hover(&self, node: Node, source: &[u8]) -> Option<HoverInfo> {
        let kind = node.kind();

        match kind {
            "cdefn" => {
                let signature = self.extract_circuit_signature(node, source)?;
                Some(HoverInfo {
                    content: format!("```compact\n{}\n```\n\nCircuit function", signature),
                    range: Some(self.node_range(node)),
                })
            }
            "edecl" => {
                let signature = self.extract_circuit_signature(node, source)?;
                Some(HoverInfo {
                    content: format!("```compact\n{}\n```\n\nExternal circuit declaration", signature),
                    range: Some(self.node_range(node)),
                })
            }
            "wdecl" => {
                let signature = self.extract_witness_signature(node, source)?;
                Some(HoverInfo {
                    content: format!("```compact\n{}\n```\n\nWitness function", signature),
                    range: Some(self.node_range(node)),
                })
            }
            "ldecl" => {
                let name = self.get_field_text(node, "name", source)?;
                let type_text = self.get_field_text(node, "type", source).unwrap_or_default();
                Some(HoverInfo {
                    content: format!("```compact\nledger {}: {}\n```\n\nLedger state", name, type_text),
                    range: Some(self.node_range(node)),
                })
            }
            "struct" => {
                let name = self.get_field_text(node, "name", source)?;
                let fields = self.extract_struct_fields(node, source);
                let fields_str = if fields.is_empty() {
                    String::new()
                } else {
                    format!("\n\nFields:\n{}", fields.join("\n"))
                };
                Some(HoverInfo {
                    content: format!("```compact\nstruct {}\n```\n\nStruct type{}", name, fields_str),
                    range: Some(self.node_range(node)),
                })
            }
            "enumdef" => {
                let name = self.get_field_text(node, "name", source)?;
                let variants = self.extract_enum_variants(node, source);
                let variants_str = if variants.is_empty() {
                    String::new()
                } else {
                    format!("\n\nVariants: {}", variants.join(", "))
                };
                Some(HoverInfo {
                    content: format!("```compact\nenum {}\n```\n\nEnum type{}", name, variants_str),
                    range: Some(self.node_range(node)),
                })
            }
            _ => None,
        }
    }

    /// Extract circuit function signature.
    fn extract_circuit_signature(&self, node: Node, source: &[u8]) -> Option<String> {
        let name = self.get_field_text(node, "id", source)?;
        let params = self.extract_params(node, source);
        let return_type = self.get_field_text(node, "rtype", source).unwrap_or_default();
        Some(format!("circuit {}({}): {}", name, params, return_type))
    }

    /// Extract witness function signature.
    fn extract_witness_signature(&self, node: Node, source: &[u8]) -> Option<String> {
        let name = self.get_field_text(node, "id", source)?;
        let params = self.extract_params(node, source);
        let return_type = self.get_field_text(node, "rtype", source).unwrap_or_default();
        Some(format!("witness {}({}): {}", name, params, return_type))
    }

    /// Extract function parameters as a string.
    fn extract_params(&self, node: Node, source: &[u8]) -> String {
        let mut params = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "arg" {
                let param_text = self.node_text(child, source);
                params.push(param_text);
            }
        }
        params.join(", ")
    }

    /// Extract struct field names.
    fn extract_struct_fields(&self, node: Node, source: &[u8]) -> Vec<String> {
        let mut fields = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "arg" {
                let field_text = self.node_text(child, source);
                fields.push(format!("- `{}`", field_text));
            }
        }
        fields
    }

    /// Extract enum variant names.
    fn extract_enum_variants(&self, node: Node, source: &[u8]) -> Vec<String> {
        let mut variants = Vec::new();
        let enum_name = self.get_field_text(node, "name", source);
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "id" {
                let text = self.node_text(child, source);
                // Skip the enum name itself
                if Some(&text) != enum_name.as_ref() {
                    variants.push(format!("`{}`", text));
                }
            }
        }
        variants
    }

    /// Find a definition by name and return hover info.
    fn find_definition_hover(&self, name: &str, root: &Node, source: &[u8]) -> Option<HoverInfo> {
        self.find_definition_node(name, *root, source)
            .and_then(|def_node| self.definition_hover(def_node, source))
    }

    /// Recursively search for a definition with the given name.
    fn find_definition_node<'a>(&self, name: &str, node: Node<'a>, source: &[u8]) -> Option<Node<'a>> {
        let kind = node.kind();

        // Check if this node is a definition with matching name
        let def_name = match kind {
            "cdefn" | "edecl" | "wdecl" => self.get_field_text(node, "id", source),
            "ldecl" | "struct" | "enumdef" | "mdefn" | "ecdecl" => self.get_field_text(node, "name", source),
            _ => None,
        };

        if def_name.as_deref() == Some(name) {
            return Some(node);
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_definition_node(name, child, source) {
                return Some(found);
            }
        }

        None
    }

    /// Go to definition for the symbol at the given position.
    ///
    /// Returns the location of the definition if found.
    pub fn goto_definition(&mut self, source: &str, line: u32, character: u32) -> Option<DefinitionLocation> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();

        // Convert LSP position to tree-sitter point
        let point = tree_sitter::Point {
            row: line as usize,
            column: character as usize,
        };

        // Find the node at this position
        let node = root.descendant_for_point_range(point, point)?;

        // Get the identifier text
        let text = self.node_text(node, source_bytes);

        // Skip if not an identifier or if it's a keyword/builtin
        if node.kind() != "id" {
            return None;
        }

        // Check if this is a keyword or builtin type (no definition to go to)
        if self.keyword_docs(&text).is_some() || self.builtin_type_docs(&text).is_some() {
            return None;
        }

        // Check if we're already on a definition
        if let Some(parent) = node.parent() {
            let parent_kind = parent.kind();
            match parent_kind {
                "cdefn" | "edecl" | "wdecl" => {
                    if self.get_field_text(parent, "id", source_bytes).as_deref() == Some(&text) {
                        // We're on the definition itself
                        return Some(DefinitionLocation {
                            range: self.node_range(parent),
                            selection_range: self.node_range(node),
                        });
                    }
                }
                "ldecl" | "struct" | "enumdef" | "mdefn" | "ecdecl" => {
                    if self.get_field_text(parent, "name", source_bytes).as_deref() == Some(&text) {
                        // We're on the definition itself
                        return Some(DefinitionLocation {
                            range: self.node_range(parent),
                            selection_range: self.node_range(node),
                        });
                    }
                }
                _ => {}
            }
        }

        // Search for the definition
        let def_node = self.find_definition_node(&text, root, source_bytes)?;

        // Get the name node for selection range
        let name_range = self.get_definition_name_range(def_node, source_bytes)
            .unwrap_or_else(|| self.node_range(def_node));

        Some(DefinitionLocation {
            range: self.node_range(def_node),
            selection_range: name_range,
        })
    }

    /// Get the range of the name within a definition node.
    fn get_definition_name_range(&self, node: Node, _source: &[u8]) -> Option<Range> {
        let kind = node.kind();

        let name_node = match kind {
            "cdefn" | "edecl" | "wdecl" => node.child_by_field_name("id"),
            "ldecl" | "struct" | "enumdef" | "mdefn" | "ecdecl" => node.child_by_field_name("name"),
            _ => None,
        }?;

        Some(self.node_range(name_node))
    }
}

impl Default for ParserEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_circuit() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}
"#;
        let symbols = parser.document_symbols(source);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "add");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_parse_struct() {
        let mut parser = ParserEngine::new();
        let source = r#"
struct Point {
    x: Field;
    y: Field;
}
"#;
        let symbols = parser.document_symbols(source);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Point");
        assert_eq!(symbols[0].kind, SymbolKind::STRUCT);
    }

    #[test]
    fn test_folding_ranges() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit test(): Field {
    const x = 1;
    return x;
}
"#;
        let ranges = parser.folding_ranges(source);
        assert!(!ranges.is_empty());
    }

    #[test]
    fn test_hover_keyword() {
        let mut parser = ParserEngine::new();
        let source = "circuit test(): Field { return 1; }";
        // Hover on "circuit" keyword at position (0, 0)
        let info = parser.hover_info(source, 0, 0);
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(info.content.contains("circuit"));
    }

    #[test]
    fn test_hover_builtin_type() {
        let mut parser = ParserEngine::new();
        let source = "circuit test(): Field { return 1; }";
        // Hover on "Field" type at position (0, 16)
        let info = parser.hover_info(source, 0, 16);
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(info.content.contains("Field"));
    }

    #[test]
    fn test_hover_circuit_definition() {
        let mut parser = ParserEngine::new();
        let source = "circuit add(a: Field, b: Field): Field { return a + b; }";
        // Hover on "add" at position (0, 8)
        let info = parser.hover_info(source, 0, 8);
        assert!(info.is_some());
        let info = info.unwrap();
        assert!(info.content.contains("add"));
        assert!(info.content.contains("Circuit function"));
    }

    #[test]
    fn test_goto_definition_circuit() {
        let mut parser = ParserEngine::new();
        let source = r#"circuit helper(): Field { return 1; }
circuit main(): Field { return helper(); }"#;
        // Go to definition of "helper" in main (line 1, around column 32)
        let loc = parser.goto_definition(source, 1, 32);
        assert!(loc.is_some());
        let loc = loc.unwrap();
        // Should point to line 0 where helper is defined
        assert_eq!(loc.selection_range.start.line, 0);
    }

    #[test]
    fn test_goto_definition_struct() {
        let mut parser = ParserEngine::new();
        let source = r#"struct Point { x: Field; y: Field; }
circuit make_point(): Point { return Point { x: 0, y: 0 }; }"#;
        // Go to definition of "Point" in make_point return type (line 1, around column 22)
        let loc = parser.goto_definition(source, 1, 22);
        assert!(loc.is_some());
        let loc = loc.unwrap();
        // Should point to line 0 where Point is defined
        assert_eq!(loc.selection_range.start.line, 0);
    }
}
