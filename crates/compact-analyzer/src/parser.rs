// This file is part of compact-lsp.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0

//! Tree-sitter parser wrapper for Compact language.
//!
//! Provides parsing capabilities for:
//! - Document symbols (outline view)
//! - Folding ranges

use lsp_types::{DocumentSymbol, FoldingRange, FoldingRangeKind, Position, Range, SymbolKind};
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
}
