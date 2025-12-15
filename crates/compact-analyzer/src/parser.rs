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

/// Parameter information for signature help.
#[derive(Debug, Clone)]
pub struct ParameterInfo {
    /// Parameter label (e.g., "a: Field").
    pub label: String,
}

/// Signature information result.
#[derive(Debug, Clone)]
pub struct SignatureInfo {
    /// The full signature label (e.g., "circuit add(a: Field, b: Field): Field").
    pub label: String,
    /// Documentation for the signature.
    pub documentation: Option<String>,
    /// Parameters with their labels.
    pub parameters: Vec<ParameterInfo>,
    /// The index of the active parameter (0-based).
    pub active_parameter: u32,
}

/// Symbol kind for completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionSymbolKind {
    Function,
    Struct,
    Enum,
    Variable,
    Module,
}

/// Location of a symbol in the source code.
#[derive(Debug, Clone)]
pub struct SymbolLocation {
    /// Start line (0-based).
    pub start_line: u32,
    /// Start character (0-based).
    pub start_char: u32,
    /// End line (0-based).
    pub end_line: u32,
    /// End character (0-based).
    pub end_char: u32,
}

/// A symbol for completion.
#[derive(Debug, Clone)]
pub struct CompletionSymbol {
    /// The symbol name.
    pub name: String,
    /// The kind of symbol.
    pub kind: CompletionSymbolKind,
    /// Detail text (e.g., "(a: Field, b: Field): Field").
    pub detail: Option<String>,
    /// Location of the symbol definition.
    pub location: Option<SymbolLocation>,
    /// Documentation for the symbol.
    pub documentation: Option<String>,
}

/// Information about an import statement.
#[derive(Debug, Clone)]
pub struct ImportInfo {
    /// The import path (e.g., "../utils/Utils" or "CompactStandardLibrary").
    pub path: String,
    /// True if this is a file import (quoted path), false if it's an identifier import.
    pub is_file: bool,
    /// The prefix for imported symbols (e.g., "Utils_").
    pub prefix: Option<String>,
}

/// A syntax error detected by tree-sitter parsing.
#[derive(Debug, Clone)]
pub struct SyntaxError {
    /// The error message.
    pub message: String,
    /// The range where the error occurred.
    pub range: Range,
}

/// A semantic token for syntax highlighting.
#[derive(Debug, Clone)]
pub struct SemanticToken {
    /// The range of the token.
    pub range: Range,
    /// The type of the token.
    pub token_type: SemanticTokenType,
    /// Modifiers for the token.
    pub modifiers: Vec<SemanticTokenModifier>,
}

/// Semantic token types for syntax highlighting.
/// Order matters - these are indices into the LSP legend.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SemanticTokenType {
    Function = 0,
    Type = 1,
    Struct = 2,
    Enum = 3,
    EnumMember = 4,
    Parameter = 5,
    Property = 6,
    Variable = 7,
    Namespace = 8,
    TypeParameter = 9,
}

/// Semantic token modifiers for syntax highlighting.
/// These are bit flags (1 << modifier_index).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SemanticTokenModifier {
    Declaration = 0,
    Readonly = 1,
    DefaultLibrary = 2,
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

    /// Convert tree-sitter node position to SymbolLocation.
    fn node_to_symbol_location(&self, node: Node) -> SymbolLocation {
        let start = node.start_position();
        let end = node.end_position();
        SymbolLocation {
            start_line: start.row as u32,
            start_char: start.column as u32,
            end_line: end.row as u32,
            end_char: end.column as u32,
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
            // Compact uses "parg" for circuit parameters, "arg" for struct fields
            if child.kind() == "parg" || child.kind() == "arg" {
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
            // Circuit definitions use "function_name" node
            "cdefn" | "edecl" | "wdecl" => self.get_function_name(node, source),
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

    /// Get signature help for a function call at the given position.
    ///
    /// Returns signature information if the cursor is inside a function call.
    pub fn signature_help(&mut self, source: &str, line: u32, character: u32) -> Option<SignatureInfo> {
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

        // Walk up to find a function call expression
        let (call_node, func_name) = self.find_enclosing_call(node, source_bytes, point)?;

        // Count which parameter we're in (count commas before cursor)
        let active_param = self.count_active_parameter(call_node, point, source_bytes);

        // Find the function definition
        let def_node = self.find_definition_node(&func_name, root, source_bytes)?;

        // Build signature info
        self.build_signature_info(def_node, source_bytes, active_param)
    }

    /// Find an enclosing function call expression.
    fn find_enclosing_call<'a>(&self, node: Node<'a>, source: &[u8], cursor_point: tree_sitter::Point) -> Option<(Node<'a>, String)> {
        let mut current = Some(node);

        while let Some(n) = current {
            let kind = n.kind();

            // Check for function call patterns (Compact uses function_call_term)
            if kind == "function_call_term" {
                if let Some(name) = self.get_call_function_name(n, source) {
                    return Some((n, name));
                }
            }

            // For blocks, search children for ERROR nodes with function calls
            // This handles incomplete code while typing
            if kind == "block" {
                if let Some((error_node, name)) = self.find_error_call_in_block(n, source, cursor_point) {
                    return Some((error_node, name));
                }
            }

            current = n.parent();
        }

        None
    }

    /// Search a block for ERROR nodes containing function calls before cursor.
    fn find_error_call_in_block<'a>(&self, block: Node<'a>, source: &[u8], cursor_point: tree_sitter::Point) -> Option<(Node<'a>, String)> {
        let mut cursor = block.walk();

        for child in block.children(&mut cursor) {
            // Look for ERROR nodes or stmt containing ERROR
            if let Some(result) = self.find_error_call_recursive(child, source, cursor_point) {
                return Some(result);
            }
        }

        None
    }

    /// Recursively search for ERROR nodes with function calls.
    fn find_error_call_recursive<'a>(&self, node: Node<'a>, source: &[u8], cursor_point: tree_sitter::Point) -> Option<(Node<'a>, String)> {
        let kind = node.kind();

        if kind == "ERROR" {
            // Check if this ERROR has a function call and the cursor is after the opening paren
            if let Some(name) = self.get_call_function_name(node, source) {
                // Check if cursor is within or after this error node's range
                let start = node.start_position();
                let end = node.end_position();

                if cursor_point.row > start.row ||
                   (cursor_point.row == start.row && cursor_point.column >= start.column) {
                    if cursor_point.row < end.row + 1 ||
                       (cursor_point.row == end.row && cursor_point.column <= end.column + 10) {
                        return Some((node, name));
                    }
                }
            }
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(result) = self.find_error_call_recursive(child, source, cursor_point) {
                return Some(result);
            }
        }

        None
    }

    /// Get the function name from a call expression.
    fn get_call_function_name(&self, node: Node, source: &[u8]) -> Option<String> {
        let kind = node.kind();

        // Handle both complete function_call_term and incomplete ERROR nodes
        if kind == "function_call_term" || kind == "ERROR" {
            // Look for a "fun" child containing "id"
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "fun" {
                    // Get the id inside fun
                    let mut inner_cursor = child.walk();
                    for inner_child in child.children(&mut inner_cursor) {
                        if inner_child.kind() == "id" {
                            return Some(self.node_text(inner_child, source));
                        }
                    }
                }
            }
        }

        None
    }

    /// Count which parameter the cursor is in (0-based).
    fn count_active_parameter(&self, call_node: Node, cursor_point: tree_sitter::Point, source: &[u8]) -> u32 {
        let mut comma_count = 0;
        let mut in_args = false;

        let mut cursor = call_node.walk();
        for child in call_node.children(&mut cursor) {
            let child_kind = child.kind();

            // Start counting after opening paren
            if child_kind == "(" {
                in_args = true;
                continue;
            }

            // Stop at closing paren
            if child_kind == ")" {
                break;
            }

            if in_args && child_kind == "," {
                // Only count commas before the cursor
                if child.start_position().row < cursor_point.row
                    || (child.start_position().row == cursor_point.row
                        && child.start_position().column < cursor_point.column)
                {
                    comma_count += 1;
                }
            }
        }

        // Also check inside nested argument nodes
        let mut nested_cursor = call_node.walk();
        for child in call_node.children(&mut nested_cursor) {
            if child.kind() == "arguments" || child.kind() == "call_args" {
                comma_count += self.count_commas_before_cursor(child, cursor_point, source);
            }
        }

        comma_count
    }

    /// Count commas before cursor position in an arguments node.
    fn count_commas_before_cursor(&self, args_node: Node, cursor_point: tree_sitter::Point, _source: &[u8]) -> u32 {
        let mut count = 0;
        let mut cursor = args_node.walk();

        for child in args_node.children(&mut cursor) {
            if child.kind() == "," {
                if child.start_position().row < cursor_point.row
                    || (child.start_position().row == cursor_point.row
                        && child.start_position().column < cursor_point.column)
                {
                    count += 1;
                }
            }
        }

        count
    }

    /// Build SignatureInfo from a definition node.
    fn build_signature_info(&self, def_node: Node, source: &[u8], active_param: u32) -> Option<SignatureInfo> {
        let kind = def_node.kind();

        let (prefix, name, doc) = match kind {
            "cdefn" => {
                // cdefn uses "function_name" for the circuit name
                let name = self.get_function_name(def_node, source)?;
                ("circuit", name, "Circuit function")
            }
            "edecl" => {
                let name = self.get_function_name(def_node, source)?;
                ("circuit", name, "External circuit")
            }
            "wdecl" => {
                let name = self.get_function_name(def_node, source)?;
                ("witness", name, "Witness function")
            }
            _ => return None,
        };

        // Extract parameters
        let params = self.extract_param_infos(def_node, source);

        // Get return type
        let return_type = self.get_type_text(def_node, source).unwrap_or_default();

        // Build signature label
        let params_str: Vec<_> = params.iter().map(|p| p.label.as_str()).collect();
        let label = format!("{} {}({}): {}", prefix, name, params_str.join(", "), return_type);

        Some(SignatureInfo {
            label,
            documentation: Some(doc.to_string()),
            parameters: params,
            active_parameter: active_param,
        })
    }

    /// Get function name from a cdefn/edecl/wdecl node.
    fn get_function_name(&self, node: Node, source: &[u8]) -> Option<String> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "function_name" {
                return Some(self.node_text(child, source));
            }
        }
        None
    }

    /// Get return type from a cdefn/edecl/wdecl node.
    fn get_type_text(&self, node: Node, source: &[u8]) -> Option<String> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "type" {
                return Some(self.node_text(child, source));
            }
        }
        None
    }

    /// Extract parameter info for signature help.
    fn extract_param_infos(&self, node: Node, source: &[u8]) -> Vec<ParameterInfo> {
        let mut params = Vec::new();
        let mut cursor = node.walk();

        for child in node.children(&mut cursor) {
            // Compact uses "parg" for circuit parameters
            if child.kind() == "parg" || child.kind() == "arg" {
                let param_text = self.node_text(child, source);
                params.push(ParameterInfo { label: param_text });
            }
        }

        params
    }

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
    fn collect_completion_symbols(&self, node: Node, source: &[u8], symbols: &mut Vec<CompletionSymbol>) {
        let kind = node.kind();

        match kind {
            // Circuit definitions
            "cdefn" => {
                if let Some(name) = self.get_function_name(node, source) {
                    let params = self.extract_params(node, source);
                    let return_type = self.get_type_text(node, source).unwrap_or_default();
                    let detail = format!("({}): {}", params, return_type);
                    let location = self.node_to_symbol_location(node);
                    let doc = format!("Circuit function\n\n```compact\ncircuit {}{}\n```", name, detail);
                    symbols.push(CompletionSymbol {
                        name,
                        kind: CompletionSymbolKind::Function,
                        detail: Some(detail),
                        location: Some(location),
                        documentation: Some(doc),
                    });
                }
            }
            // External circuit declarations
            "edecl" => {
                if let Some(name) = self.get_function_name(node, source) {
                    let params = self.extract_params(node, source);
                    let return_type = self.get_type_text(node, source).unwrap_or_default();
                    let detail = format!("({}): {}", params, return_type);
                    let location = self.node_to_symbol_location(node);
                    let doc = format!("External circuit\n\n```compact\ncircuit {}{}\n```", name, detail);
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
                    let doc = format!("Witness function\n\n```compact\nwitness {}{}\n```", name, detail);
                    symbols.push(CompletionSymbol {
                        name,
                        kind: CompletionSymbolKind::Function,
                        detail: Some(detail),
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
                        format!("Struct type\n\n```compact\nstruct {}\n```", name)
                    } else {
                        format!("Struct type\n\n```compact\nstruct {}\n```\n\nFields:\n{}", name, fields.join("\n"))
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
                        format!("Enum type\n\n```compact\nenum {}\n```", name)
                    } else {
                        format!("Enum type\n\n```compact\nenum {}\n```\n\nVariants: {}", name, variants.join(", "))
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
                        "Ledger state\n\n```compact\nledger {}: {}\n```",
                        name,
                        type_text.as_deref().unwrap_or("unknown")
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
                    let doc = format!("Module namespace\n\n```compact\nmodule {}\n```", name);
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

    /// Get all import statements from the source.
    pub fn get_imports(&mut self, source: &str) -> Vec<ImportInfo> {
        let tree = match self.parse(source) {
            Some(tree) => tree,
            None => return vec![],
        };

        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let mut imports = Vec::new();

        self.collect_imports(root, source_bytes, &mut imports);
        imports
    }

    /// Get syntax errors from tree-sitter parsing.
    ///
    /// Returns immediate syntax errors detected by tree-sitter.
    /// These are lightweight and fast - suitable for live diagnostics on every keystroke.
    pub fn get_syntax_errors(&mut self, source: &str) -> Vec<SyntaxError> {
        let tree = match self.parse(source) {
            Some(t) => t,
            None => return vec![],
        };

        let mut errors = Vec::new();
        self.collect_syntax_errors(tree.root_node(), source.as_bytes(), &mut errors);
        errors
    }

    /// Recursively collect syntax errors from the AST.
    fn collect_syntax_errors(&self, node: Node, source: &[u8], errors: &mut Vec<SyntaxError>) {
        if node.is_error() {
            // ERROR node - unexpected token or invalid syntax
            let text = self.node_text(node, source);
            let message = if text.trim().is_empty() {
                "Syntax error: unexpected token".to_string()
            } else {
                format!("Syntax error: unexpected '{}'", text.chars().take(30).collect::<String>())
            };
            errors.push(SyntaxError {
                message,
                range: self.node_range(node),
            });
        } else if node.is_missing() {
            // MISSING node - expected token not found
            let kind = node.kind();
            let message = format!("Syntax error: missing {}", kind);
            errors.push(SyntaxError {
                message,
                range: self.node_range(node),
            });
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_syntax_errors(child, source, errors);
        }
    }

    /// Get semantic tokens for syntax highlighting.
    ///
    /// Returns tokens for circuits, types, parameters, etc. with semantic meaning.
    pub fn get_semantic_tokens(&mut self, source: &str) -> Vec<SemanticToken> {
        let tree = match self.parse(source) {
            Some(t) => t,
            None => return vec![],
        };

        let mut tokens = Vec::new();
        self.collect_semantic_tokens(tree.root_node(), source.as_bytes(), &mut tokens);

        // Sort by position (line, then character) - required for LSP delta encoding
        tokens.sort_by(|a, b| {
            a.range
                .start
                .line
                .cmp(&b.range.start.line)
                .then(a.range.start.character.cmp(&b.range.start.character))
        });

        tokens
    }

    /// Recursively collect semantic tokens from the AST.
    fn collect_semantic_tokens(
        &self,
        node: Node,
        source: &[u8],
        tokens: &mut Vec<SemanticToken>,
    ) {
        match node.kind() {
            // Circuit/function definitions
            "cdefn" | "edecl" | "wdecl" => {
                // Get the function_name node which contains the actual name
                if let Some(name_node) = node
                    .children(&mut node.walk())
                    .find(|n| n.kind() == "function_name")
                {
                    tokens.push(SemanticToken {
                        range: self.node_range(name_node),
                        token_type: SemanticTokenType::Function,
                        modifiers: vec![SemanticTokenModifier::Declaration],
                    });
                }
            }

            // Struct definitions
            "struct" => {
                if let Some(name) = node.child_by_field_name("name") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Struct,
                        modifiers: vec![SemanticTokenModifier::Declaration],
                    });
                }
            }

            // Enum definitions
            "enumdef" => {
                if let Some(name) = node.child_by_field_name("name") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Enum,
                        modifiers: vec![SemanticTokenModifier::Declaration],
                    });
                }
                // Also collect enum variants
                let mut cursor = node.walk();
                let enum_name = node.child_by_field_name("name").map(|n| self.node_text(n, source));
                for child in node.children(&mut cursor) {
                    if child.kind() == "id" {
                        let text = self.node_text(child, source);
                        // Skip the enum name itself
                        if Some(&text) != enum_name.as_ref() {
                            tokens.push(SemanticToken {
                                range: self.node_range(child),
                                token_type: SemanticTokenType::EnumMember,
                                modifiers: vec![SemanticTokenModifier::Declaration],
                            });
                        }
                    }
                }
            }

            // Parameters (parg for circuits)
            // parg has: pattern (which contains id), type
            "parg" => {
                if let Some(pattern) = node.child_by_field_name("pattern") {
                    // Pattern can be an id, tuple, or struct pattern
                    // For simple identifiers, get the id field
                    if let Some(id) = pattern.child_by_field_name("id") {
                        tokens.push(SemanticToken {
                            range: self.node_range(id),
                            token_type: SemanticTokenType::Parameter,
                            modifiers: vec![],
                        });
                    }
                }
            }

            // Struct fields (arg within struct)
            "arg" => {
                // Check if parent is a struct
                let is_struct_field = node.parent().map(|p| p.kind()) == Some("struct");
                if let Some(name) = node.child_by_field_name("id") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: if is_struct_field {
                            SemanticTokenType::Property
                        } else {
                            SemanticTokenType::Parameter
                        },
                        modifiers: vec![],
                    });
                }
            }

            // Type references (user-defined types like struct names)
            "tref" => {
                if let Some(name) = node.child_by_field_name("id") {
                    let text = self.node_text(name, source);
                    let modifiers = if is_builtin_type(&text) {
                        vec![SemanticTokenModifier::DefaultLibrary]
                    } else {
                        vec![]
                    };
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Type,
                        modifiers,
                    });
                }
            }

            // Built-in types (these are literal string nodes in the grammar)
            // In tree-sitter, these show up as anonymous nodes or as type children
            "type" => {
                // Check for built-in type keywords as direct children
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    let kind = child.kind();
                    if kind == "Boolean" || kind == "Field" {
                        tokens.push(SemanticToken {
                            range: self.node_range(child),
                            token_type: SemanticTokenType::Type,
                            modifiers: vec![SemanticTokenModifier::DefaultLibrary],
                        });
                    }
                }
            }

            // Module definitions
            "mdefn" => {
                if let Some(name) = node.child_by_field_name("name") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Namespace,
                        modifiers: vec![SemanticTokenModifier::Declaration],
                    });
                }
            }

            // Ledger declarations
            "ldecl" => {
                if let Some(name) = node.child_by_field_name("name") {
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Property,
                        modifiers: vec![
                            SemanticTokenModifier::Declaration,
                            SemanticTokenModifier::Readonly,
                        ],
                    });
                }
            }

            // Variable bindings (let/const)
            "let_binding" | "const_binding" => {
                if let Some(name) = node.child_by_field_name("id") {
                    let modifiers = if node.kind() == "const_binding" {
                        vec![SemanticTokenModifier::Declaration, SemanticTokenModifier::Readonly]
                    } else {
                        vec![SemanticTokenModifier::Declaration]
                    };
                    tokens.push(SemanticToken {
                        range: self.node_range(name),
                        token_type: SemanticTokenType::Variable,
                        modifiers,
                    });
                }
            }

            _ => {}
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_semantic_tokens(child, source, tokens);
        }
    }

    /// Recursively collect import statements from the AST.
    fn collect_imports(&self, node: Node, source: &[u8], imports: &mut Vec<ImportInfo>) {
        if node.kind() == "idecl" {
            if let Some(import_info) = self.extract_import(node, source) {
                imports.push(import_info);
            }
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_imports(child, source, imports);
        }
    }

    /// Extract import information from an idecl node.
    fn extract_import(&self, node: Node, source: &[u8]) -> Option<ImportInfo> {
        // Get the import_name node (field "id" in idecl)
        let import_name_node = node.child_by_field_name("id")?;

        // Find if it's a file or id import
        let mut cursor = import_name_node.walk();
        let mut path = None;
        let mut is_file = false;

        for child in import_name_node.children(&mut cursor) {
            match child.kind() {
                "file" => {
                    // File import - remove quotes
                    let text = self.node_text(child, source);
                    path = Some(text.trim_matches('"').to_string());
                    is_file = true;
                }
                "id" => {
                    // Identifier import (e.g., CompactStandardLibrary)
                    path = Some(self.node_text(child, source));
                    is_file = false;
                }
                _ => {}
            }
        }

        let path = path?;

        // Get the prefix if present
        let prefix = node
            .child_by_field_name("prefix")
            .and_then(|prefix_node| prefix_node.child_by_field_name("id"))
            .map(|id_node| self.node_text(id_node, source));

        Some(ImportInfo {
            path,
            is_file,
            prefix,
        })
    }
}

/// Check if a type name is a Compact built-in type.
fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "Field"
            | "Boolean"
            | "Uint"
            | "Bytes"
            | "Vector"
            | "Opaque"
            | "Counter"
            | "Void"
            | "Map"
            | "Set"
            | "Cell"
            | "Address"
    )
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

    #[test]
    fn test_signature_help() {
        let mut parser = ParserEngine::new();
        let source = r#"circuit add(a: Field, b: Field): Field {
    return a + b;
}

circuit main(): Field {
    return add(1, 2);
}"#;
        // Position inside add() call - after the opening paren (line 5, col 15)
        let info = parser.signature_help(source, 5, 15);
        assert!(info.is_some(), "Should find signature help");
        let info = info.unwrap();
        assert!(info.label.contains("add"), "Label should contain function name");
        assert_eq!(info.parameters.len(), 2, "Should have 2 parameters");
        assert_eq!(info.active_parameter, 0, "First parameter should be active");
    }

    #[test]
    fn test_signature_help_second_param() {
        let mut parser = ParserEngine::new();
        let source = r#"circuit add(a: Field, b: Field): Field {
    return a + b;
}

circuit main(): Field {
    return add(1, 2);
}"#;
        // Position after the comma (line 5, col 18)
        let info = parser.signature_help(source, 5, 18);
        assert!(info.is_some(), "Should find signature help");
        let info = info.unwrap();
        assert_eq!(info.active_parameter, 1, "Second parameter should be active");
    }

    #[test]
    fn test_signature_help_incomplete() {
        let mut parser = ParserEngine::new();
        // Incomplete code - user is still typing
        let source = r#"circuit add(a: Field, b: Field): Field {
    return a + b;
}

circuit main(): Field {
    return add(
}"#;
        // Position right after opening paren (line 5, col 15)
        let info = parser.signature_help(source, 5, 15);
        assert!(info.is_some(), "Should find signature help for incomplete call");
        let info = info.unwrap();
        assert!(info.label.contains("add"), "Label should contain function name");
    }

    #[test]
    fn test_completion_symbols() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}

struct Point {
    x: Field;
    y: Field;
}

enum Color {
    Red,
    Green,
    Blue,
}
"#;
        let symbols = parser.get_completion_symbols(source);

        // Should find circuit, struct, and enum
        assert!(symbols.iter().any(|s| s.name == "add" && s.kind == CompletionSymbolKind::Function));
        assert!(symbols.iter().any(|s| s.name == "Point" && s.kind == CompletionSymbolKind::Struct));
        assert!(symbols.iter().any(|s| s.name == "Color" && s.kind == CompletionSymbolKind::Enum));
    }

    #[test]
    fn test_get_imports() {
        let mut parser = ParserEngine::new();
        let source = r#"
import CompactStandardLibrary;
import "../utils/Utils" prefix Utils_;
import "../security/Initializable" prefix Init_;
import "no_prefix_file";

circuit main(): Field {
    return 1;
}
"#;
        let imports = parser.get_imports(source);

        // Should find all 4 imports
        assert_eq!(imports.len(), 4, "Should find 4 imports");

        // Standard library import (no prefix, not a file)
        let stdlib = imports.iter().find(|i| i.path == "CompactStandardLibrary");
        assert!(stdlib.is_some(), "Should find CompactStandardLibrary import");
        let stdlib = stdlib.unwrap();
        assert!(!stdlib.is_file, "Should not be a file import");
        assert!(stdlib.prefix.is_none(), "Should have no prefix");

        // Utils import with prefix
        let utils = imports.iter().find(|i| i.path == "../utils/Utils");
        assert!(utils.is_some(), "Should find Utils import");
        let utils = utils.unwrap();
        assert!(utils.is_file, "Should be a file import");
        assert_eq!(utils.prefix.as_deref(), Some("Utils_"), "Should have Utils_ prefix");

        // Initializable import with prefix
        let init = imports.iter().find(|i| i.path == "../security/Initializable");
        assert!(init.is_some(), "Should find Initializable import");
        let init = init.unwrap();
        assert!(init.is_file, "Should be a file import");
        assert_eq!(init.prefix.as_deref(), Some("Init_"), "Should have Init_ prefix");

        // No prefix file import
        let no_prefix = imports.iter().find(|i| i.path == "no_prefix_file");
        assert!(no_prefix.is_some(), "Should find no_prefix_file import");
        let no_prefix = no_prefix.unwrap();
        assert!(no_prefix.is_file, "Should be a file import");
        assert!(no_prefix.prefix.is_none(), "Should have no prefix");
    }

    #[test]
    fn test_module_scoped_completion() {
        let mut parser = ParserEngine::new();
        // This is exactly what Utils.compact contains
        let source = r#"
pragma language_version >= 0.16;

module Utils {
  export circuit add(a: Field, b: Field): Field {
    return a + b;
  }
}
"#;
        let symbols = parser.get_completion_symbols(source);

        // Should find "add" circuit inside the module
        let add_symbol = symbols.iter().find(|s| s.name == "add");
        assert!(add_symbol.is_some(), "Should find 'add' circuit inside module. Found: {:?}", symbols);

        let add_symbol = add_symbol.unwrap();
        assert_eq!(add_symbol.kind, CompletionSymbolKind::Function, "Should be a Function");
    }

    #[test]
    fn test_syntax_errors_valid_code() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}
"#;
        let errors = parser.get_syntax_errors(source);
        assert!(errors.is_empty(), "Valid code should have no syntax errors");
    }

    #[test]
    fn test_syntax_errors_missing_brace() {
        let mut parser = ParserEngine::new();
        // Missing closing brace
        let source = r#"circuit broken(): Field {
    return 1;
"#;
        let errors = parser.get_syntax_errors(source);
        assert!(!errors.is_empty(), "Missing brace should produce syntax error");
    }

    #[test]
    fn test_syntax_errors_unexpected_token() {
        let mut parser = ParserEngine::new();
        // Invalid syntax - unexpected token
        let source = r#"circuit !!!invalid(): Field { return 1; }"#;
        let errors = parser.get_syntax_errors(source);
        assert!(!errors.is_empty(), "Invalid identifier should produce syntax error");
    }

    #[test]
    fn test_syntax_errors_multiple() {
        let mut parser = ParserEngine::new();
        // Multiple syntax errors in different circuits
        let source = r#"
circuit broken1( {
    return 1;
}

circuit broken2(): Field
    return 2;
}

circuit broken3 {
    return 3;
}
"#;
        let errors = parser.get_syntax_errors(source);
        // Should find multiple syntax errors, not just the first one
        println!("Found {} syntax errors:", errors.len());
        for (i, err) in errors.iter().enumerate() {
            println!("  {}: {} at line {}", i + 1, err.message, err.range.start.line + 1);
        }
        assert!(errors.len() >= 2, "Should find multiple syntax errors, found {}", errors.len());
    }

    #[test]
    fn test_semantic_tokens_basic() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}

struct Point {
    x: Field;
    y: Field;
}
"#;
        let tokens = parser.get_semantic_tokens(source);

        // Should find tokens for: circuit name, params, types, struct name, fields
        assert!(!tokens.is_empty(), "Should find semantic tokens");

        // Check for function token (circuit name)
        let function_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Function)
            .collect();
        assert!(!function_tokens.is_empty(), "Should find function tokens");
        assert!(
            function_tokens.iter().any(|t| t.modifiers.contains(&SemanticTokenModifier::Declaration)),
            "Function should have Declaration modifier"
        );

        // Check for type tokens (Field)
        let type_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Type)
            .collect();
        assert!(!type_tokens.is_empty(), "Should find type tokens");
        assert!(
            type_tokens.iter().any(|t| t.modifiers.contains(&SemanticTokenModifier::DefaultLibrary)),
            "Field type should have DefaultLibrary modifier"
        );

        // Check for parameter tokens
        let param_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Parameter)
            .collect();
        assert!(!param_tokens.is_empty(), "Should find parameter tokens");

        // Check for struct token
        let struct_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Struct)
            .collect();
        assert!(!struct_tokens.is_empty(), "Should find struct tokens");

        // Check for property tokens (struct fields)
        let property_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Property)
            .collect();
        assert!(!property_tokens.is_empty(), "Should find property tokens for struct fields");
    }

    #[test]
    fn test_semantic_tokens_enum() {
        let mut parser = ParserEngine::new();
        let source = r#"
enum Color {
    Red,
    Green,
    Blue,
}
"#;
        let tokens = parser.get_semantic_tokens(source);

        // Check for enum token
        let enum_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Enum)
            .collect();
        assert!(!enum_tokens.is_empty(), "Should find enum token");

        // Check for enum member tokens
        let member_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::EnumMember)
            .collect();
        assert_eq!(member_tokens.len(), 3, "Should find 3 enum member tokens");
    }

    #[test]
    fn test_semantic_tokens_sorted() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit a(): Field { return 1; }
circuit b(): Field { return 2; }
"#;
        let tokens = parser.get_semantic_tokens(source);

        // Verify tokens are sorted by position
        for window in tokens.windows(2) {
            let a = &window[0];
            let b = &window[1];
            let a_pos = (a.range.start.line, a.range.start.character);
            let b_pos = (b.range.start.line, b.range.start.character);
            assert!(a_pos <= b_pos, "Tokens should be sorted by position");
        }
    }
}
