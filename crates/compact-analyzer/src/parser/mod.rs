//! Tree-sitter parser wrapper for Compact language.
//!
//! Provides parsing capabilities for:
//! - Document symbols (outline view)
//! - Folding ranges
//! - Hover information
//! - Go to definition
//! - Signature help
//! - Completion symbols
//! - Imports extraction
//! - Syntax errors
//! - Semantic tokens
//! - Find references

mod types;

pub use types::*;

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
            // Type alias: [export] [new] type Name [<…>] = Type;  (language 0.23)
            // The crates.io `tree-sitter-compact` grammar names this `type_decl`;
            // the old `feat/grammar-v0.30.0` branch names it `type_alias_declaration`.
            "type_alias_declaration" | "type_decl" => {
                let name = self.get_field_text(node, "name", source)?;
                (name, SymbolKind::INTERFACE, Some("type".to_string()))
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
            "lconstructor" => ("constructor".to_string(), SymbolKind::CONSTRUCTOR, None),
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
                    if child.kind() == "arg"
                        && let Some(name) = self.get_field_text(child, "id", source)
                    {
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
                    if child.kind() == "ecdecl_circuit"
                        && let Some(name) = self.get_field_text(child, "id", source)
                    {
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

    /// Extract documentation comments preceding a node.
    /// Collects contiguous comment lines directly above a declaration.
    fn extract_doc_comment(&self, node: Node, source: &[u8]) -> Option<String> {
        let mut comments = Vec::new();
        let mut current = node.prev_sibling();

        // Walk backwards collecting contiguous comments
        while let Some(sibling) = current {
            if sibling.kind() == "comment" {
                let text = self.node_text(sibling, source);
                let cleaned = self.clean_comment_text(&text);
                if !cleaned.is_empty() {
                    comments.push(cleaned);
                }
                current = sibling.prev_sibling();
            } else {
                // Hit non-comment, stop
                break;
            }
        }

        if !comments.is_empty() {
            comments.reverse();
            return Some(comments.join("\n"));
        }

        // The grammar only tokenizes `//` line comments, so block comments
        // (`/* ... */`, `/** ... */`) never appear as sibling `comment` nodes.
        // Fall back to scanning the raw source directly above the declaration.
        let prefix = std::str::from_utf8(&source[..node.start_byte()]).ok()?;
        let trimmed = prefix.trim_end();
        if trimmed.ends_with("*/") {
            let start = trimmed.rfind("/*")?;
            let cleaned = self.clean_comment_text(&trimmed[start..]);
            if !cleaned.is_empty() {
                return Some(cleaned);
            }
        }

        None
    }

    /// Clean comment text by removing markers and normalizing.
    fn clean_comment_text(&self, text: &str) -> String {
        let text = text.trim();

        // Handle single-line comments: // ...
        if let Some(stripped) = text.strip_prefix("//") {
            return stripped.trim().to_string();
        }

        // Handle block comments: /* ... */ or /** ... */
        if text.starts_with("/*") && text.ends_with("*/") {
            let inner = &text[2..text.len() - 2];
            // Remove leading * from JSDoc-style (/** */)
            let inner = inner.trim_start_matches('*');

            // Process multi-line: remove leading * from each line
            let lines: Vec<&str> = inner
                .lines()
                .map(|line| line.trim().trim_start_matches('*').trim())
                .filter(|line| !line.is_empty())
                .collect();

            return lines.join("\n");
        }

        text.to_string()
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
    #[allow(clippy::only_used_in_recursion)]
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
        if let Some(parent) = node.parent()
            && let Some(info) = self.definition_hover(parent, source)
        {
            return Some(info);
        }

        // Check if hovering on an identifier - try to find its definition
        if kind == "id"
            && let Some(info) = self.find_definition_hover(&text, root, source)
        {
            return Some(info);
        }

        None
    }

    /// Get documentation for Compact keywords.
    fn keyword_docs(&self, text: &str) -> Option<String> {
        let doc = match text {
            "pragma" => {
                "**pragma**\n\nDeclares compiler version requirements.\n\n```compact\npragma compact >=0.1.0;\n```"
            }
            "import" => {
                "**import**\n\nImports a module or specific symbols.\n\n```compact\nimport MyModule;\nimport { symbol1, symbol2 } from OtherModule;\n```"
            }
            "export" => {
                "**export**\n\nExports a declaration for use by other modules.\n\n```compact\nexport circuit myCircuit(): Field { ... }\nexport struct MyStruct { ... }\n```"
            }
            "module" => {
                "**module**\n\nDefines a module namespace.\n\n```compact\nmodule MyModule {\n  // declarations\n}\n```"
            }
            "circuit" => {
                "**circuit**\n\nDefines a circuit function that executes in zero-knowledge.\n\n```compact\ncircuit add(a: Field, b: Field): Field {\n  return a + b;\n}\n```"
            }
            "witness" => {
                "**witness**\n\nDeclares a witness function that provides private inputs.\n\n```compact\nwitness get_secret(): Field;\n```"
            }
            "ledger" => {
                "**ledger**\n\nDeclares on-chain state storage.\n\n```compact\nledger balance: Map<Address, Uint<64>>;\n```"
            }
            "struct" => {
                "**struct**\n\nDefines a composite data type.\n\n```compact\nstruct Point {\n  x: Field;\n  y: Field;\n}\n```"
            }
            "enum" => {
                "**enum**\n\nDefines an enumeration type.\n\n```compact\nenum Color {\n  Red,\n  Green,\n  Blue,\n}\n```"
            }
            "contract" => {
                "**contract**\n\nDeclares an external contract interface (a *contract type*).\n\n```compact\ncontract Token {\n  circuit transfer(to: Address, amount: Uint<64>): Boolean;\n}\n```"
            }
            "implements" => {
                "**implements**\n\nAsserts that the enclosing contract implements a contract type:\n\n```compact\ncontract implements Token;\n```\n\nThe compiler verifies that every circuit declared by the contract type is present with a matching signature. Requires language 0.26+."
            }
            "emit" => {
                "**emit**\n\nEmits a Zswap effect from a circuit:\n\n```compact\nemit(ShieldedSpend { nullifier: disclose(n) });\n```\n\nRequires language 0.26+."
            }
            "constructor" => {
                "**constructor**\n\nDefines the contract initialization function.\n\n```compact\nconstructor(initial_value: Field) {\n  ledger.value = initial_value;\n}\n```"
            }
            "return" => {
                "**return**\n\nReturns a value from a circuit.\n\n```compact\nreturn result;\n```"
            }
            "if" => {
                "**if**\n\nConditional statement.\n\n```compact\nif (condition) {\n  // then branch\n} else {\n  // else branch\n}\n```"
            }
            "else" => {
                "**else**\n\nElse branch of a conditional.\n\n```compact\nif (condition) {\n  // then\n} else {\n  // else\n}\n```"
            }
            "for" => {
                "**for**\n\nLoop over a range.\n\n```compact\nfor (const i of 0..10) {\n  // loop body\n}\n```"
            }
            "const" => {
                "**const**\n\nDeclares a constant value.\n\n```compact\nconst x = 42;\nconst PI: Field = 3;\n```"
            }
            "assert" => {
                "**assert**\n\nAsserts a condition with an error message.\n\n```compact\nassert balance >= amount \"Insufficient balance\";\n```"
            }
            "map" => {
                "**map**\n\nMaps a function over elements.\n\n```compact\nconst doubled = map(values, |x| x * 2);\n```"
            }
            "fold" => {
                "**fold**\n\nReduces elements to a single value.\n\n```compact\nconst sum = fold(values, 0, |acc, x| acc + x);\n```"
            }
            "disclose" => {
                "**disclose**\n\nDiscloses a private value publicly.\n\n```compact\ndisclose(secret_value);\n```"
            }
            "pure" => {
                "**pure**\n\nMarks a circuit as having no side effects.\n\n```compact\nexport pure circuit add(a: Field, b: Field): Field { ... }\n```"
            }
            "sealed" => {
                "**sealed**\n\nMarks a ledger state as immutable after initialization.\n\n```compact\nsealed ledger config: Config;\n```"
            }
            "true" => "**true**\n\nBoolean true literal.",
            "false" => "**false**\n\nBoolean false literal.",
            _ => return None,
        };
        Some(doc.to_string())
    }

    /// Get documentation for built-in types.
    fn builtin_type_docs(&self, text: &str) -> Option<String> {
        let doc = match text {
            "Boolean" => {
                "**Boolean**\n\nBoolean type with values `true` and `false`.\n\n```compact\nconst flag: Boolean = true;\n```"
            }
            "Field" => {
                "**Field**\n\nField element - the native arithmetic type for ZK circuits.\n\nSupports addition, subtraction, multiplication, and division.\n\n```compact\nconst x: Field = 42;\nconst y = x * 2 + 1;\n```"
            }
            "Uint" => {
                "**Uint<N>**\n\nUnsigned integer with `N` bits.\n\n```compact\nconst amount: Uint<64> = 1000;\nconst small: Uint<8> = 255;\n```"
            }
            "Bytes" => {
                "**Bytes<N>**\n\nFixed-size byte array with `N` bytes.\n\n```compact\nconst hash: Bytes<32> = ...;\nconst data: Bytes<64> = ...;\n```"
            }
            "Vector" => {
                "**Vector<N, T>**\n\nFixed-size array of `N` elements of type `T`.\n\n```compact\nconst values: Vector<10, Field> = ...;\nconst flags: Vector<8, Boolean> = ...;\n```"
            }
            "Opaque" => {
                "**Opaque<\"name\">**\n\nOpaque type wrapper for external data.\n\n```compact\nconst external: Opaque<\"commitment\"> = ...;\n```"
            }
            "Map" => {
                "**Map<K, V>**\n\nKey-value mapping for ledger state.\n\n```compact\nledger balances: Map<Bytes<32>, Uint<64>>;\n```"
            }
            "Set" => {
                "**Set<T>**\n\nSet collection for ledger state.\n\n```compact\nledger members: Set<Bytes<32>>;\n```"
            }
            "Counter" => {
                "**Counter**\n\nAtomic counter for ledger state.\n\n```compact\nledger nonce: Counter;\n```"
            }
            "Cell" => {
                "**Cell<T>**\n\nMutable cell for ledger state.\n\n```compact\nledger value: Cell<Field>;\n```"
            }
            "List" => {
                "**List<T>**\n\nOrdered list for ledger state. Supports prepend/pop from the front.\n\n```compact\nledger items: List<Field>;\n```"
            }
            "MerkleTree" => {
                "**MerkleTree<N, T>**\n\nMerkle tree for ledger state. Stores leaves and maintains a root hash.\n\n```compact\nledger tree: MerkleTree<32, Bytes<32>>;\n```"
            }
            "HistoricMerkleTree" => {
                "**HistoricMerkleTree<N, T>**\n\nHistoric Merkle tree with root history. Validates roots against any past root.\n\n```compact\nledger tree: HistoricMerkleTree<32, Bytes<32>>;\n```"
            }
            "Kernel" => {
                "**Kernel**\n\nBuilt-in kernel operations available in every contract as `kernel`.\n\nProvides access to contract identity, minting, block time checks, and transaction claims."
            }
            _ => return None,
        };
        Some(doc.to_string())
    }

    /// Get hover info for a definition node.
    fn definition_hover(&self, node: Node, source: &[u8]) -> Option<HoverInfo> {
        let kind = node.kind();

        // Extract any doc comments preceding the definition
        let doc_comment = self.extract_doc_comment(node, source);
        let doc_suffix = doc_comment
            .map(|doc| format!("\n\n---\n\n{}", doc))
            .unwrap_or_default();

        match kind {
            "cdefn" => {
                let signature = self.extract_circuit_signature(node, source)?;
                Some(HoverInfo {
                    content: format!(
                        "```compact\n{}\n```\n\nCircuit function{}",
                        signature, doc_suffix
                    ),
                    range: Some(self.node_range(node)),
                })
            }
            "wdecl" => {
                let signature = self.extract_witness_signature(node, source)?;
                Some(HoverInfo {
                    content: format!(
                        "```compact\n{}\n```\n\nWitness function{}",
                        signature, doc_suffix
                    ),
                    range: Some(self.node_range(node)),
                })
            }
            "ldecl" => {
                let name = self.get_field_text(node, "name", source)?;
                let type_text = self
                    .get_field_text(node, "type", source)
                    .unwrap_or_default();
                Some(HoverInfo {
                    content: format!(
                        "```compact\nledger {}: {}\n```\n\nLedger state{}",
                        name, type_text, doc_suffix
                    ),
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
                    content: format!(
                        "```compact\nstruct {}\n```\n\nStruct type{}{}",
                        name, fields_str, doc_suffix
                    ),
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
                    content: format!(
                        "```compact\nenum {}\n```\n\nEnum type{}{}",
                        name, variants_str, doc_suffix
                    ),
                    range: Some(self.node_range(node)),
                })
            }
            "ecdecl" => {
                let info = self.contract_type_info(node, source);
                let circuits = info
                    .circuits
                    .iter()
                    .map(|c| format!("- `{}`", c.signature))
                    .collect::<Vec<_>>()
                    .join("\n");
                let circuits_str = if circuits.is_empty() {
                    String::new()
                } else {
                    format!("\n\nCircuits:\n{}", circuits)
                };
                Some(HoverInfo {
                    content: format!(
                        "```compact\ncontract {} {{ ... }}\n```\n\nContract type{}{}",
                        info.name, circuits_str, doc_suffix
                    ),
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
        let return_type = self
            .get_field_text(node, "type", source)
            .unwrap_or_default();
        Some(format!("circuit {}({}): {}", name, params, return_type))
    }

    /// Extract witness function signature.
    fn extract_witness_signature(&self, node: Node, source: &[u8]) -> Option<String> {
        let name = self.get_field_text(node, "id", source)?;
        let params = self.extract_params(node, source);
        let return_type = self
            .get_field_text(node, "type", source)
            .unwrap_or_default();
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
    fn find_definition_node<'a>(
        &self,
        name: &str,
        node: Node<'a>,
        source: &[u8],
    ) -> Option<Node<'a>> {
        let kind = node.kind();

        // Check if this node is a definition with matching name
        let def_name = match kind {
            // Circuit definitions use "function_name" node
            "cdefn" | "wdecl" => self.get_function_name(node, source),
            "ldecl" | "struct" | "enumdef" | "mdefn" | "ecdecl" => {
                self.get_field_text(node, "name", source)
            }
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
    pub fn goto_definition(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<DefinitionLocation> {
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
                "cdefn" | "wdecl" => {
                    if self.get_field_text(parent, "id", source_bytes).as_deref() == Some(&text) {
                        // We're on the definition itself
                        return Some(DefinitionLocation {
                            range: self.node_range(parent),
                            selection_range: self.node_range(node),
                        });
                    }
                }
                "ldecl" | "struct" | "enumdef" | "mdefn" | "ecdecl"
                    if self.get_field_text(parent, "name", source_bytes).as_deref()
                        == Some(&text) =>
                {
                    // We're on the definition itself
                    return Some(DefinitionLocation {
                        range: self.node_range(parent),
                        selection_range: self.node_range(node),
                    });
                }
                _ => {}
            }
        }

        // Search for the definition
        let def_node = self.find_definition_node(&text, root, source_bytes)?;

        // Get the name node for selection range
        let name_range = self
            .get_definition_name_range(def_node, source_bytes)
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
            "cdefn" | "wdecl" => node.child_by_field_name("id"),
            "ldecl" | "struct" | "enumdef" | "mdefn" | "ecdecl" => node.child_by_field_name("name"),
            _ => None,
        }?;

        Some(self.node_range(name_node))
    }

    /// Get signature help for a function call at the given position.
    ///
    /// Returns signature information if the cursor is inside a function call.
    pub fn signature_help(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<SignatureInfo> {
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
        let active_param = self.count_active_parameter(call_node, point);

        // Find the function definition
        let def_node = self.find_definition_node(&func_name, root, source_bytes)?;

        // Build signature info
        self.build_signature_info(def_node, source_bytes, active_param)
    }

    /// Find an enclosing function call expression.
    fn find_enclosing_call<'a>(
        &self,
        node: Node<'a>,
        source: &[u8],
        cursor_point: tree_sitter::Point,
    ) -> Option<(Node<'a>, String)> {
        let mut current = Some(node);

        while let Some(n) = current {
            let kind = n.kind();

            // Check for function call patterns (Compact uses function_call_term)
            if kind == "function_call_term"
                && let Some(name) = self.get_call_function_name(n, source)
            {
                return Some((n, name));
            }

            // For blocks, search children for ERROR nodes with function calls
            // This handles incomplete code while typing
            if kind == "block"
                && let Some((error_node, name)) =
                    self.find_error_call_in_block(n, source, cursor_point)
            {
                return Some((error_node, name));
            }

            current = n.parent();
        }

        None
    }

    /// Search a block for ERROR nodes containing function calls before cursor.
    fn find_error_call_in_block<'a>(
        &self,
        block: Node<'a>,
        source: &[u8],
        cursor_point: tree_sitter::Point,
    ) -> Option<(Node<'a>, String)> {
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
    fn find_error_call_recursive<'a>(
        &self,
        node: Node<'a>,
        source: &[u8],
        cursor_point: tree_sitter::Point,
    ) -> Option<(Node<'a>, String)> {
        let kind = node.kind();

        if kind == "ERROR" {
            // Check if this ERROR has a function call and the cursor is after the opening paren
            if let Some(name) = self.get_call_function_name(node, source) {
                // Check if cursor is within or after this error node's range
                let start = node.start_position();
                let end = node.end_position();

                let after_start = cursor_point.row > start.row
                    || (cursor_point.row == start.row && cursor_point.column >= start.column);
                let before_end = cursor_point.row < end.row + 1
                    || (cursor_point.row == end.row && cursor_point.column <= end.column + 10);

                if after_start && before_end {
                    return Some((node, name));
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
    fn count_active_parameter(&self, call_node: Node, cursor_point: tree_sitter::Point) -> u32 {
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

        comma_count
    }

    /// Build SignatureInfo from a definition node.
    fn build_signature_info(
        &self,
        def_node: Node,
        source: &[u8],
        active_param: u32,
    ) -> Option<SignatureInfo> {
        let kind = def_node.kind();

        let (prefix, name, doc) = match kind {
            "cdefn" => {
                // cdefn uses "function_name" for the circuit name
                let name = self.get_function_name(def_node, source)?;
                ("circuit", name, "Circuit function")
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
        let label = format!(
            "{} {}({}): {}",
            prefix,
            name,
            params_str.join(", "),
            return_type
        );

        Some(SignatureInfo {
            label,
            documentation: Some(doc.to_string()),
            parameters: params,
            active_parameter: active_param,
        })
    }

    /// Get function name from a cdefn/wdecl node.
    fn get_function_name(&self, node: Node, source: &[u8]) -> Option<String> {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "function_name" {
                return Some(self.node_text(child, source));
            }
        }
        None
    }

    /// Get return type from a cdefn/wdecl node.
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
    fn collect_completion_symbols(
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
                format!(
                    "Syntax error: unexpected '{}'",
                    text.chars().take(30).collect::<String>()
                )
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
    fn collect_semantic_tokens(&self, node: Node, source: &[u8], tokens: &mut Vec<SemanticToken>) {
        match node.kind() {
            // Circuit/function definitions
            "cdefn" | "wdecl" => {
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
                };
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
                let enum_name = node
                    .child_by_field_name("name")
                    .map(|n| self.node_text(n, source));
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

            // Member access: `ref.transfer(...)` (method) or `info.value` (field).
            "member_access_expr" => {
                if let Some(member) = node.child_by_field_name("member") {
                    let is_call = node.child_by_field_name("arguments").is_some();
                    tokens.push(SemanticToken {
                        range: self.node_range(member),
                        token_type: if is_call {
                            SemanticTokenType::Function
                        } else {
                            SemanticTokenType::Property
                        },
                        modifiers: vec![],
                    });
                }
            }

            // New language keywords from grammar 0.3.0. The grammar models
            // these as anonymous literal children, so emit explicit keyword
            // tokens for them.
            "emit_term" => {
                self.push_keyword_token(node, "emit", tokens);
            }
            "contract_implements_declaration" => {
                self.push_keyword_token(node, "contract", tokens);
                self.push_keyword_token(node, "implements", tokens);
            }

            // Const bindings: `const <binding>, … ;` where each binding holds a
            // `pattern` field (multi-binding `const a = 1, b = 2;`). The crates.io
            // grammar names the binding `const_binding`; the old
            // `feat/grammar-v0.30.0` branch names it `cbinding`. There is no `let`.
            "const_stmt" => {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if matches!(child.kind(), "cbinding" | "const_binding")
                        && let Some(pattern) = child.child_by_field_name("pattern")
                    {
                        self.collect_binding_id_tokens(pattern, tokens);
                    }
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

    /// Push a `Keyword` semantic token for the direct child of `node` whose
    /// kind is `kind`. Used for the anonymous literal nodes of keywords.
    fn push_keyword_token(&self, node: Node, kind: &str, tokens: &mut Vec<SemanticToken>) {
        let mut cursor = node.walk();
        if let Some(child) = node.children(&mut cursor).find(|c| c.kind() == kind) {
            tokens.push(SemanticToken {
                range: self.node_range(child),
                token_type: SemanticTokenType::Keyword,
                modifiers: vec![],
            });
        }
    }

    /// Emit a readonly variable-declaration token for every `id` bound by a
    /// `pattern` node. Handles simple (`const x = ...`) and destructuring
    /// (`const [a, b] = ...`, `const { f } = ...`) bindings.
    fn collect_binding_id_tokens(&self, pattern: Node, tokens: &mut Vec<SemanticToken>) {
        if pattern.kind() == "id" {
            tokens.push(SemanticToken {
                range: self.node_range(pattern),
                token_type: SemanticTokenType::Variable,
                modifiers: vec![
                    SemanticTokenModifier::Declaration,
                    SemanticTokenModifier::Readonly,
                ],
            });
            return;
        }
        let mut cursor = pattern.walk();
        for child in pattern.children(&mut cursor) {
            self.collect_binding_id_tokens(child, tokens);
        }
    }

    /// Find all references to a symbol in source code.
    ///
    /// Returns both definition sites and usage sites.
    pub fn find_references(&mut self, source: &str, symbol_name: &str) -> Vec<ReferenceLocation> {
        let tree = match self.parse(source) {
            Some(t) => t,
            None => return vec![],
        };

        let mut refs = Vec::new();
        self.collect_references(tree.root_node(), source.as_bytes(), symbol_name, &mut refs);
        refs
    }

    /// Recursively collect references to a symbol from the AST.
    fn collect_references(
        &self,
        node: Node,
        source: &[u8],
        symbol_name: &str,
        refs: &mut Vec<ReferenceLocation>,
    ) {
        let kind = node.kind();

        // Check for definition sites
        match kind {
            "cdefn" | "wdecl" => {
                // Check function_name child for circuit/witness definitions
                if let Some(name_node) = node
                    .children(&mut node.walk())
                    .find(|n| n.kind() == "function_name")
                    && self.node_text(name_node, source) == symbol_name
                {
                    refs.push(ReferenceLocation {
                        range: self.node_range(name_node),
                        is_definition: true,
                    });
                };
            }
            "struct" | "enumdef" | "mdefn" => {
                if let Some(name) = node.child_by_field_name("name")
                    && self.node_text(name, source) == symbol_name
                {
                    refs.push(ReferenceLocation {
                        range: self.node_range(name),
                        is_definition: true,
                    });
                }
            }
            "ldecl" => {
                if let Some(name) = node.child_by_field_name("name")
                    && self.node_text(name, source) == symbol_name
                {
                    refs.push(ReferenceLocation {
                        range: self.node_range(name),
                        is_definition: true,
                    });
                }
            }
            _ => {}
        }

        // Check for usage sites
        match kind {
            // Function calls - look for the function name being called
            "function_call_term" => {
                if let Some(fun) = node.child_by_field_name("fun") {
                    // The fun field contains the function being called
                    // It could be an id directly or a more complex expression
                    self.check_function_call_name(fun, source, symbol_name, refs);
                }
            }
            // Type references (struct/enum usage in type positions)
            "tref" => {
                if let Some(id) = node.child_by_field_name("id")
                    && self.node_text(id, source) == symbol_name
                {
                    refs.push(ReferenceLocation {
                        range: self.node_range(id),
                        is_definition: false,
                    });
                }
            }
            _ => {}
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_references(child, source, symbol_name, refs);
        }
    }

    /// Check if a function call's target matches the symbol name.
    fn check_function_call_name(
        &self,
        fun_node: Node,
        source: &[u8],
        symbol_name: &str,
        refs: &mut Vec<ReferenceLocation>,
    ) {
        // The function being called could be:
        // 1. A simple identifier: add(...)
        // 2. A qualified name: Module.add(...) (though Compact uses prefix style)
        // For now, check if the entire text matches or if it's a simple id
        let text = self.node_text(fun_node, source);
        if text == symbol_name {
            refs.push(ReferenceLocation {
                range: self.node_range(fun_node),
                is_definition: false,
            });
        }
    }

    /// Recursively collect import statements from the AST.
    fn collect_imports(&self, node: Node, source: &[u8], imports: &mut Vec<ImportInfo>) {
        if node.kind() == "idecl"
            && let Some(import_info) = self.extract_import(node, source)
        {
            imports.push(import_info);
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

    /// Get member access context at a cursor position.
    ///
    /// If the cursor is on `increment` in `round.increment(1)`, returns
    /// `MemberAccessContext { base_name: "round", member_name: "increment", ... }`.
    pub fn get_member_access_context(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<MemberAccessContext> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();

        let point = tree_sitter::Point {
            row: line as usize,
            column: character as usize,
        };

        // Find the deepest node at this position
        let node = root.descendant_for_point_range(point, point)?;

        // The node should be an identifier
        if node.kind() != "id" {
            return None;
        }

        // Its parent should be a member_access_expr
        let parent = node.parent()?;
        if parent.kind() != "member_access_expr" {
            return None;
        }

        // Verify this node is the member field (not the base)
        let member_node = parent.child_by_field_name("member")?;
        if member_node.id() != node.id() {
            return None;
        }

        // Extract the base expression (`w.token`, `make()`, `arr[i]`, …).
        let base_node = parent.child_by_field_name("base")?;
        let base = self.member_base(base_node, source_bytes);
        if base == MemberBase::Other {
            return None;
        }
        let base_name = Self::member_base_display(&base);
        let member_name = self.node_text(node, source_bytes);
        let member_range = self.node_range(node);

        Some(MemberAccessContext {
            base_name,
            base,
            member_name,
            member_range,
        })
    }

    /// Extract the identifier name from a base expression node.
    ///
    /// For simple identifiers this returns the text directly.
    /// For more complex expressions, tries to find an `id` child.
    /// Recursively collect member accesses named `member_name`.
    fn collect_member_accesses_rec(
        &self,
        node: Node,
        source: &[u8],
        member_name: &str,
        out: &mut Vec<MemberAccessContext>,
    ) {
        if node.kind() == "member_access_expr"
            && let Some(member) = node.child_by_field_name("member")
            && self.node_text(member, source) == member_name
            && let Some(base) = node.child_by_field_name("base")
        {
            let member_base = self.member_base(base, source);
            if member_base != MemberBase::Other {
                out.push(MemberAccessContext {
                    base_name: Self::member_base_display(&member_base),
                    base: member_base,
                    member_name: member_name.to_string(),
                    member_range: self.node_range(member),
                });
            }
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            self.collect_member_accesses_rec(child, source, member_name, out);
        }
    }

    /// Model a member-access base expression (`w.token`, `make()`, `arr[i]`).
    fn member_base(&self, node: Node, source: &[u8]) -> MemberBase {
        match node.kind() {
            "id" => MemberBase::Path(vec![self.node_text(node, source)]),
            "member_access_expr" => {
                let base = node
                    .child_by_field_name("base")
                    .map(|base| self.member_base(base, source));
                let member = node
                    .child_by_field_name("member")
                    .map(|member| self.node_text(member, source));
                // A `member_access_expr` with `arguments` is a method call
                // (`factory.create()`, `map.lookup(k)`), not a field access.
                let is_call = node.child_by_field_name("arguments").is_some();
                match (base, member) {
                    (Some(base), Some(member)) => {
                        if is_call {
                            MemberBase::MethodCall(Box::new(base), member)
                        } else {
                            match base {
                                MemberBase::Path(mut path) => {
                                    path.push(member);
                                    MemberBase::Path(path)
                                }
                                MemberBase::Other => MemberBase::Other,
                                other => MemberBase::Access(Box::new(other), member),
                            }
                        }
                    }
                    _ => MemberBase::Other,
                }
            }
            "function_call_term" => {
                let callee = node
                    .child_by_field_name("fun")
                    .and_then(|fun| fun.child_by_field_name("id"))
                    .map(|id| self.node_text(id, source));
                match callee {
                    Some(callee) => {
                        let mut args = Vec::new();
                        let mut cursor = node.walk();
                        for arg in node.children_by_field_name("expr", &mut cursor) {
                            args.push(self.member_base(arg, source));
                        }
                        MemberBase::Call { callee, args }
                    }
                    None => MemberBase::Other,
                }
            }
            "index_access_expr" => match node.child_by_field_name("base") {
                Some(base) => MemberBase::Index(Box::new(self.member_base(base, source))),
                None => MemberBase::Other,
            },
            "term" | "expr" | "expr_seq" | "expr_seq_term" => {
                let mut cursor = node.walk();
                node.named_children(&mut cursor)
                    .next()
                    .map(|child| self.member_base(child, source))
                    .unwrap_or(MemberBase::Other)
            }
            _ => MemberBase::Other,
        }
    }

    /// Human-readable rendering of a member base, for hover/definition text.
    fn member_base_display(base: &MemberBase) -> String {
        match base {
            MemberBase::Path(path) => path.join("."),
            MemberBase::Call { callee, .. } => format!("{}()", callee),
            MemberBase::MethodCall(receiver, method) => {
                format!("{}.{}()", Self::member_base_display(receiver), method)
            }
            MemberBase::Access(receiver, field) => {
                format!("{}.{}", Self::member_base_display(receiver), field)
            }
            MemberBase::Index(inner) => format!("{}[]", Self::member_base_display(inner)),
            MemberBase::Other => String::new(),
        }
    }

    /// Whether a `struct` named `name` is declared in this file.
    pub fn struct_exists(&mut self, source: &str, name: &str) -> bool {
        let Some(tree) = self.parse(source) else {
            return false;
        };
        let root = tree.root_node();
        let resolved = self.resolve_type_name(root, source.as_bytes(), name);
        self.find_struct(root, source.as_bytes(), &resolved)
            .is_some()
    }

    /// Declared type of a `struct` field (`struct S { field: T }`), following
    /// same-file aliases for the struct name.
    pub fn get_struct_field_type(
        &mut self,
        source: &str,
        struct_name: &str,
        field_name: &str,
    ) -> Option<String> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let resolved = self.resolve_type_name(root, source_bytes, struct_name);
        let struct_node = self.find_struct(root, source_bytes, &resolved)?;
        let mut cursor = struct_node.walk();
        for child in struct_node.children(&mut cursor) {
            if child.kind() == "arg"
                && self.get_field_text(child, "id", source_bytes).as_deref() == Some(field_name)
            {
                return self.get_field_text(child, "type", source_bytes);
            }
        }
        None
    }

    /// Declared generic parameters of a `struct` (`struct Box<T>` → `"<T>"`),
    /// or `None` when the struct is not generic.
    pub fn get_struct_type_params(&mut self, source: &str, struct_name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let resolved = self.resolve_type_name(root, source_bytes, struct_name);
        let struct_node = self.find_struct(root, source_bytes, &resolved)?;
        self.get_field_text(struct_node, "gparams", source_bytes)
    }

    /// Recursively find a `struct` named `name`.
    fn find_struct<'a>(&self, node: Node<'a>, source: &[u8], name: &str) -> Option<Node<'a>> {
        if node.kind() == "struct"
            && self.get_field_text(node, "name", source).as_deref() == Some(name)
        {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_struct(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// Look up the type of a variable by name.
    ///
    /// Returns the declared type annotation when present, otherwise a type
    /// inferred from a `const x = f();` initializer call. Same-file `type`
    /// aliases are followed to their target, so a contract reference is
    /// reachable even when it is written through an alias.
    pub fn get_variable_type(&mut self, source: &str, variable_name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let raw = self.find_variable_type(root, root, source_bytes, variable_name)?;
        Some(self.resolve_type_name(root, source_bytes, &raw))
    }

    /// Like [`Self::get_variable_type`], but returns the declared type text
    /// unchanged (generics preserved) — needed for element-type extraction.
    pub fn get_variable_type_raw(&mut self, source: &str, variable_name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        self.find_variable_type(root, root, source.as_bytes(), variable_name)
    }

    /// Resolve a written type name through same-file `type` aliases.
    pub fn resolve_alias(&mut self, source: &str, type_name: &str) -> String {
        match self.parse(source) {
            Some(tree) => self.resolve_type_name(tree.root_node(), source.as_bytes(), type_name),
            None => type_name.to_string(),
        }
    }

    /// Declared return type of a circuit/witness/contract-circuit named `name`.
    pub fn get_callable_return_type(&mut self, source: &str, name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        self.find_callable_return_type(tree.root_node(), source.as_bytes(), name)
    }

    /// Declared signature (parameter types, generic params, return type) of a
    /// circuit or witness by name (`circuit id<T>(x: T): T`).
    pub fn get_callable_signature(
        &mut self,
        source: &str,
        name: &str,
    ) -> Option<CallableSignature> {
        let tree = self.parse(source)?;
        self.find_callable_signature(tree.root_node(), source.as_bytes(), name)
    }

    fn find_callable_signature(
        &self,
        node: Node,
        source: &[u8],
        name: &str,
    ) -> Option<CallableSignature> {
        if matches!(node.kind(), "cdefn" | "wdecl" | "ecdecl_circuit")
            && self.get_field_text(node, "id", source).as_deref() == Some(name)
        {
            let mut param_types = Vec::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if matches!(child.kind(), "parg" | "arg")
                    && let Some(ty) = self.get_field_text(child, "type", source)
                {
                    param_types.push(ty);
                }
            }
            return Some(CallableSignature {
                param_types,
                generic_params: self
                    .get_field_text(node, "gparams", source)
                    .unwrap_or_default(),
                return_type: self.get_field_text(node, "type", source),
            });
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_callable_signature(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// For an unannotated `const x = f();` binding, return the callee name `f`.
    /// Lets the caller infer `x`'s type when `f` is defined in another file.
    pub fn get_initializer_callee(&mut self, source: &str, variable_name: &str) -> Option<String> {
        let tree = self.parse(source)?;
        self.find_initializer_callee(tree.root_node(), source.as_bytes(), variable_name)
    }

    /// All member accesses of `member_name` in the file, with their base names.
    /// Used to gate Find References / Rename by the base's contract type.
    pub fn collect_member_accesses(
        &mut self,
        source: &str,
        member_name: &str,
    ) -> Vec<MemberAccessContext> {
        let Some(tree) = self.parse(source) else {
            return Vec::new();
        };
        let mut out = Vec::new();
        self.collect_member_accesses_rec(
            tree.root_node(),
            source.as_bytes(),
            member_name,
            &mut out,
        );
        out
    }

    /// If the position is on a circuit declared by a contract type
    /// (`ecdecl_circuit` inside `ecdecl`), return `(contract_name, circuit_name)`.
    pub fn circuit_at_position(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<(String, String)> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let source_bytes = source.as_bytes();
        let point = tree_sitter::Point {
            row: line as usize,
            column: character as usize,
        };
        let node = root.descendant_for_point_range(point, point)?;
        let parent = node.parent()?;
        if parent.kind() != "ecdecl_circuit" {
            return None;
        }
        // Ensure the cursor is on the circuit's name, not a type in its signature.
        let id_node = parent.child_by_field_name("id")?;
        if id_node.start_byte() != node.start_byte() || id_node.end_byte() != node.end_byte() {
            return None;
        }
        let circuit = self.node_text(id_node, source_bytes);
        let mut ancestor = parent.parent();
        while let Some(current) = ancestor {
            if current.kind() == "ecdecl" {
                let contract = self.get_field_text(current, "name", source_bytes)?;
                return Some((contract, circuit));
            }
            ancestor = current.parent();
        }
        None
    }

    /// Recursively search the AST for a declaration of `variable_name` and
    /// return its type (annotation, or inferred from the initializer).
    ///
    /// Handles ledger fields (`ldecl`), circuit/witness parameters (`parg`), and
    /// typed/`const` bindings (`const x: T = ...` / `const x = f()`). A contract
    /// reference is typically a `parg` or an `ldecl`, so this powers
    /// `ref.circuit(...)` member completion.
    fn find_variable_type(
        &self,
        root: Node,
        node: Node,
        source: &[u8],
        variable_name: &str,
    ) -> Option<String> {
        match node.kind() {
            "ldecl" => {
                if self.get_field_text(node, "name", source).as_deref() == Some(variable_name) {
                    return self.get_field_text(node, "type", source);
                }
            }
            "parg" | "const_binding" | "cbinding" => {
                if let Some(pattern) = node.child_by_field_name("pattern")
                    && self.pattern_binds(pattern, source, variable_name)
                {
                    if let Some(ty) = self.get_field_text(node, "type", source) {
                        return Some(ty);
                    }
                    // `const t = f();` with no annotation: use `f`'s return type.
                    if let Some(value) = node.child_by_field_name("value")
                        && let Some(ty) = self.infer_call_return_type(root, source, value)
                    {
                        return Some(ty);
                    }
                }
            }
            _ => {}
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(result) = self.find_variable_type(root, child, source, variable_name) {
                return Some(result);
            }
        }

        None
    }

    /// Follow same-file `type Name = Target;` aliases (chained) to the final
    /// head type name.
    fn resolve_type_name(&self, root: Node, source: &[u8], type_text: &str) -> String {
        let mut current = Self::head_type_name(type_text);
        let mut seen = std::collections::HashSet::new();
        // Bounded by `seen` (cycles) and a hard cap as a safety net.
        while seen.insert(current.clone()) && seen.len() <= 32 {
            match self.find_type_alias(root, source, &current) {
                Some(target) => current = Self::head_type_name(&target),
                None => break,
            }
        }
        current
    }

    /// The head identifier of a type expression (`Foo<Bar>` -> `Foo`).
    fn head_type_name(type_text: &str) -> String {
        type_text
            .split('<')
            .next()
            .unwrap_or(type_text)
            .trim()
            .to_string()
    }

    /// Find a non-generic `type_alias_declaration` named `name` and return its
    /// target type text. Generic aliases are skipped (we cannot substitute args).
    fn find_type_alias(&self, node: Node, source: &[u8], name: &str) -> Option<String> {
        if node.kind() == "type_alias_declaration"
            && node.child_by_field_name("gparams").is_none()
            && self.get_field_text(node, "name", source).as_deref() == Some(name)
        {
            return self.get_field_text(node, "type", source);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_type_alias(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// If `value` is (a parenthesized) call to a circuit declared in this file,
    /// return that circuit's declared return type.
    fn infer_call_return_type(&self, root: Node, source: &[u8], value: Node) -> Option<String> {
        let call = self.find_call_term(value)?;
        let fun = call.child_by_field_name("fun")?;
        let callee = fun.child_by_field_name("id")?;
        let callee_name = self.node_text(callee, source);
        self.find_callable_return_type(root, source, &callee_name)
    }

    /// Descend through transparent expression wrappers to the first call term.
    fn find_call_term<'a>(&self, node: Node<'a>) -> Option<Node<'a>> {
        match node.kind() {
            "function_call_term" => return Some(node),
            "expr" | "term" | "expr_seq" | "expr_seq_term" => {}
            _ => return None,
        }
        let mut cursor = node.walk();
        node.named_children(&mut cursor)
            .find_map(|child| self.find_call_term(child))
    }

    /// Find a circuit/witness/contract-circuit definition named `name` and
    /// return its declared return type.
    fn find_callable_return_type(&self, node: Node, source: &[u8], name: &str) -> Option<String> {
        if matches!(node.kind(), "cdefn" | "wdecl" | "ecdecl_circuit")
            && self.get_field_text(node, "id", source).as_deref() == Some(name)
        {
            return self.get_field_text(node, "type", source);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_callable_return_type(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// Find an unannotated `const x = f();` binding of `variable_name` and
    /// return the callee name `f`.
    fn find_initializer_callee(
        &self,
        node: Node,
        source: &[u8],
        variable_name: &str,
    ) -> Option<String> {
        if matches!(node.kind(), "const_binding" | "cbinding")
            && node.child_by_field_name("type").is_none()
            && let Some(pattern) = node.child_by_field_name("pattern")
            && self.pattern_binds(pattern, source, variable_name)
            && let Some(value) = node.child_by_field_name("value")
            && let Some(call) = self.find_call_term(value)
            && let Some(fun) = call.child_by_field_name("fun")
            && let Some(id) = fun.child_by_field_name("id")
        {
            return Some(self.node_text(id, source));
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_initializer_callee(child, source, variable_name) {
                return Some(found);
            }
        }
        None
    }

    /// Whether a `pattern` node binds `name` (simple or destructuring).
    fn pattern_binds(&self, pattern: Node, source: &[u8], name: &str) -> bool {
        if let Some(id) = pattern.child_by_field_name("id") {
            return self.node_text(id, source) == name;
        }
        let mut cursor = pattern.walk();
        pattern
            .children(&mut cursor)
            .any(|child| self.pattern_binds(child, source, name))
    }

    /// Find a contract type (`contract Name { … }`) by name and return its
    /// declared circuits. Used for cross-contract-call intelligence.
    pub fn contract_type(&mut self, source: &str, name: &str) -> Option<ContractTypeInfo> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let ecdecl = self.find_ecdecl(root, source.as_bytes(), name)?;
        Some(self.contract_type_info(ecdecl, source.as_bytes()))
    }

    /// Recursively find an `ecdecl` with the given name.
    fn find_ecdecl<'a>(&self, node: Node<'a>, source: &[u8], name: &str) -> Option<Node<'a>> {
        if node.kind() == "ecdecl"
            && self.get_field_text(node, "name", source).as_deref() == Some(name)
        {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_ecdecl(child, source, name) {
                return Some(found);
            }
        }
        None
    }

    /// Build the info for an `ecdecl` node, including its declared circuits.
    fn contract_type_info(&self, node: Node, source: &[u8]) -> ContractTypeInfo {
        let name = self
            .get_field_text(node, "name", source)
            .unwrap_or_default();
        let mut circuits = Vec::new();
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() != "ecdecl_circuit" {
                continue;
            }
            let Some(circuit_name) = self.get_field_text(child, "id", source) else {
                continue;
            };
            let params = self.extract_params(child, source);
            let return_type = self
                .get_field_text(child, "type", source)
                .unwrap_or_default();
            let detail = format!("({}): {}", params, return_type);
            let signature = format!("circuit {}{}", circuit_name, detail);
            let documentation = self
                .extract_doc_comment(child, source)
                .map(|doc| format!("---\n\n{}", doc));
            let name_location = child
                .child_by_field_name("id")
                .map(|id| self.node_to_symbol_location(id))
                .unwrap_or_else(|| self.node_to_symbol_location(child));
            circuits.push(ContractCircuit {
                name: circuit_name,
                detail,
                signature,
                documentation,
                location: self.node_to_symbol_location(child),
                name_location,
            });
        }
        ContractTypeInfo {
            name,
            circuits,
            location: self.node_to_symbol_location(node),
        }
    }

    /// Like [`Self::get_member_access_context`], but works anywhere inside the
    /// call's argument list. Used by signature help for `reference.circuit(...)`.
    pub fn get_enclosing_member_call(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<MemberAccessContext> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let point = tree_sitter::Point {
            row: line as usize,
            column: character as usize,
        };
        let mut node = root.descendant_for_point_range(point, point)?;
        loop {
            if node.kind() == "member_access_expr"
                && node.child_by_field_name("arguments").is_some()
            {
                let member_node = node.child_by_field_name("member")?;
                let base_node = node.child_by_field_name("base")?;
                let base = self.member_base(base_node, source.as_bytes());
                if base == MemberBase::Other {
                    return None;
                }
                let base_name = Self::member_base_display(&base);
                let member_name = self.node_text(member_node, source.as_bytes());
                return Some(MemberAccessContext {
                    base_name,
                    base,
                    member_name,
                    member_range: self.node_range(member_node),
                });
            }
            node = node.parent()?;
        }
    }
}

/// Check if a type name is a Compact built-in type.
pub fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "Field"
            | "Boolean"
            | "Uint"
            | "Bytes"
            | "Vector"
            | "Opaque"
            | "Counter"
            | "Map"
            | "Set"
            | "Cell"
            | "List"
            | "MerkleTree"
            | "HistoricMerkleTree"
            | "Kernel"
            | "ContractAddress"
            | "UserAddress"
            | "ShieldedCoinInfo"
            | "QualifiedShieldedCoinInfo"
            | "ShieldedSendResult"
            | "MerkleTreeDigest"
            | "Maybe"
            | "Either"
            | "JubjubPoint"
            | "PublicAddress"
            | "Curve25519Base"
            | "Curve25519Scalar"
            | "Curve25519Point"
            | "Secp256r1Base"
            | "Secp256r1Scalar"
            | "Secp256r1Point"
            | "MerkleTreePathEntry"
            | "MerkleTreePath"
            | "ZswapCoinPublicKey"
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
        assert!(
            info.label.contains("add"),
            "Label should contain function name"
        );
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
        assert_eq!(
            info.active_parameter, 1,
            "Second parameter should be active"
        );
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
        assert!(
            info.is_some(),
            "Should find signature help for incomplete call"
        );
        let info = info.unwrap();
        assert!(
            info.label.contains("add"),
            "Label should contain function name"
        );
    }

    #[test]
    fn test_signature_help_third_param() {
        let mut parser = ParserEngine::new();
        let source = r#"circuit add3(a: Field, b: Field, c: Field): Field {
    return a + b + c;
}

circuit main(): Field {
    return add3(1, 2, 3);
}"#;
        // Position after the second comma (line 5, col 21)
        let info = parser.signature_help(source, 5, 21);
        assert!(info.is_some(), "Should find signature help");
        let info = info.unwrap();
        assert_eq!(info.parameters.len(), 3, "Should have 3 parameters");
        assert_eq!(info.active_parameter, 2, "Third parameter should be active");
    }

    #[test]
    fn test_const_binding_semantic_token() {
        let mut parser = ParserEngine::new();
        let source = r#"circuit f(): Field {
    const x = 1;
    return x;
}"#;
        let tokens = parser.get_semantic_tokens(source);

        // The `const` binding `x` must be reported as a readonly variable
        // declaration. The grammar node is `const_stmt` (not `const_binding`),
        // so this regresses if the match arm references the wrong node kind.
        let var_decl = tokens.iter().find(|t| {
            t.token_type == SemanticTokenType::Variable
                && t.modifiers.contains(&SemanticTokenModifier::Declaration)
                && t.modifiers.contains(&SemanticTokenModifier::Readonly)
        });
        assert!(
            var_decl.is_some(),
            "const binding should produce a readonly Variable declaration token"
        );
    }

    #[test]
    fn test_circuit_hover_includes_return_type() {
        let mut parser = ParserEngine::new();
        let source = "circuit add(a: Field, b: Field): Field { return a + b; }";
        // Hover on "add" at position (0, 8).
        let info = parser.hover_info(source, 0, 8).expect("hover on circuit");
        // The return type comes from the `type` field; if the code reads a
        // non-existent `rtype` field the signature ends with ": " (empty).
        assert!(
            info.content.contains("): Field"),
            "circuit hover should include the return type, got: {}",
            info.content
        );
    }

    #[test]
    fn test_parses_language_0_23_syntax() {
        let mut parser = ParserEngine::new();
        // Constructs that only the 0.23-capable grammar parses: block comment,
        // selective import, `new type` alias, single-quoted Opaque tag,
        // multi-binding const with hex/octal literals, and slice<>.
        let source = r#"/* block comment */
import { Maybe } from CompactStandardLibrary;
export new type Tag = Opaque<'mytag'>;
export circuit demo(data: Bytes<32>): Bytes<16> {
    const a = 0xFF, b = 0o17;
    return slice<16>(data, 0);
}"#;
        let errors = parser.get_syntax_errors(source);
        assert!(
            errors.is_empty(),
            "0.23 syntax should parse without errors, got: {:?}",
            errors
        );
        // The circuit is still discoverable as a symbol.
        let symbols = parser.get_completion_symbols(source);
        assert!(
            symbols
                .iter()
                .any(|s| s.name == "demo" && s.kind == CompletionSymbolKind::Function)
        );
        // The `new type` alias appears in the document-symbol outline.
        let doc_symbols = parser.document_symbols(source);
        assert!(
            doc_symbols.iter().any(|s| s.name == "Tag"),
            "type alias should appear in the outline"
        );
    }

    /// Validation step for grammar bumps: every node kind the parser compares
    /// with `.kind()` (and every `child_by_field_name`) must exist in the linked
    /// `tree-sitter-compact` schema. Fails loudly if a grammar rename/removal
    /// slips in, instead of silently degrading editor features.
    ///
    /// A few **legacy** node kinds are intentionally kept so the parser works
    /// with both the old and new grammar schemes; they are listed below.
    #[test]
    fn test_parser_references_exist_in_grammar_schema() {
        use std::collections::HashSet;

        // Names kept for the older grammar scheme; not in the current schema.
        const LEGACY_NODE_KINDS: &[&str] = &["cbinding", "type_decl"];

        let schema: serde_json::Value =
            serde_json::from_str(tree_sitter_compact::NODE_TYPES).expect("valid node-types.json");
        let nodes = schema.as_array().expect("node-types.json is an array");

        let named: HashSet<&str> = nodes
            .iter()
            .filter_map(|n| n.get("type").and_then(|t| t.as_str()))
            .collect();
        let mut fields: HashSet<&str> = HashSet::new();
        for n in nodes {
            if let Some(obj) = n.get("fields").and_then(|f| f.as_object()) {
                fields.extend(obj.keys().map(String::as_str));
            }
        }

        let src = include_str!("mod.rs");

        // Node kinds compared with `.kind() ==` or `matches!(... .kind(), …)`.
        let re_eq = regex::Regex::new(r#"\.kind\(\)\s*==\s*"([a-z_][a-z0-9_]*)""#).unwrap();
        let re_matches = regex::Regex::new(r"(?s)matches!\([^;]*?\.kind\(\),\s*([^)]*)\)").unwrap();
        let re_quoted = regex::Regex::new(r#""([a-z_][a-z0-9_]*)""#).unwrap();

        let mut kinds: HashSet<String> =
            re_eq.captures_iter(src).map(|c| c[1].to_string()).collect();
        for cap in re_matches.captures_iter(src) {
            kinds.extend(re_quoted.captures_iter(&cap[1]).map(|c| c[1].to_string()));
        }

        let missing_kinds: Vec<&String> = kinds
            .iter()
            .filter(|k| !named.contains(k.as_str()) && !LEGACY_NODE_KINDS.contains(&k.as_str()))
            .collect();
        assert!(
            missing_kinds.is_empty(),
            "parser references node kinds absent from the grammar schema: {missing_kinds:?}"
        );

        // Field names read via `child_by_field_name(...)`.
        let re_field = regex::Regex::new(r#"child_by_field_name\("([a-z_][a-z0-9_]*)"\)"#).unwrap();
        let field_refs: HashSet<String> = re_field
            .captures_iter(src)
            .map(|c| c[1].to_string())
            .collect();
        let missing_fields: Vec<&String> = field_refs
            .iter()
            .filter(|f| !fields.contains(f.as_str()))
            .collect();
        assert!(
            missing_fields.is_empty(),
            "parser references fields absent from the grammar schema: {missing_fields:?}"
        );
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
        assert!(
            symbols
                .iter()
                .any(|s| s.name == "add" && s.kind == CompletionSymbolKind::Function)
        );
        assert!(
            symbols
                .iter()
                .any(|s| s.name == "Point" && s.kind == CompletionSymbolKind::Struct)
        );
        assert!(
            symbols
                .iter()
                .any(|s| s.name == "Color" && s.kind == CompletionSymbolKind::Enum)
        );
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
        assert!(
            stdlib.is_some(),
            "Should find CompactStandardLibrary import"
        );
        let stdlib = stdlib.unwrap();
        assert!(!stdlib.is_file, "Should not be a file import");
        assert!(stdlib.prefix.is_none(), "Should have no prefix");

        // Utils import with prefix
        let utils = imports.iter().find(|i| i.path == "../utils/Utils");
        assert!(utils.is_some(), "Should find Utils import");
        let utils = utils.unwrap();
        assert!(utils.is_file, "Should be a file import");
        assert_eq!(
            utils.prefix.as_deref(),
            Some("Utils_"),
            "Should have Utils_ prefix"
        );

        // Initializable import with prefix
        let init = imports
            .iter()
            .find(|i| i.path == "../security/Initializable");
        assert!(init.is_some(), "Should find Initializable import");
        let init = init.unwrap();
        assert!(init.is_file, "Should be a file import");
        assert_eq!(
            init.prefix.as_deref(),
            Some("Init_"),
            "Should have Init_ prefix"
        );

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
        assert!(
            add_symbol.is_some(),
            "Should find 'add' circuit inside module. Found: {:?}",
            symbols
        );

        let add_symbol = add_symbol.unwrap();
        assert_eq!(
            add_symbol.kind,
            CompletionSymbolKind::Function,
            "Should be a Function"
        );
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
        assert!(
            !errors.is_empty(),
            "Missing brace should produce syntax error"
        );
    }

    #[test]
    fn test_syntax_errors_unexpected_token() {
        let mut parser = ParserEngine::new();
        // Invalid syntax - unexpected token
        let source = r#"circuit !!!invalid(): Field { return 1; }"#;
        let errors = parser.get_syntax_errors(source);
        assert!(
            !errors.is_empty(),
            "Invalid identifier should produce syntax error"
        );
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
            println!(
                "  {}: {} at line {}",
                i + 1,
                err.message,
                err.range.start.line + 1
            );
        }
        assert!(
            errors.len() >= 2,
            "Should find multiple syntax errors, found {}",
            errors.len()
        );
    }

    #[test]
    fn test_contract_type_extraction() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
    circuit balanceOf(owner: Address): Uint<64>;
}
"#;
        let info = parser
            .contract_type(source, "Token")
            .expect("contract type");
        assert_eq!(info.name, "Token");
        assert_eq!(info.circuits.len(), 2);
        let transfer = &info.circuits[0];
        assert_eq!(transfer.name, "transfer");
        assert_eq!(transfer.detail, "(to: Address, amount: Uint<64>): Boolean");
        assert_eq!(
            transfer.signature,
            "circuit transfer(to: Address, amount: Uint<64>): Boolean"
        );
        assert!(parser.contract_type(source, "Missing").is_none());
    }

    #[test]
    fn test_variable_type_resolves_contract_references() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

circuit useParam(ref: Token): Boolean {
    return ref.transfer(0);
}

circuit useConst(): Boolean {
    const ref: Token = default<Token>;
    return ref.transfer(0);
}

ledger stored: Token;

circuit useLedger(): Boolean {
    return stored.transfer(0);
}
"#;
        assert_eq!(
            parser.get_variable_type(source, "ref").as_deref(),
            Some("Token")
        );
        assert_eq!(
            parser.get_variable_type(source, "stored").as_deref(),
            Some("Token")
        );
        assert!(parser.get_variable_type(source, "nope").is_none());
    }

    #[test]
    fn test_variable_type_follows_aliases() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

type MyToken = Token;
type Chain = MyToken;

circuit viaAlias(ref: MyToken): Boolean {
    return ref.transfer(0);
}

const stored: Chain = default<Chain>;

circuit viaChainedAlias(): Boolean {
    return stored.transfer(0);
}
"#;
        assert_eq!(
            parser.get_variable_type(source, "ref").as_deref(),
            Some("Token")
        );
        assert_eq!(
            parser.get_variable_type(source, "stored").as_deref(),
            Some("Token")
        );
    }

    #[test]
    fn test_variable_type_inferred_from_call_return() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

circuit make(): Token {
    return ref;
}

circuit useInferred(): Boolean {
    const t = make();
    const u = (make());
    return t.transfer(0);
}
"#;
        assert_eq!(
            parser.get_variable_type(source, "t").as_deref(),
            Some("Token")
        );
        assert_eq!(
            parser.get_variable_type(source, "u").as_deref(),
            Some("Token")
        );
    }

    #[test]
    fn test_variable_type_annotation_wins_over_inference() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}
contract Other {
    circuit nop(): Boolean;
}

type MyToken = Token;

circuit make(): Other {
    return other;
}

circuit useAnnotated(): Boolean {
    const t: MyToken = make();
    return t.transfer(0);
}
"#;
        // The annotation `MyToken` is resolved through the alias to `Token`,
        // not the callee's (unrelated) return type `Other`.
        assert_eq!(
            parser.get_variable_type(source, "t").as_deref(),
            Some("Token")
        );
    }

    #[test]
    fn test_variable_type_no_inference_for_non_call() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit f(a: Uint<64>, b: Uint<64>): Uint<64> {
    return a;
}

circuit g(): Uint<64> {
    const x = f(1, 2);
    const y = 1 + 2;
    return x;
}
"#;
        // A call to a circuit whose return type is a builtin resolves to its
        // head type name (matching `extract_base_type`)...
        assert_eq!(
            parser.get_variable_type(source, "x").as_deref(),
            Some("Uint")
        );
        // ...but a non-call initializer yields nothing.
        assert!(parser.get_variable_type(source, "y").is_none());
    }

    #[test]
    fn test_variable_type_alias_cycle_terminates() {
        let mut parser = ParserEngine::new();
        let source = r#"
type A = B;
type B = A;

circuit f(x: A): Boolean {
    return x;
}
"#;
        // A cyclic alias chain must terminate rather than loop forever.
        assert!(parser.get_variable_type(source, "x").is_some());
    }

    #[test]
    fn test_variable_type_generic_alias_skipped() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

type Box<T> = Token;

circuit f(ref: Box<Uint<64>>): Boolean {
    return ref.transfer(0);
}
"#;
        // Generic aliases are not substituted; the head name is returned as-is.
        assert_eq!(
            parser.get_variable_type(source, "ref").as_deref(),
            Some("Box")
        );
    }

    #[test]
    fn test_variable_type_inferred_from_witness_return() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

witness make(): Token;

circuit f(): Boolean {
    const t = make();
    return t.transfer(0);
}
"#;
        assert_eq!(
            parser.get_variable_type(source, "t").as_deref(),
            Some("Token")
        );
    }

    #[test]
    fn test_get_callable_return_type_and_initializer_callee() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

circuit make(): Token {
    return ref;
}

circuit useIt(): Boolean {
    const t = make();
    const annotated: Token = make();
    return t.transfer(0);
}
"#;
        assert_eq!(
            parser.get_callable_return_type(source, "make").as_deref(),
            Some("Token")
        );
        assert_eq!(
            parser.get_initializer_callee(source, "t").as_deref(),
            Some("make")
        );
        // Annotated bindings are not unannotated initializer callees.
        assert!(parser.get_initializer_callee(source, "annotated").is_none());
    }

    #[test]
    fn test_collect_member_accesses() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit useIt(a: Token, b: Token): Boolean {
    a.transfer(0);
    b.transfer(1);
    a.balanceOf();
}
"#;
        let uses = parser.collect_member_accesses(source, "transfer");
        assert_eq!(uses.len(), 2);
        assert_eq!(uses[0].base_name, "a");
        assert_eq!(uses[0].base, MemberBase::Path(vec!["a".to_string()]));
        assert_eq!(uses[1].base_name, "b");
        assert!(parser.collect_member_accesses(source, "missing").is_empty());

        // A member-chain base is captured in full.
        let chained = r#"
struct Wallet { token: Token, }
circuit g(w: Wallet): Boolean {
    w.token.transfer(0);
}
"#;
        let uses = parser.collect_member_accesses(chained, "transfer");
        assert_eq!(uses.len(), 1);
        assert_eq!(uses[0].base_name, "w.token");
        assert_eq!(
            uses[0].base,
            MemberBase::Path(vec!["w".to_string(), "token".to_string()])
        );
    }

    #[test]
    fn test_get_struct_field_type() {
        let mut parser = ParserEngine::new();
        let source = r#"
struct Wallet {
    token: Token,
    owner: Address,
}

type MyWallet = Wallet;

circuit f(w: Wallet): Boolean {
    return w.token.transfer(0);
}
"#;
        assert_eq!(
            parser
                .get_struct_field_type(source, "Wallet", "token")
                .as_deref(),
            Some("Token")
        );
        assert_eq!(
            parser
                .get_struct_field_type(source, "MyWallet", "owner")
                .as_deref(),
            Some("Address")
        );
        assert!(
            parser
                .get_struct_field_type(source, "Wallet", "missing")
                .is_none()
        );
        assert!(parser.struct_exists(source, "Wallet"));
        assert!(!parser.struct_exists(source, "Nope"));
    }

    #[test]
    fn test_member_access_base_chain() {
        let mut parser = ParserEngine::new();
        let source = r#"
struct Wallet { token: Token, }
struct Account { wallet: Wallet, }
circuit f(a: Account): Boolean {
    a.wallet.token.transfer(0, 1);
}
"#;
        let line = 4;
        let text = source.lines().nth(line as usize).unwrap();
        let col = text.find("transfer").unwrap() as u32 + 1;
        let ctx = parser
            .get_member_access_context(source, line, col)
            .expect("member access context");
        assert_eq!(ctx.base_name, "a.wallet.token");
        assert_eq!(
            ctx.base,
            MemberBase::Path(vec![
                "a".to_string(),
                "wallet".to_string(),
                "token".to_string()
            ])
        );
        assert_eq!(ctx.member_name, "transfer");
    }

    #[test]
    fn test_member_base_call_and_index() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token { circuit transfer(to: Address): Boolean; }
circuit make(): Token { }
circuit useCall(): Boolean {
    return make().transfer(0);
}
ledger items: Map<Address, Token>;
circuit useIndex(): Boolean {
    return items[0].transfer(0);
}
"#;
        let line = 4;
        let text = source.lines().nth(line as usize).unwrap();
        let col = text.find("transfer").unwrap() as u32 + 1;
        let ctx = parser
            .get_member_access_context(source, line, col)
            .expect("call base context");
        assert_eq!(
            ctx.base,
            MemberBase::Call {
                callee: "make".to_string(),
                args: vec![],
            }
        );
        assert_eq!(ctx.member_name, "transfer");

        let line = 8;
        let text = source.lines().nth(line as usize).unwrap();
        let col = text.find("transfer").unwrap() as u32 + 1;
        let ctx = parser
            .get_member_access_context(source, line, col)
            .expect("index base context");
        assert_eq!(
            ctx.base,
            MemberBase::Index(Box::new(MemberBase::Path(vec!["items".to_string()])))
        );
    }

    #[test]
    fn test_member_base_method_call() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Factory { circuit create(): Token; }
contract Token { circuit transfer(to: Address): Boolean; }
circuit useIt(factory: Factory, tokens: Map<Address, Token>): Boolean {
    factory.create().transfer(0);
    tokens.lookup(0).transfer(0);
    return true;
}
"#;
        let line = 4;
        let text = source.lines().nth(line as usize).unwrap();
        let col = text.find("transfer").unwrap() as u32 + 1;
        let ctx = parser
            .get_member_access_context(source, line, col)
            .expect("contract method-call base");
        assert_eq!(
            ctx.base,
            MemberBase::MethodCall(
                Box::new(MemberBase::Path(vec!["factory".to_string()])),
                "create".to_string()
            )
        );
        assert_eq!(ctx.base_name, "factory.create()");
        assert_eq!(ctx.member_name, "transfer");

        let line = 5;
        let text = source.lines().nth(line as usize).unwrap();
        let col = text.find("transfer").unwrap() as u32 + 1;
        let ctx = parser
            .get_member_access_context(source, line, col)
            .expect("builtin method-call base");
        assert_eq!(
            ctx.base,
            MemberBase::MethodCall(
                Box::new(MemberBase::Path(vec!["tokens".to_string()])),
                "lookup".to_string()
            )
        );
        assert_eq!(ctx.base_name, "tokens.lookup()");
    }

    #[test]
    fn test_member_base_access_on_call() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit useIt(items: List<Token>): Boolean {
    items.head().value.transfer(0);
    return true;
}
"#;
        let line = 2;
        let text = source.lines().nth(line as usize).unwrap();
        let col = text.find("transfer").unwrap() as u32 + 1;
        let ctx = parser
            .get_member_access_context(source, line, col)
            .expect("field-on-method-call base");
        assert_eq!(
            ctx.base,
            MemberBase::Access(
                Box::new(MemberBase::MethodCall(
                    Box::new(MemberBase::Path(vec!["items".to_string()])),
                    "head".to_string()
                )),
                "value".to_string()
            )
        );
        assert_eq!(ctx.base_name, "items.head().value");
        assert_eq!(ctx.member_name, "transfer");
    }

    #[test]
    fn test_member_base_call_args() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit id<T>(x: T): T { return x; }
circuit useIt(token: Token): Boolean {
    return id(token).transfer(0);
}
"#;
        let line = 3;
        let text = source.lines().nth(line as usize).unwrap();
        let col = text.find("transfer").unwrap() as u32 + 1;
        let ctx = parser
            .get_member_access_context(source, line, col)
            .expect("call-with-args base");
        assert_eq!(
            ctx.base,
            MemberBase::Call {
                callee: "id".to_string(),
                args: vec![MemberBase::Path(vec!["token".to_string()])],
            }
        );
        assert_eq!(ctx.base_name, "id()");
    }

    #[test]
    fn test_callable_signature_generic() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit id<T>(x: T): T { return x; }
circuit plain(a: Field): Boolean { return true; }
"#;
        let sig = parser.get_callable_signature(source, "id").unwrap();
        assert_eq!(sig.param_types, vec!["T".to_string()]);
        assert_eq!(sig.generic_params, "<T>");
        assert_eq!(sig.return_type.as_deref(), Some("T"));

        let plain = parser.get_callable_signature(source, "plain").unwrap();
        assert_eq!(plain.param_types, vec!["Field".to_string()]);
        assert!(plain.generic_params.is_empty());
        assert_eq!(plain.return_type.as_deref(), Some("Boolean"));
    }

    #[test]
    fn test_struct_type_params() {
        let mut parser = ParserEngine::new();
        let source = r#"
struct Box<T> { value: T; }
struct Plain { n: Uint<64>; }
"#;
        assert_eq!(
            parser.get_struct_type_params(source, "Box").as_deref(),
            Some("<T>")
        );
        assert_eq!(parser.get_struct_type_params(source, "Plain"), None);
    }

    #[test]
    fn test_get_variable_type_raw_preserves_generics() {
        let mut parser = ParserEngine::new();
        let source = r#"
ledger items: Map<Address, Token>;
circuit f(): Boolean { return true; }
"#;
        assert_eq!(
            parser.get_variable_type_raw(source, "items").as_deref(),
            Some("Map<Address, Token>")
        );
        // `get_variable_type` strips generics to the head type name.
        assert_eq!(
            parser.get_variable_type(source, "items").as_deref(),
            Some("Map")
        );

        // Parameter (`parg`) types preserve generics too.
        let parg_src = r#"
circuit useIt(items: Vector<10, T_Token>): Boolean {
    return true;
}
"#;
        assert_eq!(
            parser.get_variable_type_raw(parg_src, "items").as_deref(),
            Some("Vector<10, T_Token>")
        );
    }

    #[test]
    fn test_circuit_at_position() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}
"#;
        // On the circuit name.
        assert_eq!(
            parser.circuit_at_position(source, 2, 15),
            Some(("Token".to_string(), "transfer".to_string()))
        );
        // On a type inside the signature.
        assert!(parser.circuit_at_position(source, 2, 25).is_none());
        // On the contract name.
        assert!(parser.circuit_at_position(source, 1, 10).is_none());
    }

    #[test]
    fn test_enclosing_member_call() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address, amount: Uint<64>): Boolean;
}

circuit useParam(ref: Token): Boolean {
    return ref.transfer(0, 1);
}
"#;
        let line = 6;
        let line_text = source.lines().nth(line as usize).unwrap();
        let char = line_text.find(',').unwrap() as u32 + 1;
        let ctx = parser
            .get_enclosing_member_call(source, line, char)
            .expect("enclosing member call");
        assert_eq!(ctx.base_name, "ref");
        assert_eq!(ctx.member_name, "transfer");
    }

    #[test]
    fn test_ecdecl_hover() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}
"#;
        let info = parser
            .hover_info(source, 1, 9)
            .expect("hover for contract type");
        assert!(info.content.contains("Contract type"));
        assert!(info.content.contains("Token"));
        assert!(
            info.content
                .contains("circuit transfer(to: Address): Boolean")
        );
    }

    #[test]
    fn test_contract_type_in_completion_symbols() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}
"#;
        let symbols = parser.get_completion_symbols(source);
        let contract = symbols
            .iter()
            .find(|s| s.name == "Token")
            .expect("contract type in completion symbols");
        assert_eq!(contract.kind, CompletionSymbolKind::Contract);
        assert!(
            contract
                .documentation
                .as_deref()
                .unwrap_or("")
                .contains("transfer")
        );
    }

    #[test]
    fn test_goto_definition_contract_implements() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Address): Boolean;
}

contract implements Token;
"#;
        let loc = parser
            .goto_definition(source, 5, 21)
            .expect("definition of the implemented contract type");
        assert_eq!(loc.selection_range.start.line, 1);
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
            function_tokens
                .iter()
                .any(|t| t.modifiers.contains(&SemanticTokenModifier::Declaration)),
            "Function should have Declaration modifier"
        );

        // Check for type tokens (Field)
        let type_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Type)
            .collect();
        assert!(!type_tokens.is_empty(), "Should find type tokens");
        assert!(
            type_tokens
                .iter()
                .any(|t| t.modifiers.contains(&SemanticTokenModifier::DefaultLibrary)),
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
        assert!(
            !property_tokens.is_empty(),
            "Should find property tokens for struct fields"
        );
    }

    #[test]
    fn test_semantic_tokens_new_keywords() {
        let mut parser = ParserEngine::new();
        let source = r#"
contract Token {
    circuit transfer(to: Field): Boolean;
}

contract implements Token;

circuit f(): [] {
    emit([1, 2]);
    return [];
}
"#;
        let tokens = parser.get_semantic_tokens(source);
        let keywords: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Keyword)
            .collect();
        assert!(
            keywords.len() >= 2,
            "expected keyword tokens for emit/implements, got {keywords:?}"
        );
        assert!(
            keywords.iter().any(|t| t.range.start.line == 5),
            "expected keyword token(s) on the `contract implements` line"
        );
        assert!(
            keywords.iter().any(|t| t.range.start.line == 8),
            "expected the `emit` keyword token"
        );
    }

    #[test]
    fn test_semantic_tokens_member_access() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit f(ref: Field): Field {
    return ref.value;
}
"#;
        let tokens = parser.get_semantic_tokens(source);
        assert!(
            tokens
                .iter()
                .any(|t| t.token_type == SemanticTokenType::Property && t.range.start.line == 2),
            "member field access should be a Property token"
        );
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

    #[test]
    fn test_find_references_circuit() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit add(a: Field, b: Field): Field {
    return a + b;
}

circuit main(): Field {
    let x = add(1, 2);
    let y = add(3, 4);
    return add(x, y);
}
"#;
        let refs = parser.find_references(source, "add");

        // Should find: 1 definition + 3 calls = 4 references
        assert_eq!(refs.len(), 4, "Should find 4 references to 'add'");

        // Check that exactly one is a definition
        let definitions: Vec<_> = refs.iter().filter(|r| r.is_definition).collect();
        assert_eq!(definitions.len(), 1, "Should find exactly 1 definition");

        // Check that three are usages
        let usages: Vec<_> = refs.iter().filter(|r| !r.is_definition).collect();
        assert_eq!(usages.len(), 3, "Should find 3 usages");
    }

    #[test]
    fn test_find_references_struct() {
        let mut parser = ParserEngine::new();
        let source = r#"
struct Point {
    x: Field;
    y: Field;
}

circuit make_point(): Point {
    return Point { x: 0, y: 0 };
}

circuit use_point(p: Point): Field {
    return p.x;
}
"#;
        let refs = parser.find_references(source, "Point");

        // Should find: 1 definition + usages in return type, function param type
        assert!(
            refs.len() >= 3,
            "Should find at least 3 references to 'Point'"
        );

        // Check that exactly one is a definition
        let definitions: Vec<_> = refs.iter().filter(|r| r.is_definition).collect();
        assert_eq!(definitions.len(), 1, "Should find exactly 1 definition");
    }

    #[test]
    fn test_find_references_no_match() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit foo(): Field {
    return 1;
}
"#;
        let refs = parser.find_references(source, "bar");

        // Should find nothing
        assert!(refs.is_empty(), "Should find no references to 'bar'");
    }

    // ========== New edge case tests ==========

    #[test]
    fn test_find_references_ledger() {
        let mut parser = ParserEngine::new();
        let source = r#"
ledger balance: Map<Address, Uint<64>>;

circuit deposit(amount: Uint<64>): Void {
    balance.insert(sender(), amount);
}

circuit withdraw(amount: Uint<64>): Uint<64> {
    return balance.lookup(sender());
}
"#;
        let refs = parser.find_references(source, "balance");

        // Should find at least the definition
        assert!(!refs.is_empty(), "Should find references to 'balance'");

        // Check that exactly one is a definition
        let definitions: Vec<_> = refs.iter().filter(|r| r.is_definition).collect();
        assert_eq!(
            definitions.len(),
            1,
            "Should find exactly 1 definition of 'balance'"
        );
    }

    #[test]
    fn test_goto_definition_ledger() {
        let mut parser = ParserEngine::new();
        let source = r#"ledger counter: Counter;

circuit increment(): Void {
    counter.increment(1);
}"#;
        // Go to definition of "counter" on line 0 (the ledger declaration itself)
        let loc = parser.goto_definition(source, 0, 7);
        assert!(loc.is_some(), "Should find ledger definition");
        let loc = loc.unwrap();
        // Should point to line 0 where counter is defined
        assert_eq!(loc.selection_range.start.line, 0);
    }

    #[test]
    fn test_semantic_tokens_ledger() {
        let mut parser = ParserEngine::new();
        let source = r#"
ledger balance: Map<Address, Uint<64>>;
ledger counter: Counter;
"#;
        let tokens = parser.get_semantic_tokens(source);

        // Check for property tokens (ledger names)
        let property_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Property)
            .collect();
        assert!(
            property_tokens.len() >= 2,
            "Should find at least 2 property tokens for ledgers"
        );

        // Check that ledger tokens have readonly modifier
        assert!(
            property_tokens
                .iter()
                .any(|t| t.modifiers.contains(&SemanticTokenModifier::Readonly)),
            "Ledger should have Readonly modifier"
        );
    }

    #[test]
    fn test_semantic_tokens_witness() {
        let mut parser = ParserEngine::new();
        let source = r#"
witness get_secret(): Field;
witness get_private_key(id: Uint<32>): Bytes<32>;
"#;
        let tokens = parser.get_semantic_tokens(source);

        // Check for function tokens (witness names)
        let function_tokens: Vec<_> = tokens
            .iter()
            .filter(|t| t.token_type == SemanticTokenType::Function)
            .collect();
        assert!(
            function_tokens.len() >= 2,
            "Should find at least 2 function tokens for witnesses"
        );

        // Check that witness tokens have declaration modifier
        assert!(
            function_tokens
                .iter()
                .all(|t| t.modifiers.contains(&SemanticTokenModifier::Declaration)),
            "Witness functions should have Declaration modifier"
        );
    }

    #[test]
    fn test_folding_ranges_nested() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit test(x: Field): Field {
    if (x > 0) {
        const a = 1;
        if (x > 10) {
            const b = 2;
            return b;
        }
        return a;
    }
    return 0;
}
"#;
        let ranges = parser.folding_ranges(source);

        // Should find at least 3 folding ranges: circuit body + 2 if statements
        assert!(
            ranges.len() >= 3,
            "Should find at least 3 folding ranges for nested blocks, found {}",
            ranges.len()
        );

        // Check that we have different starting lines for nested structures
        let start_lines: Vec<_> = ranges.iter().map(|r| r.start_line).collect();
        let unique_lines: std::collections::HashSet<_> = start_lines.iter().collect();
        assert!(
            unique_lines.len() >= 2,
            "Should have folding ranges on different lines"
        );
    }

    #[test]
    fn test_document_symbols_enum_variants() {
        let mut parser = ParserEngine::new();
        let source = r#"
enum Status {
    Pending,
    Active,
    Completed,
}
"#;
        let symbols = parser.document_symbols(source);

        assert_eq!(symbols.len(), 1, "Should find 1 enum");
        assert_eq!(symbols[0].name, "Status");
        assert_eq!(symbols[0].kind, SymbolKind::ENUM);

        // Check that enum has variant children
        let children = symbols[0].children.as_ref();
        assert!(children.is_some(), "Enum should have children");
        let children = children.unwrap();
        assert_eq!(children.len(), 3, "Should find 3 enum variants");

        // Verify variant names
        let variant_names: Vec<_> = children.iter().map(|c| c.name.as_str()).collect();
        assert!(
            variant_names.contains(&"Pending"),
            "Should have Pending variant"
        );
        assert!(
            variant_names.contains(&"Active"),
            "Should have Active variant"
        );
        assert!(
            variant_names.contains(&"Completed"),
            "Should have Completed variant"
        );
    }

    #[test]
    fn test_document_symbols_struct_fields() {
        let mut parser = ParserEngine::new();
        let source = r#"
struct Transaction {
    sender: Address;
    recipient: Address;
    amount: Uint<64>;
    nonce: Field;
}
"#;
        let symbols = parser.document_symbols(source);

        assert_eq!(symbols.len(), 1, "Should find 1 struct");
        assert_eq!(symbols[0].name, "Transaction");
        assert_eq!(symbols[0].kind, SymbolKind::STRUCT);

        // Check that struct has field children
        let children = symbols[0].children.as_ref();
        assert!(children.is_some(), "Struct should have children");
        let children = children.unwrap();
        assert_eq!(children.len(), 4, "Should find 4 struct fields");

        // All children should be fields
        assert!(
            children.iter().all(|c| c.kind == SymbolKind::FIELD),
            "All struct children should be fields"
        );
    }

    #[test]
    fn test_document_symbols_module() {
        let mut parser = ParserEngine::new();
        let source = r#"
module Math {
    export circuit add(a: Field, b: Field): Field {
        return a + b;
    }

    export circuit multiply(a: Field, b: Field): Field {
        return a * b;
    }
}
"#;
        let symbols = parser.document_symbols(source);

        assert_eq!(symbols.len(), 1, "Should find 1 module");
        assert_eq!(symbols[0].name, "Math");
        assert_eq!(symbols[0].kind, SymbolKind::MODULE);

        // Check that module has circuit children
        let children = symbols[0].children.as_ref();
        assert!(children.is_some(), "Module should have children");
        let children = children.unwrap();
        assert!(
            children.len() >= 2,
            "Should find at least 2 circuits in module"
        );
    }

    #[test]
    fn test_hover_ledger_declaration() {
        let mut parser = ParserEngine::new();
        let source = r#"ledger balance: Map<Address, Uint<64>>;"#;
        // Hover on "balance" at position (0, 7)
        let info = parser.hover_info(source, 0, 7);
        assert!(info.is_some(), "Should find hover info for ledger");
        let info = info.unwrap();
        assert!(
            info.content.contains("ledger"),
            "Hover should mention 'ledger'"
        );
        assert!(
            info.content.contains("balance"),
            "Hover should contain name 'balance'"
        );
    }

    #[test]
    fn test_hover_witness_declaration() {
        let mut parser = ParserEngine::new();
        let source = r#"witness get_secret(): Field;"#;
        // Hover on "get_secret" at position (0, 8)
        let info = parser.hover_info(source, 0, 8);
        assert!(info.is_some(), "Should find hover info for witness");
        let info = info.unwrap();
        assert!(
            info.content.contains("witness") || info.content.contains("Witness"),
            "Hover should mention 'witness'"
        );
    }

    #[test]
    fn test_completion_symbols_with_documentation() {
        let mut parser = ParserEngine::new();
        let source = r#"
circuit calculate(x: Field, y: Field): Field {
    return x * y + 1;
}

struct Config {
    threshold: Uint<64>;
    enabled: Boolean;
}
"#;
        let symbols = parser.get_completion_symbols(source);

        // Check circuit has documentation
        let circuit_sym = symbols.iter().find(|s| s.name == "calculate");
        assert!(circuit_sym.is_some(), "Should find calculate circuit");
        let circuit_sym = circuit_sym.unwrap();
        assert!(
            circuit_sym.documentation.is_some(),
            "Circuit should have documentation"
        );
        assert!(
            circuit_sym
                .documentation
                .as_ref()
                .unwrap()
                .contains("Circuit function"),
            "Documentation should mention Circuit function"
        );

        // Check struct has documentation
        let struct_sym = symbols.iter().find(|s| s.name == "Config");
        assert!(struct_sym.is_some(), "Should find Config struct");
        let struct_sym = struct_sym.unwrap();
        assert!(
            struct_sym.documentation.is_some(),
            "Struct should have documentation"
        );
        assert!(
            struct_sym
                .documentation
                .as_ref()
                .unwrap()
                .contains("Struct type"),
            "Documentation should mention Struct type"
        );
    }

    #[test]
    fn test_import_with_complex_path() {
        let mut parser = ParserEngine::new();
        let source = r#"
import "../../contracts/token/ERC20" prefix Token_;
import "../../../shared/utils/Helpers" prefix Help_;
import CompactStandardLibrary;

circuit main(): Field {
    return 1;
}
"#;
        let imports = parser.get_imports(source);

        assert_eq!(imports.len(), 3, "Should find 3 imports");

        // Check complex relative paths are handled
        let token_import = imports.iter().find(|i| i.path.contains("ERC20"));
        assert!(token_import.is_some(), "Should find ERC20 import");
        assert!(token_import.unwrap().is_file, "Should be a file import");

        let helpers_import = imports.iter().find(|i| i.path.contains("Helpers"));
        assert!(helpers_import.is_some(), "Should find Helpers import");
        assert_eq!(
            helpers_import.unwrap().prefix.as_deref(),
            Some("Help_"),
            "Should have Help_ prefix"
        );
    }

    // ========== Doc comment tests ==========

    #[test]
    fn test_clean_comment_text_single_line() {
        let parser = ParserEngine::new();
        assert_eq!(parser.clean_comment_text("// hello world"), "hello world");
        assert_eq!(parser.clean_comment_text("//hello"), "hello");
        assert_eq!(parser.clean_comment_text("//   spaced   "), "spaced");
    }

    #[test]
    fn test_clean_comment_text_block() {
        let parser = ParserEngine::new();
        assert_eq!(parser.clean_comment_text("/* simple */"), "simple");
        assert_eq!(parser.clean_comment_text("/*multi\nline*/"), "multi\nline");
    }

    #[test]
    fn test_clean_comment_text_jsdoc() {
        let parser = ParserEngine::new();
        // JSDoc-style with leading asterisks on each line
        let jsdoc = r#"/**
 * This is a JSDoc comment.
 * It has multiple lines.
 */"#;
        let cleaned = parser.clean_comment_text(jsdoc);
        assert!(
            cleaned.contains("This is a JSDoc comment"),
            "Should contain first line"
        );
        assert!(
            cleaned.contains("It has multiple lines"),
            "Should contain second line"
        );
    }

    #[test]
    fn test_hover_with_doc_comment() {
        let mut parser = ParserEngine::new();
        let source = r#"// Adds two field elements together.
// Returns the sum.
circuit add(a: Field, b: Field): Field {
    return a + b;
}"#;
        // Hover on "add" at position (2, 8)
        let info = parser.hover_info(source, 2, 8);
        assert!(info.is_some(), "Should find hover info");
        let info = info.unwrap();
        assert!(
            info.content.contains("Circuit function"),
            "Should contain type description"
        );
        assert!(
            info.content.contains("Adds two field elements together"),
            "Should contain doc comment: {}",
            info.content
        );
        assert!(
            info.content.contains("Returns the sum"),
            "Should contain second line of doc: {}",
            info.content
        );
    }

    #[test]
    fn test_hover_with_block_doc_comment() {
        let mut parser = ParserEngine::new();
        let source = r#"/* A point in 2D space */
struct Point {
    x: Field;
    y: Field;
}"#;
        // Hover on "Point" at position (1, 7)
        let info = parser.hover_info(source, 1, 7);
        assert!(info.is_some(), "Should find hover info");
        let info = info.unwrap();
        assert!(
            info.content.contains("Struct type"),
            "Should contain type description"
        );
        assert!(
            info.content.contains("A point in 2D space"),
            "Should contain block doc comment: {}",
            info.content
        );
    }

    #[test]
    fn test_completion_with_doc_comment() {
        let mut parser = ParserEngine::new();
        let source = r#"// Calculates the sum of two numbers.
circuit add(a: Field, b: Field): Field {
    return a + b;
}

/**
 * Represents a 2D coordinate.
 * Used for geometric calculations.
 */
struct Point {
    x: Field;
    y: Field;
}"#;
        let symbols = parser.get_completion_symbols(source);

        // Check circuit has doc comment
        let add_sym = symbols.iter().find(|s| s.name == "add");
        assert!(add_sym.is_some(), "Should find add circuit");
        let add_doc = add_sym.unwrap().documentation.as_ref().unwrap();
        assert!(
            add_doc.contains("Calculates the sum"),
            "Circuit docs should contain user comment: {}",
            add_doc
        );

        // Check struct has JSDoc comment
        let point_sym = symbols.iter().find(|s| s.name == "Point");
        assert!(point_sym.is_some(), "Should find Point struct");
        let point_doc = point_sym.unwrap().documentation.as_ref().unwrap();
        assert!(
            point_doc.contains("Represents a 2D coordinate"),
            "Struct docs should contain JSDoc comment: {}",
            point_doc
        );
    }

    #[test]
    fn test_hover_no_doc_comment() {
        let mut parser = ParserEngine::new();
        // No doc comment, just the circuit
        let source = "circuit add(a: Field, b: Field): Field { return a + b; }";
        let info = parser.hover_info(source, 0, 8);
        assert!(info.is_some(), "Should find hover info");
        let info = info.unwrap();
        assert!(
            info.content.contains("Circuit function"),
            "Should contain type description"
        );
        // Should not have the separator since there's no doc comment
        assert!(
            !info.content.contains("---\n\n"),
            "Should not have doc separator without doc: {}",
            info.content
        );
    }
}
