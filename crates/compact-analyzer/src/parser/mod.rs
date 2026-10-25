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

mod syntax;
mod types;

pub use syntax::is_builtin_type;
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

        // Selective elements: `import { A as B, C } from …`.
        let mut elements = Vec::new();
        if let Some(selection) = node.child_by_field_name("selection") {
            let mut cursor = selection.walk();
            for element in selection.children_by_field_name("element", &mut cursor) {
                let Some(id) = element.child_by_field_name("id") else {
                    continue;
                };
                let name = self.node_text(id, source);
                let alias = element
                    .child_by_field_name("alias")
                    .map(|a| self.node_text(a, source));
                elements.push((name, alias));
            }
        }

        Some(ImportInfo {
            path,
            is_file,
            prefix,
            elements,
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
                let fun = node.child_by_field_name("fun");
                let callee = fun
                    .and_then(|fun| fun.child_by_field_name("id"))
                    .map(|id| self.node_text(id, source));
                match callee {
                    Some(callee) => {
                        // Explicit type arguments: `id<Uint<64>>(x)`. Compact
                        // requires these for generic calls, so read them rather
                        // than relying solely on argument inference.
                        let type_args = fun
                            .and_then(|fun| fun.child_by_field_name("gargs"))
                            .map(|gargs| {
                                let mut cursor = gargs.walk();
                                gargs
                                    .children_by_field_name("garg", &mut cursor)
                                    .map(|garg| self.node_text(garg, source).trim().to_string())
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or_default();
                        let mut args = Vec::new();
                        let mut cursor = node.walk();
                        for arg in node.children_by_field_name("expr", &mut cursor) {
                            args.push(self.member_base(arg, source));
                        }
                        MemberBase::Call {
                            callee,
                            args,
                            type_args,
                        }
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

    /// Whether an `enum` named `name` is declared in this file.
    pub fn enum_exists(&mut self, source: &str, name: &str) -> bool {
        let Some(tree) = self.parse(source) else {
            return false;
        };
        let root = tree.root_node();
        let resolved = self.resolve_type_name(root, source.as_bytes(), name);
        self.find_enum(root, source.as_bytes(), &resolved).is_some()
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

    /// If the position is on a struct-field declaration (`arg` inside a
    /// `struct`) or an enum-variant declaration (`id` inside an `enumdef`),
    /// return the owning type name, the member name, and the kind.
    #[allow(clippy::type_complexity)]
    pub fn type_member_at_position(
        &mut self,
        source: &str,
        line: u32,
        character: u32,
    ) -> Option<(String, String, TypeMemberKind)> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let bytes = source.as_bytes();
        let point = tree_sitter::Point {
            row: line as usize,
            column: character as usize,
        };
        let node = root.descendant_for_point_range(point, point)?;
        if node.kind() != "id" {
            return None;
        }
        let parent = node.parent()?;
        // Struct field: id → arg → struct.
        if parent.kind() == "arg" {
            let owner = parent.parent()?;
            if owner.kind() != "struct" {
                return None;
            }
            let owner_name = self.get_field_text(owner, "name", bytes)?;
            return Some((
                owner_name,
                self.node_text(node, bytes),
                TypeMemberKind::Field,
            ));
        }
        // Enum variant: id → enumdef.
        if parent.kind() == "enumdef" {
            let owner_name = self.get_field_text(parent, "name", bytes)?;
            return Some((
                owner_name,
                self.node_text(node, bytes),
                TypeMemberKind::Variant,
            ));
        }
        None
    }

    /// Range of a struct-field or enum-variant declaration's identifier.
    pub fn find_member_definition_location(
        &mut self,
        source: &str,
        owner_name: &str,
        member_name: &str,
        kind: TypeMemberKind,
    ) -> Option<Range> {
        let tree = self.parse(source)?;
        let root = tree.root_node();
        let bytes = source.as_bytes();
        let resolved = self.resolve_type_name(root, bytes, owner_name);
        match kind {
            TypeMemberKind::Field => {
                let struct_node = self.find_struct(root, bytes, &resolved)?;
                let mut cursor = struct_node.walk();
                for child in struct_node.children(&mut cursor) {
                    if child.kind() == "arg"
                        && self.get_field_text(child, "id", bytes).as_deref() == Some(member_name)
                        && let Some(id) = child.child_by_field_name("id")
                    {
                        return Some(self.node_range(id));
                    }
                }
                None
            }
            TypeMemberKind::Variant => {
                let enum_node = self.find_enum(root, bytes, &resolved)?;
                let mut cursor = enum_node.walk();
                for child in enum_node.children(&mut cursor) {
                    if child.kind() == "id" && self.node_text(child, bytes) == member_name {
                        return Some(self.node_range(child));
                    }
                }
                None
            }
        }
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

    /// Recursively find an `enum` named `name`.
    fn find_enum<'a>(&self, node: Node<'a>, source: &[u8], name: &str) -> Option<Node<'a>> {
        if node.kind() == "enumdef"
            && self.get_field_text(node, "name", source).as_deref() == Some(name)
        {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = self.find_enum(child, source, name) {
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

impl Default for ParserEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
