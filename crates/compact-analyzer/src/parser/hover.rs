//! Hover concerns for the parser engine.

use super::*;

impl ParserEngine {
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
    pub(super) fn hover_for_node(
        &self,
        node: Node,
        source: &[u8],
        root: &Node,
    ) -> Option<HoverInfo> {
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
    pub(super) fn keyword_docs(&self, text: &str) -> Option<String> {
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
    pub(super) fn builtin_type_docs(&self, text: &str) -> Option<String> {
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
    pub(super) fn definition_hover(&self, node: Node, source: &[u8]) -> Option<HoverInfo> {
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
    pub(super) fn extract_circuit_signature(&self, node: Node, source: &[u8]) -> Option<String> {
        let name = self.get_field_text(node, "id", source)?;
        let params = self.extract_params(node, source);
        let return_type = self
            .get_field_text(node, "type", source)
            .unwrap_or_default();
        Some(format!("circuit {}({}): {}", name, params, return_type))
    }

    /// Extract witness function signature.
    pub(super) fn extract_witness_signature(&self, node: Node, source: &[u8]) -> Option<String> {
        let name = self.get_field_text(node, "id", source)?;
        let params = self.extract_params(node, source);
        let return_type = self
            .get_field_text(node, "type", source)
            .unwrap_or_default();
        Some(format!("witness {}({}): {}", name, params, return_type))
    }

    /// Extract function parameters as a string.
    pub(super) fn extract_params(&self, node: Node, source: &[u8]) -> String {
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
    pub(super) fn extract_struct_fields(&self, node: Node, source: &[u8]) -> Vec<String> {
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
    pub(super) fn extract_enum_variants(&self, node: Node, source: &[u8]) -> Vec<String> {
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
    pub(super) fn find_definition_hover(
        &self,
        name: &str,
        root: &Node,
        source: &[u8],
    ) -> Option<HoverInfo> {
        self.find_definition_node(name, *root, source)
            .and_then(|def_node| self.definition_hover(def_node, source))
    }

    /// Recursively search for a definition with the given name.
    pub(super) fn find_definition_node<'a>(
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
}
