//! References concerns for the parser engine.

use super::*;

impl ParserEngine {
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
    pub(super) fn collect_references(
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
            // Selective imports bind the exported symbol, so the element id is
            // a reference: `import { Order } from "./lib"`.
            "import_element" => {
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
    pub(super) fn check_function_call_name(
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
}
