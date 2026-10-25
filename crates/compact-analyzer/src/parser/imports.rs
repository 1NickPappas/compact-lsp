//! Imports concerns for the parser engine.

use super::*;

impl ParserEngine {
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

    /// Recursively collect import statements from the AST.
    pub(super) fn collect_imports(&self, node: Node, source: &[u8], imports: &mut Vec<ImportInfo>) {
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
    pub(super) fn extract_import(&self, node: Node, source: &[u8]) -> Option<ImportInfo> {
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
}
