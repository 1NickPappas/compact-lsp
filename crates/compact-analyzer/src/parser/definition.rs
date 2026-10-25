//! Definition concerns for the parser engine.

use super::*;

impl ParserEngine {
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
        let point = self.point_from_utf16(line, character);

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
    pub(super) fn get_definition_name_range(&self, node: Node, _source: &[u8]) -> Option<Range> {
        let kind = node.kind();

        let name_node = match kind {
            "cdefn" | "wdecl" => node.child_by_field_name("id"),
            "ldecl" | "struct" | "enumdef" | "mdefn" | "ecdecl" => node.child_by_field_name("name"),
            _ => None,
        }?;

        Some(self.node_range(name_node))
    }
}
