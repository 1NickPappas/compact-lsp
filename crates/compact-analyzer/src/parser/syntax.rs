//! Low-level tree-sitter helpers shared across parser concerns.

use super::*;

impl ParserEngine {
    /// Get text from a named field of a node.
    pub(super) fn get_field_text(&self, node: Node, field: &str, source: &[u8]) -> Option<String> {
        let child = node.child_by_field_name(field)?;
        Some(self.node_text(child, source))
    }

    /// Get text content of a node.
    pub(super) fn node_text(&self, node: Node, source: &[u8]) -> String {
        node.utf8_text(source).unwrap_or("").to_string()
    }

    /// Extract documentation comments preceding a node.
    /// Collects contiguous comment lines directly above a declaration.
    pub(super) fn extract_doc_comment(&self, node: Node, source: &[u8]) -> Option<String> {
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
    pub(super) fn clean_comment_text(&self, text: &str) -> String {
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
    pub(super) fn node_range(&self, node: Node) -> Range {
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
    pub(super) fn node_to_symbol_location(&self, node: Node) -> SymbolLocation {
        let start = node.start_position();
        let end = node.end_position();
        SymbolLocation {
            start_line: start.row as u32,
            start_char: start.column as u32,
            end_line: end.row as u32,
            end_char: end.column as u32,
        }
    }
}

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
