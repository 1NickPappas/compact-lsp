//! Document state management.

use lsp_types::Range;
use ropey::Rope;

/// A document we're tracking (an open file in the editor).
#[derive(Debug, Clone)]
pub struct Document {
    /// The document content, stored as a rope for efficient editing.
    /// Rope is a data structure optimized for text editing:
    /// - O(log N) for insert/delete at any position
    /// - Cheap clones (structural sharing)
    pub content: Rope,

    /// Document version (incremented by editor on each change).
    /// We can use this to detect out-of-order updates.
    pub version: i32,
}

impl Document {
    /// Apply a single LSP content change: an incremental edit when `range` is
    /// present, otherwise a full-document replacement.
    pub fn apply_change(&mut self, range: Option<&Range>, text: &str) {
        match range {
            Some(range) => {
                let start_idx = self.content.line_to_char(range.start.line as usize)
                    + range.start.character as usize;
                let end_idx = self.content.line_to_char(range.end.line as usize)
                    + range.end.character as usize;
                self.content.remove(start_idx..end_idx);
                self.content.insert(start_idx, text);
            }
            None => {
                self.content = Rope::from_str(text);
            }
        }
    }
}

#[cfg(test)]
mod tests;
