//! Document state management.

use lsp_types::{Position, Range};
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
    ///
    /// Malformed ranges from a client (line past the end, character past the
    /// line, inverted bounds) are clamped or ignored rather than panicking,
    /// so a bad incremental edit cannot take the server down.
    pub fn apply_change(&mut self, range: Option<&Range>, text: &str) {
        match range {
            Some(range) => {
                let start_idx = self.char_index(&range.start);
                let end_idx = self.char_index(&range.end);
                if start_idx > end_idx {
                    return;
                }
                self.content.remove(start_idx..end_idx);
                self.content.insert(start_idx, text);
            }
            None => {
                self.content = Rope::from_str(text);
            }
        }
    }

    /// Char index for an LSP position, clamped to the document (and to the
    /// line when the position is within it).
    ///
    /// `position.character` is a UTF-16 code-unit offset (the LSP default
    /// encoding), so it is translated rather than added to the char index.
    fn char_index(&self, position: &Position) -> usize {
        let line = position.line as usize;
        if line >= self.content.len_lines() {
            return self.content.len_chars();
        }
        let line_start = self.content.line_to_char(line);
        let line_end = if line + 1 < self.content.len_lines() {
            self.content.line_to_char(line + 1)
        } else {
            self.content.len_chars()
        };
        let start_utf16 = self.content.char_to_utf16_cu(line_start);
        let end_utf16 = self.content.char_to_utf16_cu(line_end);
        let target_utf16 = (start_utf16 + position.character as usize).min(end_utf16);
        self.content.utf16_cu_to_char(target_utf16)
    }
}

#[cfg(test)]
mod tests;
