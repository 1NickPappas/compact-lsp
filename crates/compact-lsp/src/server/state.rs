//! Document state management.

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
