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

mod completion;
mod definition;
mod hover;
mod imports;
mod member;
mod references;
mod resolution;
mod signature;
mod symbols;
mod syntax;
mod tokens;
mod types;

pub use syntax::is_builtin_type;
pub use types::*;

use lsp_types::{DocumentSymbol, FoldingRange, FoldingRangeKind, Position, Range, SymbolKind};
use tree_sitter::{Node, Parser, Tree};

/// Parser engine wrapping tree-sitter-compact.
pub struct ParserEngine {
    parser: Parser,
    /// Source of the most recent [`ParserEngine::parse`].
    ///
    /// tree-sitter reports byte columns while LSP positions are UTF-16 code
    /// units, so every emitted range is translated through this text (with the
    /// line index below) to convert byte offsets back to UTF-16 columns.
    source: String,
    /// Byte offset of the start of each line in `source` (the last entry is
    /// the character after the final newline).
    line_starts: Vec<usize>,
}

impl ParserEngine {
    /// Create a new parser engine.
    pub fn new() -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_compact::LANGUAGE.into())
            .expect("Failed to load Compact grammar");
        Self {
            parser,
            source: String::new(),
            line_starts: vec![0],
        }
    }

    /// Parse source code and return the syntax tree.
    pub fn parse(&mut self, source: &str) -> Option<Tree> {
        self.source.clear();
        self.source.push_str(source);
        self.line_starts.clear();
        self.line_starts.push(0);
        for (index, byte) in source.bytes().enumerate() {
            if byte == b'\n' {
                self.line_starts.push(index + 1);
            }
        }
        self.parser.parse(source, None)
    }
}

impl Default for ParserEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod fuzz_tests;

#[cfg(test)]
mod tests;
