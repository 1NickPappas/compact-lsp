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
