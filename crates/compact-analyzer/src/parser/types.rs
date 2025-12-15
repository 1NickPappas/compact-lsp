//! Type definitions for the parser module.

use lsp_types::Range;

/// Hover information result.
#[derive(Debug, Clone)]
pub struct HoverInfo {
    /// The content to display (markdown).
    pub content: String,
    /// The range of the hovered element.
    pub range: Option<Range>,
}

/// Definition location result.
#[derive(Debug, Clone)]
pub struct DefinitionLocation {
    /// The range where the definition is located.
    pub range: Range,
    /// The range of just the symbol name (for selection).
    pub selection_range: Range,
}

/// Parameter information for signature help.
#[derive(Debug, Clone)]
pub struct ParameterInfo {
    /// Parameter label (e.g., "a: Field").
    pub label: String,
}

/// Signature information result.
#[derive(Debug, Clone)]
pub struct SignatureInfo {
    /// The full signature label (e.g., "circuit add(a: Field, b: Field): Field").
    pub label: String,
    /// Documentation for the signature.
    pub documentation: Option<String>,
    /// Parameters with their labels.
    pub parameters: Vec<ParameterInfo>,
    /// The index of the active parameter (0-based).
    pub active_parameter: u32,
}

/// Symbol kind for completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionSymbolKind {
    Function,
    Struct,
    Enum,
    Variable,
    Module,
}

/// Location of a symbol in the source code.
#[derive(Debug, Clone)]
pub struct SymbolLocation {
    /// Start line (0-based).
    pub start_line: u32,
    /// Start character (0-based).
    pub start_char: u32,
    /// End line (0-based).
    pub end_line: u32,
    /// End character (0-based).
    pub end_char: u32,
}

/// A symbol for completion.
#[derive(Debug, Clone)]
pub struct CompletionSymbol {
    /// The symbol name.
    pub name: String,
    /// The kind of symbol.
    pub kind: CompletionSymbolKind,
    /// Detail text (e.g., "(a: Field, b: Field): Field").
    pub detail: Option<String>,
    /// Location of the symbol definition.
    pub location: Option<SymbolLocation>,
    /// Documentation for the symbol.
    pub documentation: Option<String>,
}

/// Information about an import statement.
#[derive(Debug, Clone)]
pub struct ImportInfo {
    /// The import path (e.g., "../utils/Utils" or "CompactStandardLibrary").
    pub path: String,
    /// True if this is a file import (quoted path), false if it's an identifier import.
    pub is_file: bool,
    /// The prefix for imported symbols (e.g., "Utils_").
    pub prefix: Option<String>,
}

/// A syntax error detected by tree-sitter parsing.
#[derive(Debug, Clone)]
pub struct SyntaxError {
    /// The error message.
    pub message: String,
    /// The range where the error occurred.
    pub range: Range,
}

/// A semantic token for syntax highlighting.
#[derive(Debug, Clone)]
pub struct SemanticToken {
    /// The range of the token.
    pub range: Range,
    /// The type of the token.
    pub token_type: SemanticTokenType,
    /// Modifiers for the token.
    pub modifiers: Vec<SemanticTokenModifier>,
}

/// Semantic token types for syntax highlighting.
/// Order matters - these are indices into the LSP legend.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SemanticTokenType {
    Function = 0,
    Type = 1,
    Struct = 2,
    Enum = 3,
    EnumMember = 4,
    Parameter = 5,
    Property = 6,
    Variable = 7,
    Namespace = 8,
    TypeParameter = 9,
}

/// Semantic token modifiers for syntax highlighting.
/// These are bit flags (1 << modifier_index).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SemanticTokenModifier {
    Declaration = 0,
    Readonly = 1,
    DefaultLibrary = 2,
}

/// A reference location for Find References.
#[derive(Debug, Clone)]
pub struct ReferenceLocation {
    /// The range of the reference.
    pub range: Range,
    /// True if this is the definition site, false if it's a usage.
    pub is_definition: bool,
}
