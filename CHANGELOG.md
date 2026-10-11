# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2025-01-XX

### Added
- Initial release
- Language Server Protocol (LSP) support for Compact smart contract language
- **Completion**: Auto-complete for circuits, structs, enums, and built-in types
- **Hover**: Documentation on hover for keywords, types, and symbols
- **Go to Definition**: Navigate to symbol definitions
- **Find References**: Find all usages of a symbol
- **Rename**: Rename symbols across files
- **Signature Help**: Function parameter hints while typing
- **Document Symbols**: Outline view of file structure
- **Folding Ranges**: Code folding for blocks and functions
- **Semantic Tokens**: Rich syntax highlighting
- **Diagnostics**: Real-time syntax error detection via tree-sitter
- **Formatting**: Code formatting via `format-compact`
- Cross-file symbol resolution via imports
