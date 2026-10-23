# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Grammar-schema validation test** — the parser's node-kind and field references are checked
  against the linked `tree-sitter-compact` schema, so a grammar rename/removal fails `cargo test`
  instead of silently degrading features.
- **Automated crates.io publishing** (`publish.yml`) — a `v*` tag verifies the workspace, then
  publishes `compact-analyzer` followed by `compact-lsp`.

### Changed
- Depend on the published crates.io crate **`tree-sitter-compact`** (0.2.0) instead of a git branch
  on the archived `midnightntwrk/compact-tree-sitter` repository.
- README: removed the Neovim setup section; Related Projects lists only the maintained
  tree-sitter grammar.

## [0.2.0] - 2026-10-22

### Added
- **Version-aware registry gating** — a `LanguageVersion` type and an optional
  `min_language_version` on stdlib/builtins entries. Symbols added after language 0.23.0
  (e.g. `keccak256`, added in 0.23.103) are hidden when the detected compiler rejects them.
- **Compact devtools toolchain discovery** — the LSP now resolves the active toolchain
  (`$COMPACT_DIRECTORY/bin/compactc` → real `compactc.bin`) for both the compiler and formatter.
- `compiler-probe` CI job that installs toolchain 0.31.1 and runs the registry probe tests.

### Changed
- Target **Compact toolchain 0.31.1 (language 0.23.0)** — the version pinned by
  Mainnet/Preprod/Preview. Toolchains 0.34/0.35 (ledger 9) are not deployable to public networks.
- Startup now logs compiler, language, ledger, and runtime versions.
- CI runs on `dev` as well as `main`.

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
