# compact-lsp

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![CI](https://github.com/1NickPappas/compact-lsp/actions/workflows/ci.yml/badge.svg)](https://github.com/1NickPappas/compact-lsp/actions/workflows/ci.yml)
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org)
[![crates.io](https://img.shields.io/crates/v/compact-lsp.svg)](https://crates.io/crates/compact-lsp)
[![Downloads](https://img.shields.io/crates/d/compact-lsp.svg)](https://crates.io/crates/compact-lsp)
[![docs.rs](https://docs.rs/compact-analyzer/badge.svg)](https://docs.rs/compact-analyzer)

This project extends the Midnight Network with additional developer tooling.

> ⚠️ **Note:** This is an experimental project developed in my personal free time for educational purposes — primarily to learn how Language Server Protocol implementations work. While functional, it is not officially supported. Feedback and contributions are welcome!

Language Server Protocol implementation for the [Compact](https://docs.midnight.network/compact/reference/compact-reference) smart contract language (Midnight network).

Targets **Compact toolchain 0.31.1 (language 0.23.0)** — the compiler pinned by Midnight Mainnet, Preprod, and Preview.

## Features

| Feature | Description |
|---------|-------------|
| **Diagnostics** | Real-time syntax errors + compiler errors on save |
| **Semantic Tokens** | Rich syntax highlighting (functions, types, parameters, etc.) |
| **Completion** | Keywords, types, snippets, local and imported symbols |
| **Hover** | Documentation for keywords, types, and symbols |
| **Go to Definition** | Jump to symbol definitions (local and imported) |
| **Find References** | Find all usages of a symbol (local and cross-file) |
| **Rename** | Rename symbols across the workspace |
| **Signature Help** | Parameter hints while typing function calls |
| **Document Symbols** | Outline view (circuits, structs, enums, modules) |
| **Formatting** | Code formatting via `format-compact` |
| **Folding Ranges** | Code folding for blocks and functions |
| **Cross-file Errors** | Errors propagate to dependent files on save |
| **Version-aware symbols** | Registry suggestions follow the detected compiler's language version |

### Cross-Project Support

Works with Compact's import system:

```compact
import "./Utils" prefix Utils_;

Utils_add(5, 5);  // Completion, hover, go-to-def, find refs, rename, signature help all work
```

### Missing Things

| Feature | Status |
|---------|--------|
| Code Actions | TODO |

## Requirements

- Rust toolchain (for building)
- Compact devtools (`compact`) with toolchain **0.31.1** — install with `compact update 0.31.1`.
  Provides `compactc` (diagnostics) and `format-compact` (formatting).

### Version support

The LSP reads the compiler's language version and **gates registry symbols** so one registry serves
both the mainnet compiler and newer toolchains:

| Toolchain | Language | Ledger | Deployable to public networks |
|---|---|---|---|
| **0.31.1** | **0.23.0** | 8 | ✅ Mainnet / Preprod / Preview (target) |
| 0.34.0 | 0.26.0 | 9 | ❌ |
| 0.35.0 | 0.27.0 | 9 | ❌ |

Symbols added after language 0.23.0 (e.g. `keccak256`, added in 0.23.103) are hidden when the
detected compiler rejects them. When no compiler is found, gating is disabled and all symbols are
shown. Upstream release notes: <https://docs.midnight.network/relnotes/compact>; full version map
and migration notes: [`MIGRATION_COMPILER_0.31.1.md`](./MIGRATION_COMPILER_0.31.1.md) and
[`CLAUDE.md`](./CLAUDE.md).

## Install

Install the language server from crates.io:

```bash
cargo install compact-lsp
```

Prebuilt binaries for Linux, macOS (Intel + Apple silicon), and Windows are on the
[releases page](https://github.com/1NickPappas/compact-lsp/releases).

Or build from source:

```bash
cargo build --release
# binary at target/release/compact-lsp
```

Two crates are published:

| Crate | What it is | Docs |
|---|---|---|
| [`compact-lsp`](https://crates.io/crates/compact-lsp) | the language server binary (`cargo install compact-lsp`) | — |
| [`compact-analyzer`](https://crates.io/crates/compact-analyzer) | reusable analysis library (diagnostics, formatter, parser) | [docs.rs](https://docs.rs/compact-analyzer) |

## Compiler Location

The LSP auto-detects the `compactc` compiler:
1. `COMPACT_COMPILER` environment variable
2. Compact devtools active toolchain `$COMPACT_DIRECTORY/bin/compactc` (default `~/.compact/bin/compactc`;
   the symlink `compact update <ver>` repoints) — resolved to the real `compactc.bin`
3. `~/compactc/compactc.bin`
4. `compactc.bin` / `compactc` in `PATH`

The detected language version gates registry symbols added after 0.23.0 (e.g. `keccak256`, which is
hidden on the mainnet 0.23.0 compiler). When no compiler is found, gating is disabled and all
symbols are shown. `format-compact` is found the same way (`COMPACT_FORMATTER`, then the devtools
`bin/`, then `PATH`).

## Development

```bash
cargo fmt --all --check
cargo clippy --all-targets --all-features -- -D warnings
cargo build --release
cargo test
```

GitHub Actions runs these on `main` and `dev`, plus a `compiler-probe` job that installs toolchain
0.31.1 and runs the registry probe tests (which skip themselves when no compiler is present).

- **Releasing** — see [`RELEASING.md`](./RELEASING.md); cut a release with `scripts/release.sh`.
- **Upstream tracking** — see [`UPSTREAM_RELEASES.md`](./UPSTREAM_RELEASES.md); the weekly
  `upstream-watch` workflow files an issue when a newer Compact toolchain ships.

## Related Projects

- [compact-tree-sitter](https://github.com/1NickPappas/compact-tree-sitter) - Tree-sitter grammar for Compact (published on crates.io as `tree-sitter-compact`)

## License

MIT
