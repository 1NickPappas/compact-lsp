# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

A Language Server Protocol (LSP) implementation for [Compact](https://docs.midnight.network/develop/reference/compact/lang-ref), the smart contract language for the Midnight zero-knowledge blockchain. It speaks JSON-RPC over stdin/stdout and is consumed by editors (Neovim setup is documented in the README).

This is a self-contained git repository. The `CLAUDE.md` in the parent `midnight/lsp/` directory describes a different, broader monorepo and does not apply here.

## Build & Test Commands

```bash
cargo build --release          # Binary at target/release/compact-lsp
cargo test                     # Run all tests (105+ unit/integration tests)
cargo test <name>              # Run a single test by name substring
cargo fmt --all --check        # CI fails if not formatted
cargo clippy --all-targets --all-features -- -D warnings  # CI fails on any warning
```

CI (`.github/workflows/ci.yml`) runs fmt-check, clippy (warnings-as-errors), build, and test. Match all four locally before pushing. Edition 2021, `max_width = 100` (`rustfmt.toml`).

### Debugging

You cannot use `println!` — stdout is the LSP transport. Logging goes to stderr via `tracing`. Enable with `RUST_LOG=compact_lsp=debug` (or `RUST_LOG=debug`).

## Architecture

Cargo workspace with two crates. Shared dependency versions are pinned in the root `Cargo.toml` under `[workspace.dependencies]` and consumed with `.workspace = true`.

### `crates/compact-analyzer` (library)

The analysis layer, independent of the LSP protocol. Exposes three engines via `lib.rs`:

- **`DiagnosticEngine`** (`diagnostics.rs`) — wraps the external `compactc.bin` compiler as an async subprocess (`compactc.bin --vscode <file> <temp_dir>`), parsing stderr lines of the form `Exception: <file> line <l> char <c>: <msg>` into LSP `Diagnostic`s. Compiler is auto-detected in this order: `COMPACT_COMPILER` env var → `~/compactc/compactc.bin` → `compactc.bin` on `PATH`. Degrades gracefully (no diagnostics) when absent.
- **`FormatterEngine`** (`formatter.rs`) — wraps the external `format-compact` binary.
- **`ParserEngine`** (`parser/mod.rs`, ~2700 lines) — wraps `tree-sitter-compact` (pulled from git). This is the heart of every AST-driven feature: document symbols, folding, hover, go-to-definition, signature help, completion symbols, import extraction, syntax errors, semantic tokens, and find-references. Operations take `&mut self` (the tree-sitter `Parser` is mutable).

### `crates/compact-lsp` (binary)

`main.rs` sets up `tracing` (stderr) and runs the `tower-lsp` server over stdin/stdout. All logic lives in `server/`:

- **`mod.rs`** — `CompactLanguageServer` holds all engines (wrapped in `Arc`; `ParserEngine` in `Arc<Mutex<>>`) and several `DashMap` caches. It implements the `tower_lsp::LanguageServer` trait (`initialize`, `did_open/change/save/close`, `completion`, `hover`, `goto_definition`, `references`, `rename`, `signature_help`, `semantic_tokens_full`, `formatting`, `document_symbol`, `folding_range`, etc.).
- **`state.rs`** — `Document` (content as a `ropey::Rope` for efficient incremental edits + version).
- **`utils.rs`** — text/position helpers (e.g. word-at-position).
- **`validation.rs`** — hardcoded keyword / built-in-type / identifier checks.
- **`workspace.rs`** — recursive `.compact` file discovery (skips hidden dirs, `node_modules`, `target`).
- **`imports.rs`** — resolves relative `import "./Foo"` paths to absolute `file://` URIs.

### Key cross-cutting behaviors

- **Two-tier diagnostics.** Fast tree-sitter syntax errors are published immediately; the slower `compactc` compiler pass is debounced (~500ms in `schedule_semantic_diagnostics`) and also runs on save. A new edit aborts the pending compiler task via the `pending_diagnostics` `JoinHandle` map.
- **Cross-file support.** On `initialize`, `scan_workspace` parses every `.compact` file to populate `symbol_cache` (for completion) and `source_cache` (for hover/definition across files). `reverse_dependencies` propagates compiler errors to dependent files on save. Import-prefixed symbols (`import "./Utils" prefix Utils_;`) resolve through `imports.rs`.
- **Data-driven registries.** `server/builtins.toml` (methods on built-in types like `Counter`, `Map`) and `server/stdlib.toml` (stdlib structs/circuits) are embedded at compile time (`include_str!`) and parsed once behind a `OnceLock` in `builtins.rs` / `stdlib.rs`. To add/change a built-in method or stdlib symbol, edit the TOML — not the Rust. The registries also synthesize markdown doc "files" so go-to-definition works on built-ins.

## Conventions

- License is **MIT** (see `LICENSE`, `Cargo.toml`). Source files carry an Apache-2.0 SPDX header; preserve the existing header style when editing a file.
- The LSP types crate is `ls-types` aliased to `lsp-types`, and `tower-lsp` is the `tower-lsp-server` community fork — import as `lsp_types::*` and `tower_lsp::*` respectively.
