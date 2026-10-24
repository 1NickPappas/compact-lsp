# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

A Language Server Protocol (LSP) implementation for [Compact](https://docs.midnight.network/develop/reference/compact/lang-ref), the smart contract language for the Midnight zero-knowledge blockchain. It speaks JSON-RPC over stdin/stdout and is consumed by editors (Neovim setup is documented in the README).

This is a self-contained git repository. The `CLAUDE.md` in the parent `midnight/lsp/` directory describes a different, broader monorepo and does not apply here.

**Supported: Compact language 0.23.0 – 0.27.0 (toolchains 0.31.1 – 0.35.0).** The public-network pin is **0.31.1 (language 0.23.0, runtime 0.16.0, ledger 8)**. Toolchains 0.34.0/0.35.0 target ledger 9 and are **not deployable to public networks yet**; their language features are registry-gated. See [`MIGRATION_COMPILER_0.31.1.md`](./MIGRATION_COMPILER_0.31.1.md).

## Upstream sources of truth (where to watch for changes)

- **Release notes**: <https://docs.midnight.network/relnotes/compact> (overview) and
  `https://docs.midnight.network/relnotes/compact/toolchain-<version>.md` (per version).
  Append `.md` to any docs.midnight.network path — the HTML pages are behind a Vercel
  security checkpoint and do not fetch reliably. Compatibility matrix:
  <https://docs.midnight.network/relnotes/support-matrix.md> (which toolchain the public
  networks pin). Docs index for LLMs: <https://docs.midnight.network/llms.txt>.
- **Compiler source — `LFDT-Minokawa/compact`** (<https://github.com/LFDT-Minokawa/compact>):
  `CHANGELOG.md` is the authoritative per-toolchain history (replaces the discontinued
  `CHANGELOG_compactc.md`); `compiler/standard-library.compact`, `doc/compact-reference.mdx`,
  and `doc/release-notes/toolchain-*.md` are the language/API references.
- **Public release artifacts — `midnightntwrk/compact`**
  (<https://github.com/midnightntwrk/compact/releases>).
- **Grammar — `tree-sitter-compact` on crates.io**, maintained at
  <https://github.com/1NickPappas/compact-tree-sitter> (the upstream
  `midnightntwrk/compact-tree-sitter` repo is archived). The LSP depends on the published
  crate version, not a git branch.

> **Gotcha:** compiler crate versions such as `0.31.103` (language `0.23.103`) are internal
> build versions, **not released toolchains**. Do not assume they are installable or
> mainnet-safe. Map: 0.31.x → language 0.23.0; 0.32 → 0.24; 0.33 → 0.25; 0.34 → 0.26
> (ledger 9); 0.35 → 0.27 (ledger 9).

## Build & Test Commands

```bash
cargo build --release          # Binary at target/release/compact-lsp
cargo test                     # Run all tests (110+ unit/integration tests)
cargo test <name>              # Run a single test by name substring
cargo fmt --all --check        # CI fails if not formatted
cargo clippy --all-targets --all-features -- -D warnings  # CI fails on any warning
```

CI (`.github/workflows/ci.yml`) runs fmt-check, clippy (warnings-as-errors), build, and test. Match all four locally before pushing. Edition 2024, `max_width = 100` (`rustfmt.toml`).

### Debugging

You cannot use `println!` — stdout is the LSP transport. Logging goes to stderr via `tracing`. Enable with `RUST_LOG=compact_lsp=debug` (or `RUST_LOG=debug`).

## Architecture

Cargo workspace with two crates. Shared dependency versions are pinned in the root `Cargo.toml` under `[workspace.dependencies]` and consumed with `.workspace = true`.

### `crates/compact-analyzer` (library)

The analysis layer, independent of the LSP protocol. Exposes three engines via `lib.rs`:

- **`DiagnosticEngine`** (`diagnostics.rs`) — wraps the external `compactc.bin` compiler as an async subprocess (`compactc.bin --vscode --skip-zk [feature flags] <file> <temp_dir>`), parsing stderr lines of the form `Exception: <file> line <l> char <c>: <msg>` into LSP `Diagnostic`s. Compiler is auto-detected in this order: `COMPACT_COMPILER` env var → Compact devtools active toolchain `$COMPACT_DIRECTORY/bin/compactc` (default `~/.compact/bin/compactc`, the symlink `compact update` repoints; **resolved through the wrapper to the real `compactc.bin`**, since the `bin/compactc` wrapper breaks when invoked via its symlink) → `~/compactc/compactc.bin` → `compactc.bin`/`compactc` on `PATH`. Degrades gracefully (no diagnostics) when absent. `version_info()` / `language_version()` report the detected compiler and language versions (logged on startup). `feature_flags()` adds `--feature-zkir-v3` when the compiler still advertises it (0.34.100 – 0.35.104), which the foreign-curve symbols require. Note: diagnostics carry only a message (no `code`/structured data) and a 1-char range.
- **`LanguageVersion`** (`version.rs`) — a parsed semantic version used for **version-aware registry gating** (see below).
- **`FormatterEngine`** (`formatter.rs`) — wraps the external `format-compact` binary.
- **`ParserEngine`** (`parser/mod.rs`, ~2700 lines) — wraps `tree-sitter-compact` (crates.io crate). This is the heart of every AST-driven feature: document symbols, folding, hover, go-to-definition, signature help, completion symbols, import extraction, syntax errors, semantic tokens, and find-references. Operations take `&mut self` (the tree-sitter `Parser` is mutable).

> **Grammar source:** `Cargo.toml` depends on the crates.io crate **`tree-sitter-compact`**
> (maintained at `1NickPappas/compact-tree-sitter`; the upstream `midnightntwrk` repo is
> archived). Version **0.3.x** parses the language **0.23.0 – 0.27.0** superset (toolchains
> 0.31.1 – 0.35.0), including `contract implements` and `emit(...)`. The parser accepts **both
> node-naming schemes**:
> `type_alias_declaration` | `type_decl`, and `cbinding` | `const_binding` (the published crate
> uses `type_decl`/`const_binding`; the old `feat/grammar-v0.30.0` branch used the others).
> To update: bump the version in `Cargo.toml`, run `cargo update -p tree-sitter-compact`, then
> `cargo test`. Deferred follow-up: hover/go-to-definition for type aliases and resolution of
> `import_selection` selective imports.

### `crates/compact-lsp` (binary)

`main.rs` sets up `tracing` (stderr) and runs the `tower-lsp` server over stdin/stdout. All logic lives in `server/`:

- **`mod.rs`** — `CompactLanguageServer` holds all engines (wrapped in `Arc`; `ParserEngine` in `Arc<Mutex<>>`) and several `DashMap` caches. It implements the `tower_lsp::LanguageServer` trait (`initialize`, `did_open/change/save/close`, `completion`, `hover`, `goto_definition`, `references`, `rename`, `signature_help`, `semantic_tokens_full`, `formatting`, `document_symbol`, `folding_range`, `symbol` (workspace symbols), etc.).
- **`state.rs`** — `Document` (content as a `ropey::Rope` for efficient incremental edits + version).
- **`utils.rs`** — text/position helpers (e.g. word-at-position).
- **`validation.rs`** — hardcoded keyword / built-in-type / identifier checks.
- **`workspace.rs`** — recursive `.compact` file discovery (skips hidden dirs, `node_modules`, `target`).
- **`imports.rs`** — resolves relative `import "./Foo"` paths to absolute `file://` URIs.

### Key cross-cutting behaviors

- **Two-tier diagnostics.** Fast tree-sitter syntax errors are published immediately; the slower `compactc` compiler pass is debounced (~500ms in `schedule_semantic_diagnostics`) and also runs on save. A new edit aborts the pending compiler task via the `pending_diagnostics` `JoinHandle` map.
- **Cross-file support.** On `initialize`, `scan_workspace` parses every `.compact` file to populate `symbol_cache` (completion + `workspace/symbol`) and `source_cache` (hover/definition across files); per-file updates happen on `did_open` and `did_save`. `reverse_dependencies` propagates compiler errors to dependent files on save. Import-prefixed symbols (`import "./Utils" prefix Utils_;`) resolve through `imports.rs`.
- **Cross-contract calls.** Contract types (`ecdecl`) are indexed in `symbol_cache` with kind `Contract`. `resolve_contract_type()` in `server/mod.rs` looks a type up in the current file, then imported/workspace files; it powers member completion/hover/definition/signature help for `ref.circuit(...)` and completion after `contract implements`. Parser support: `contract_type()`, `get_enclosing_member_call()`, and `get_variable_type()` — which resolves `parg` / `const_binding` / `ldecl` annotations, follows same-file `type X = Y` aliases (`resolve_type_name()` / `find_type_alias()`), and infers `const t = make();` from the callee's declared return type (`infer_call_return_type()` / `find_callable_return_type()`).
- **Data-driven registries.** `server/builtins.toml` (methods on built-in ledger types like `Counter`, `Map`, `Kernel`) and `server/stdlib.toml` (stdlib structs/circuits) are embedded at compile time (`include_str!`) and parsed once behind a `OnceLock` in `builtins.rs` / `stdlib.rs`. To add/change a built-in method or stdlib symbol, edit the TOML — not the Rust. The registries also synthesize markdown doc "files" so go-to-definition works on built-ins.
  - **Target: Compact language 0.23.0 – 0.27.0.** Source of truth is the LFDT-Minokawa Compact repo (the `official_compact` checkout / `minokawa` remote): `compiler/standard-library.compact` (snake_case fields, signatures), `doc/api/CompactStandardLibrary/exports.md` (full public symbols incl. compiler intrinsics), `doc/ledger-adt.mdx` (ledger ADT methods). The same `is_builtin_type` keyword/type list is duplicated in `server/validation.rs` and `parser/mod.rs` — keep them in lockstep.
  - **Version-aware gating.** Registry entries carry `min_language_version` and an exclusive `max_language_version` (the version in which a symbol was removed). At presentation time (completion/hover/definition/signature help) `stdlib.rs`/`builtins.rs` filter against the detected compiler's language version, so one registry serves mainnet (0.23.0) and newer toolchains. When **no compiler is detected the filter is disabled** (everything shows). Examples: `keccak256` (`min` 0.23.103; hidden on mainnet 0.23.0); `sha512` (0.26.103); `kernel.caller` / `PublicAddress` (0.26.105); `secp256r1EcdsaVerify` / `ed25519Verify` (0.26.106). Set `max_language_version = "0.26.0"` for symbols removed in 0.26+.
  - **Verify registry changes** by compiling a probe `.compact` contract with the detected compiler (unbound/wrong-signature symbols error before disclosure analysis, so a clean or disclosure-only compile validates the symbols). `cargo test` includes a probe test that skips itself when no compiler is found.

## Conventions

- License is **MIT** (see `LICENSE`, `Cargo.toml`). Source files carry an Apache-2.0 SPDX header; preserve the existing header style when editing a file.
- The LSP types crate is `ls-types` aliased to `lsp-types`, and `tower-lsp` is the `tower-lsp-server` community fork — import as `lsp_types::*` and `tower_lsp::*` respectively.
