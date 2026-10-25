# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.5.0] - 2026-10-25

### Added
- **Method-call bases** for contract references: `factory.create().transfer(...)` resolves the
  receiver's contract type and reads the called circuit's declared return type, and
  `tokens.lookup(k).transfer(...)` resolves a built-in method's return type from the registry with
  the receiver's generic arguments substituted (`Map<K, V>` → `V`). All member features and
  find-references/rename are covered.
- **Field access on a method result**: `items.head().value.transfer(...)` resolves — the method's
  return type is used as the container type and the field type is read from the workspace struct or
  the standard-library registry (`Maybe<T>.value` → `T`, `Either<A, B>.left` → `A`), with the
  receiver's generic arguments substituted.
- **Generic type-parameter instantiation**: a generic circuit's return type is instantiated from the
  argument types at the call site (`circuit id<T>(x: T): T` called as `id(token)` resolves to
  `token`'s type), and generic struct fields substitute the receiver's arguments (`Box<Token>.value`
  → `Token`).
- **Find References and Rename for struct fields and enum variants** (`w.balance`, `Phase.day`),
  gated by the owner type (including imported structs/enums) so unrelated members that share a name
  are never touched.
- **Range (selection) formatting**: `textDocument/rangeFormatting` is now supported. Since
  `format-compact` only formats whole files, the document is formatted in full and a line-level diff
  is filtered to the changes intersecting the selected range, so unselected code is untouched.
- **Explicit call-site type arguments**: generic calls written as `id<Uint<64>>(x)` (the form
  Compact *requires* — it does not infer generic arguments) now bind the type parameters directly,
  covering parameters that argument inference cannot bind: nested (`first<T>(m: Maybe<T>)`) and
  return-only (`none<T>()`). Argument-based inference is kept as a permissive superset.
- **Selective-import binding**: `import { Token, Wallet as W } from "./mod"` files are parsed into
  their elements, so imported symbols (including aliases) resolve explicitly to the imported file
  rather than via the ambiguous by-name workspace scan, and completion offers exactly the selected
  names.

## [0.4.0] - 2026-10-24

### Added
- Improved contract-reference typing: same-file `type` aliases are followed (including chains,
  e.g. `type A = B; type B = Token;`) and `const t = make();` infers `t`'s type from the callee's
  declared return type. Member completion/hover/definition/signature help now fire on contract
  types reached through an alias or an inferred binding.
- **Cross-file callee inference**: `const t = F_make();` where `make` is defined in another file
  now infers `t`'s contract type from the callee's declared return type, resolved in the callee's
  own file scope (type names are file-scoped).
- **Find References and Rename for contract-type circuits**: triggered on a circuit declaration or
  a `ref.circuit(...)` use and gated by the base's resolved contract type, so unrelated `.circuit`
  members are never touched. Works across files, tolerates canonical/non-canonical file URIs.
- **Member-chain bases**: a contract reference reached through struct fields
  (`w.token.transfer(...)`, nested `a.wallet.token.transfer(...)`, and structs imported from
  another file) now resolves for completion, hover, definition, signature help,
  find-references, and rename.
- **Expression bases** for contract references: `make().circuit(...)` resolves via the callee's
  declared return type and `items[i].circuit(...)` via the container's element type
  (`Map<K, V>` → `V`, `Vector<n, T>` → `T`, `List<T>` → `T`, …). All member features and
  find-references/rename are covered.

## [0.3.0] - 2026-10-24

### Added
- **Cross-contract-call intelligence** (Compact 0.26+):
  - contract types (`contract Token { … }`) are indexed for completion and workspace symbols;
  - `contract implements Token;` offers contract types and resolves hover/go-to-definition;
  - a contract reference (`ref: Token`) gets member completion, hover, go-to-definition, and
    signature help for `ref.circuit(...)`, including contract types imported from other files.
- Member accesses are emitted as method/property semantic tokens.
- End-to-end LSP integration tests (`crates/compact-lsp/tests/ccc_integration.rs`) covering
  same-file and **cross-file** contract types: unprefixed imports, `prefix` imports, and a
  defining file that is not open (resolved via the workspace scan).
- **Compact 0.34/0.35 support** (language 0.26/0.27): registry entries for `PublicAddress`,
  `kernel.caller()`, `sha512`, `jubjubSchnorrVerify`, `secp256r1EcdsaVerify`, `ed25519Verify`,
  the secp256r1/Curve25519 field and point types, and their signature structs. All are
  version-gated.
- Hover docs, keyword completion, snippets, and semantic tokens for `contract implements` and
  `emit(...)`.
- Automatic `--feature-zkir-v3` detection on toolchains where the ZKIR-v3 backend is opt-in
  (0.34.100–0.35.104), required for the foreign-curve symbols.
- CI compiler-probe matrix over toolchains 0.31.1, 0.34.0, and 0.35.0.

### Changed
- Bumped the grammar to `tree-sitter-compact` 0.3.0 (language 0.23.0–0.27.0).
- Registry gating now supports an exclusive `max_language_version` alongside
  `min_language_version`, so symbols removed in later language versions can be hidden.
- `emit` and `implements` are treated as keywords (the 0.35 compiler reserves them), and
  semantic tokens gained a `keyword` type.

## [0.2.3] - 2026-10-24

### Changed
- **Rust toolchain / edition:** upgraded CI and the pinned toolchain to Rust **1.99.1**, migrated
  the workspace to **edition 2024**, enabled the MSRV-aware `resolver = "3"`, and corrected the
  declared MSRV (`rust-version`) from `1.70` to **`1.90`** (the real floor, set by
  `tree-sitter` 0.27). `cargo fix --edition` + `cargo clippy --fix` modernized nested `if`/`if let`
  blocks into let-chains.

## [0.2.2] - 2026-10-24

### Added
- README: crates.io/downloads badges and an **Install** section documenting
  `cargo install compact-lsp`, prebuilt binaries, and the two published crates.

### Changed
- Updated all direct dependencies to their latest stable versions: `tree-sitter` 0.25 → 0.27,
  `toml` 0.8 → 1, `ls-types` 0.0.2 → 0.0.6, plus the latest `tokio`, `serde`, `serde_json`,
  `dashmap`, `tracing`, `regex`, `tempfile`, and `anyhow`. Removed the unused `thiserror`
  dependency.
- Set the new `offset_encoding` field on `InitializeResult` (added in `ls-types` 0.0.6) to `None`
  to keep the client's default UTF-16 positions.

## [0.2.1] - 2026-10-23

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
