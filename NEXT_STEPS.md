# NEXT_STEPS

Actionable backlog for `compact-lsp`, kept in sync with the Compact compiler/language and the `compact-tree-sitter` grammar.

Each remaining item is tagged **[Add] / [Update] / [Remove]** with the file to touch and the source of truth.

## Status

**Target language: 0.23.103** (latest LFDT-Minokawa source). Installed/installable compiler: 0.31.0 = language **0.23.0**. The registries are verified against the 0.23.0 compiler (probe-compiled) and otherwise track the 0.23.103 source.

Completed:
- ✅ **Parser node-kind correctness bugs** (`f062c71`) — `const_binding`→`const_stmt`, removed dead `let_binding`/`call_args` arms, fixed `rtype`→`type` return-type field; regression tests added.
- ✅ **Stdlib / keyword accuracy → language 0.23.103** (`907d36e`, `676c505`, `5a74533`, `19d8e47`) — snake_case fields; `Coin*`→`Shielded*`; `CurvePoint`→`JubjubPoint`; unshielded-token family + `UserAddress`; Kernel `mintShielded`/`mintUnshielded` + `balance*`/`claimUnshieldedCoinSpend`/`incUnshielded*`; keyword list (`type`/`new`/`from`/`slice`, dropped `while`/`let`/`public`/`private`); dropped stale `Void`/`Address`; added `keccak256`. Verified by compiling a probe contract with compactc 0.23.0.
- ✅ **Pre-existing fixes** — block-comment doc extraction (`ac0080b`), repo reformat + `rust-toolchain.toml` pin to 1.91.1 (`9bd2151`, `8a9f1e3`), `redundant_closure` fix (`d872a1f`).
- ✅ **Track A — 0.23-capable grammar** (`683627f`) — pinned `tree-sitter-compact` to `feat/grammar-v0.30.0`; migrated the parser (`const_stmt`→`cbinding`, removed `edecl`, type-alias in outline); 0.23 parse smoke test. **Temporary pin** — revert to `branch = "main"` once that branch merges upstream (see `CLAUDE.md`).
- ✅ **Track B Phase 1** (`a23f9c8`) — `workspace/symbol` (filters `symbol_cache`), index caches on `did_open`, surface compiler/language version on startup.
- ✅ **Track C fast wins** (`216d700`) — dropped `Cell` from type completion (not a writable type); consolidated the duplicated `is_builtin_type` into a single source in `compact-analyzer`.

## Background

- **Installed compiler** (via the `compact` version manager): toolchain 0.31.0 = language **0.23.0**. The latest *source* is **0.23.103** (LFDT-Minokawa `main`), ahead of the public release channel — so `keccak256` is in the registry but the installed 0.23.0 compiler rejects it until a 0.23.10x toolchain is released.
- **Point the LSP at the right compiler** with `COMPACT_COMPILER=$HOME/.compact/versions/0.31.0/aarch64-darwin/compactc.bin` (detection order is `COMPACT_COMPILER` → `~/compactc/compactc.bin` → PATH).
- **Verification method now available**: compile a probe `.compact` contract that exercises stdlib structs/fields, circuits, ledger ADT and kernel methods with the installed compiler; unbound/wrong-signature symbols error before disclosure analysis, so a clean (or disclosure-only-error) compile validates the registry.

## Source-of-truth references

- `official_compact` checkout @ language 0.23.103 (`minokawa` remote = `github.com/LFDT-Minokawa/compact`, `main`):
  - `compiler/standard-library.compact` — stdlib structs/circuits (snake_case fields, signatures)
  - `doc/api/CompactStandardLibrary/exports.md` — full public symbol set incl. compiler intrinsics
  - `doc/ledger-adt.mdx` — ledger ADT (Kernel/Cell/Counter/Set/Map/List/MerkleTree) methods
  - `CHANGELOG_compactc.md` — detailed per-version language/stdlib history
- `compact-tree-sitter` — `grammar.js`, `src/node-types.json`. The LSP currently pins the `feat/grammar-v0.30.0` branch (0.23-capable); `main` is still the older line-comments-only grammar until the branch merges.

---

## Track A — 0.23 grammar ✅ done (`683627f`)

The LSP now pins `compact-tree-sitter` `feat/grammar-v0.30.0` (`Cargo.toml:72`), which parses all 0.23 syntax (`slice`, `new type` aliases, selective imports, block comments, hex/oct/bin literals, `$`-identifiers, multi-binding `const`). Parser migrated (`const_stmt`→`cbinding`, `edecl` removed, type-alias added to the outline) with a parse smoke test.

Remaining follow-ups:
- **[Revert when merged]** when `feat/grammar-v0.30.0` lands on `compact-tree-sitter` `main`, switch the `Cargo.toml` pin back to `branch = "main"` and drop the `CLAUDE.md` temporary-pin note.
- **[Add] deeper new-construct coverage** (deferred): hover + go-to-definition for `type_alias_declaration`; resolution of `import_selection`/`import_element` (selective imports) in imports extraction / goto / completion. Optional: semantic-token handling for `slice_term`, `assign_expr`, `assert_term`, array/bytes/spread literals.

---

## Track B — LSP feature roadmap (missing capabilities)

Mostly `crates/compact-lsp/src/server/mod.rs`.

**Phase 1 ✅ done** (`a23f9c8`): workspace symbols (`workspace/symbol`, filters `symbol_cache`), cache indexing on `did_open` (was save/initial-scan only), and compiler/language version surfacing on startup (`DiagnosticEngine::version_info`).

Remaining:
1. **[Add] Code actions (Phase 2)** — primary: a "fix up & format file" source action via `fixup-compact` (whole-file). Quick-fixes from individual diagnostics are limited because compiler `Diagnostic`s carry **no `code`/structured data** (message text only) — would first need to enrich `Diagnostic.code`/`data` in `diagnostics.rs` for precise matching. The only README-listed TODO.
2. **[Add] Inlay hints (Phase 3)** — parameter-name hints at call sites (reuse signature/param info); optionally inferred-type hints.
3. **[Add] Range formatting (Phase 3)** — limited: `format-compact` is whole-document only, so either defer or implement a clearly-labeled whole-doc fallback.
4. **[Add] Call hierarchy (Phase 3)** — `prepare_call_hierarchy`/`incoming_calls`/`outgoing_calls`; needs caller/callee graph analysis the parser doesn't expose yet (the biggest effort).

---

## Track C — Smaller refinements

- ✅ **`Cell` type completion** (`216d700`) — dropped `Cell`/`Cell<>` from the type-completion/snippet lists; it's not a writable type (a single-value field uses the bare type and *becomes* a Cell). Cell's ledger-ADT methods in `builtins.toml` are kept.
- ✅ **Consolidate duplicate `is_builtin_type`** (`216d700`) — single source in `compact-analyzer` (`pub fn`, re-exported from `lib.rs`); `compact-lsp/src/server/validation.rs` re-exports it.
- **[Revisit] `keccak256`** — once a 0.23.10x compiler is installable (`compact update`), re-run the probe to confirm and point `COMPACT_COMPILER` at it; until then the LSP is intentionally ahead of the released compiler on this one symbol.
- **[Add] (optional) probe-compile dev check** — a small script/test that compiles a registry-coverage `.compact` contract with the detected compiler, to catch future stdlib drift automatically.

---

## Verification

- Code gates (repo root, no `cd`): `cargo build`, `cargo test --all-features`, `cargo clippy --all-targets --all-features -- -D warnings`, `cargo fmt --all --check`.
- **Track B/C (remaining)**: feature-specific manual checks in an editor; e.g. for code actions, confirm the "fix up & format" action appears and rewrites the file.
