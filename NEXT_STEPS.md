# NEXT_STEPS

Actionable backlog for `compact-lsp`, kept in sync with the Compact compiler/language and the `compact-tree-sitter` grammar.

Each remaining item is tagged **[Add] / [Update] / [Remove]** with the file to touch and the source of truth.

## Status

**Target language: 0.23.103** (latest LFDT-Minokawa source). Installed/installable compiler: 0.31.0 = language **0.23.0**. The registries are verified against the 0.23.0 compiler (probe-compiled) and otherwise track the 0.23.103 source.

Completed:
- ✅ **Parser node-kind correctness bugs** (`f062c71`) — `const_binding`→`const_stmt`, removed dead `let_binding`/`call_args` arms, fixed `rtype`→`type` return-type field; regression tests added.
- ✅ **Stdlib / keyword accuracy → language 0.23.103** (`907d36e`, `676c505`, `5a74533`, `19d8e47`) — snake_case fields; `Coin*`→`Shielded*`; `CurvePoint`→`JubjubPoint`; unshielded-token family + `UserAddress`; Kernel `mintShielded`/`mintUnshielded` + `balance*`/`claimUnshieldedCoinSpend`/`incUnshielded*`; keyword list (`type`/`new`/`from`/`slice`, dropped `while`/`let`/`public`/`private`); dropped stale `Void`/`Address`; added `keccak256`. Verified by compiling a probe contract with compactc 0.23.0.
- ✅ **Pre-existing fixes** — block-comment doc extraction (`ac0080b`), repo reformat + `rust-toolchain.toml` pin to 1.91.1 (`9bd2151`, `8a9f1e3`), `redundant_closure` fix (`d872a1f`).

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
- `compact-tree-sitter` — `grammar.js`, `src/node-types.json` (branch `main` vs `feat/grammar-v0.30.0`)

---

## Track A — Grammar lags language 0.23 (top priority)

The LSP pins `compact-tree-sitter` `branch = "main"` (`Cargo.toml:72`), currently `6693aff`. That grammar is **line-comments-only** and has **none** of the 0.23 node types — verified absent in `src/node-types.json`: `slice_term`, `type_alias_declaration`, `import_selection`, `cbinding`, `assign_expr`, `array_literal`, `spread_element`, `bytes_literal`.

**Impact:** every parser-driven feature (semantic tokens, document symbols, hover, folding, signature help, references, rename) silently misbehaves on real 0.23 contracts that use:
- `slice<n>(value, start)` (26 example files use it), `new type` aliases (9), `{a, b as c} from "…"` selective imports (15)
- block comments `/* */`, hex/octal/binary literals, single-quoted strings + escapes, `$`-prefixed identifiers, `[expr]` index access, multi-binding `const a = 1, b = 2;`, generic `for`-loop bounds

**Dependency:** a 0.23-capable grammar = `feat/grammar-v0.30.0`, which is **still unmerged** on `compact-tree-sitter`. Options:
- (a) wait for upstream to merge it to `main`, then bump the `Cargo.toml` pin;
- (b) pin the LSP to the `feat/grammar-v0.30.0` branch/rev now to support 0.23 syntax immediately;
- (c) help land the merge upstream.

**Parser migration** (`crates/compact-analyzer/src/parser/mod.rs`, plus `validation.rs`, completion in `server/mod.rs`), once on the new grammar:
- **[Remove]** all `edecl` (external circuit) handling — the rule is deleted in the new grammar (parser references it ~10×).
- **[Add]** node handling: `type_alias_declaration` (+ `type_name`), `import_selection`/`import_element`, `cbinding` (multi-binding const), `assign_expr` (assignment-as-expression), `assert_term`, `array_literal`/`bytes_literal`/`spread_element`, `slice_term`.
- **[Update]** lexical features: block comments (folding + highlight), hex/oct/bin number literals, single-quoted strings, `$`-identifiers, `[expr]` indexing, generic `for`-bounds.
- **[Update]** bump the `tree-sitter-compact` pin; re-run the `tree-sitter test` corpus and the LSP parser tests; re-run a probe compile.

---

## Track B — LSP feature roadmap (missing capabilities)

Mostly `crates/compact-lsp/src/server/mod.rs`. Priority order:

1. **[Add] Code actions** — quick fixes from compiler diagnostics (incl. `compact fixup` renamings). The only README-listed TODO; highest value.
2. **[Add] Workspace symbols** (`workspace/symbol`) — reuse the existing `symbol_cache`.
3. **[Add] Incremental workspace re-scan** on change — caches are populated only on `initialize`.
4. **[Add]** Inlay hints (param names / inferred types), call hierarchy, range formatting (`format-compact` is whole-document only).
5. **[Add] Surface the detected compiler/language version** (log + status) — now especially useful given the 0.23.0-vs-0.23.103 drift; lets users see when the LSP's registry is ahead of/behind their compiler.

---

## Track C — Smaller refinements

- **[Update] `Cell` type completion** — `server/mod.rs` offers `Cell` as a type, but `Cell<T>` is **not** a valid ledger-field type (a single-value field is declared with the bare type, e.g. `ledger flag: Boolean;`, and *becomes* a Cell). Verified via the 0.23 compiler (`unbound identifier Cell`). Drop `Cell` from the type-completion/snippet list (keep its ledger-ADT methods in `builtins.toml`), or relabel it so it isn't suggested as a writable type.
- **[Update] Consolidate duplicate `is_builtin_type`** — the same list exists in `crates/compact-lsp/src/server/validation.rs` and `crates/compact-analyzer/src/parser/mod.rs:1810`. They must be kept in lockstep (both were updated for 0.23); consider a single shared source to prevent future drift.
- **[Revisit] `keccak256`** — once a 0.23.10x compiler is installable (`compact update`), re-run the probe to confirm and point `COMPACT_COMPILER` at it; until then the LSP is intentionally ahead of the released compiler on this one symbol.
- **[Add] (optional) probe-compile dev check** — a small script/test that compiles a registry-coverage `.compact` contract with the detected compiler, to catch future stdlib drift automatically.

---

## Verification

- Code gates (repo root, no `cd`): `cargo build`, `cargo test --all-features`, `cargo clippy --all-targets --all-features -- -D warnings`, `cargo fmt --all --check`.
- **Track A**: validate against the `tree-sitter test` corpus and LSP parser tests after the grammar bump; open a 0.23 contract using `slice`/type aliases/selective imports/block comments and confirm semantic tokens, symbols, hover, and folding are correct.
- **Track B/C**: feature-specific manual checks in an editor; for `Cell`, confirm it no longer autocompletes as a type.
