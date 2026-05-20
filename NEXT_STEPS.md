# NEXT_STEPS

Actionable backlog from a cross-repo audit of `compact-lsp` against the upstream tree-sitter grammar (`compact-tree-sitter`) and the official compiler/spec (`official_compact`).

Each item is tagged **[Add] / [Update] / [Remove]** with the file to touch and the source of truth.

## Background & findings

- **Pinned grammar** — `tree-sitter-compact` is pinned to `branch = "main"` (HEAD `6693aff`). The recent `65b1399..6693aff` bump was CI/deps only; the grammar is structurally unchanged.
- **`feat/grammar-v0.30.0`** — a large *future* language revision that is **not yet merged to `main`**. Verified: none of its node types (`cbinding`, `assign_expr`, `type_alias_declaration`, `import_selection`, `slice_term`, `assert_term`, `array_literal`) exist in the current `src/node-types.json`. Track 3 is preparation, not immediate work.
- **Installed compiler** the LSP auto-detects (`~/compactc/compactc.bin`): **compiler 0.27.0 / language 0.19.0**.
- **Authoritative stdlib source**: `official_compact/compiler/standard-library.compact` (language **0.21.101**). The installed compiler ships binaries only — no stdlib `.compact` source on disk, so the registries must be transcribed/generated from this file.
- **Our registries are ~language 0.18-era**, behind even the installed 0.19.0 compiler. Relevant rename history (from `official_compact/CHANGELOG.md`):
  - `Coin*`→`Shielded*` rename + unshielded-token family: **lang 0.18.103**
  - `CurvePoint`→`NativePoint`: **lang 0.19.102**
  - `NativePoint`→`JubjubPoint` (`new type` alias of `Opaque<'JubjubPoint'>`): **lang 0.21.x**

**Target for this work: language 0.21.x.** Source of truth = `official_compact/compiler/standard-library.compact`.

## Source-of-truth references

- `official_compact/compiler/standard-library.compact` — stdlib structs/circuits/type aliases
- `official_compact/doc/api/CompactStandardLibrary` — ledger ADT method docs
- `official_compact/CHANGELOG.md` — version/rename history
- `compact-tree-sitter/grammar.js`, `compact-tree-sitter/src/node-types.json` — grammar node kinds
- `compact-tree-sitter` branch `feat/grammar-v0.30.0` — upcoming grammar

---

## Track 1 — Parser correctness bugs (current pinned grammar)

File: `crates/compact-analyzer/src/parser/mod.rs`. Verify each node-kind string against `compact-tree-sitter/src/node-types.json`.

These are real bugs **today** — the parser matches node kinds the grammar never emits, so the dependent features silently no-op.

- **[Update]** `const_binding` → `const_stmt`. The grammar emits `const_stmt` (with a `pattern`/`id` child), not `const_binding`. Currently breaks const-declaration handling, including the `READONLY`/declaration semantic-token modifier (~`mod.rs:1462`). Read the `pattern`/`id` inside `const_stmt`.
- **[Remove]** `let_binding` match — the `main` grammar has no `let` construct. Dead arm; confirm no feature relies on it.
- **[Remove]** `call_args` arm — the grammar emits `arguments` (already matched alongside it at ~`mod.rs:923`); `call_args` never matches.
- **[Update]** Exhaustive audit — enumerate every node-kind string literal referenced in `parser/mod.rs` and assert each exists in `node-types.json`. Fix or delete every mismatch. Also verify the less-obvious ones (`rtype`, `no_prefix_file`) are real node kinds and not stale.
- **[Add]** Regression tests in the `#[cfg(test)] mod tests` block: parse representative snippets and assert the produced node kinds and the resulting symbols / semantic tokens (esp. const declarations). They should fail before the fixes and pass after.

---

## Track 2 — Stdlib / keyword accuracy (target lang 0.21.x)

### `crates/compact-lsp/src/server/stdlib.toml`

- **[Update] renames**
  - `CoinInfo` → `ShieldedCoinInfo`
  - `QualifiedCoinInfo` → `QualifiedShieldedCoinInfo`
  - `SendResult` → `ShieldedSendResult`
  - `CurvePoint` → `JubjubPoint` (now a `new type` alias of `Opaque<'JubjubPoint'>`)
  - `send` → `sendShielded`, `receive` → `receiveShielded`, `sendImmediate` → `sendImmediateShielded`
  - `mintToken` → `mintShieldedToken`, `burnAddress` → `shieldedBurnAddress`
  - `evolveNonce` index arg `Uint<64>` → `Uint<128>`
- **[Add]**
  - struct `UserAddress { bytes: Bytes<32> }`
  - type alias `JubjubPoint`
  - circuits: `mintUnshieldedToken`, `sendUnshielded`, `receiveUnshielded`, `unshieldedBalance`, `unshieldedBalanceLt`, `unshieldedBalanceGte`, `unshieldedBalanceGt`, `unshieldedBalanceLte`
- **[Remove / verify]** Confirm `ownPublicKey`, `createZswapInput`, `createZswapOutput`, `ecAdd`, `ecMul`, `ecMulGenerator`, `hashToCurve` still exist in `standard-library.compact` and that EC signatures use `JubjubPoint`; drop anything no longer present.
- **Recommended approach**: regenerate `stdlib.toml` from `standard-library.compact` rather than hand-patching, to eliminate drift in one pass.

### `crates/compact-lsp/src/server/builtins.toml` (ledger ADT methods)

Source: `official_compact/doc/api/CompactStandardLibrary` (ledger ADTs).

- **[Update]** coin-param types `CoinInfo` → `ShieldedCoinInfo` on `Cell.writeCoin`, `Map.insertCoin`, `Set.insertCoin`, `List.pushFrontCoin`.
- **[Update]** Kernel `mint` → split into `mintShielded` and `mintUnshielded`.
- **[Add]** Kernel methods: `balance`, `balanceGreaterThan`, `balanceLessThan`, `claimUnshieldedCoinSpend`, `incUnshieldedInputs`, `incUnshieldedOutputs`.

### `crates/compact-lsp/src/server/validation.rs`

- **[Remove]** `while`, `let`, `public`, `private` from `is_keyword` (not active Compact keywords).
- **[Add]** `type`, `new`, `from`, `slice` to `is_keyword`.
- **[Update]** `is_builtin_type`: apply the Track 2 renames and add `UserAddress`; ensure every stdlib struct name is covered.

### `crates/compact-lsp/src/server/mod.rs` (completion handler)

- **[Update]** keyword and type completions/snippets to match the renamed/added names (add `type`/`new`/`from`; add `ShieldedCoinInfo`/`UserAddress`/`JubjubPoint` snippets; drop stale names).
- **[Add]** a drift-guard test asserting every name in `stdlib.toml` exists in `standard-library.compact`.

---

## Track 3 — v0.30.0 grammar migration prep

Staged work for **when `feat/grammar-v0.30.0` merges to `main`**. Do **not** switch the dependency pin until then. Files: `Cargo.toml`, `crates/compact-analyzer/src/parser/mod.rs`, `validation.rs`, completion in `mod.rs`.

- **[Remove]** all `edecl` (external circuit declaration) handling — the rule is deleted in 0.30. The parser currently references `edecl` ~10×; these become dead on migration.
- **[Add]** node handling for new constructs:
  - `type_alias_declaration` (+ `type_name`) — `type [new] Name [<gparams>] = Type;`
  - `import_selection` / `import_element` — selective imports `import { a, b as c } from "…";`
  - `cbinding` — multi-binding const (`const a = 1, b = 2;`)
  - `assign_expr` — assignment is now an expression, not a statement
  - `assert_term` — `assert(expr, "msg")` as a term
  - `array_literal`, `bytes_literal`, `spread_element` — `[a, ...b]`, `Bytes[...]`
  - `slice_term` — `slice<size>(value, index)`
- **[Update]** lexical features: block comments `/* */` (folding + highlight), hex/octal/binary number literals, single-quoted strings + escapes, `$`-prefixed identifiers, `[expr]` index access (was `[nat]`), `tsize..tsize` for-ranges.
- **[Update]** bump the `tree-sitter-compact` pin in `Cargo.toml`; re-run the full parser test corpus against the new grammar.

---

## Track 4 — LSP feature roadmap (missing capabilities)

Mostly `crates/compact-lsp/src/server/mod.rs`. Priority order:

1. **[Add] Code actions** — quick fixes derived from compiler diagnostics (including the `compact fixup` renamings). Currently the only README-listed TODO; highest user value.
2. **[Add] Workspace symbols** (`workspace/symbol`) — reuse the existing `symbol_cache`.
3. **[Add] Incremental workspace re-scan** on change — the symbol/source caches are currently populated only on `initialize`.
4. **[Add]** Inlay hints (parameter names / inferred types), call hierarchy, and range formatting (`format-compact` is whole-document only today).
5. **[Add]** Surface the detected compiler/language version (log + capability) so version drift between the LSP's registries and the active compiler is visible.

---

## Verification

For the code work above (not for this document):

- Gates (run from repo root): `cargo build`, `cargo test`, `cargo clippy --all-targets -- -D warnings`, `cargo fmt --check`.
- **Track 1**: parser unit tests asserting expected node kinds and const-declaration semantic tokens; confirm fail-before / pass-after.
- **Track 2**: open a `.compact` file exercising `ShieldedCoinInfo`, `sendShielded`, `unshieldedBalance`, and `JubjubPoint`; verify hover, completion, signature help, and go-to-definition resolve. Run the new drift-guard test.
- **Track 3**: gated behind the grammar merge — validate against the `tree-sitter test` corpus and LSP parse tests.
