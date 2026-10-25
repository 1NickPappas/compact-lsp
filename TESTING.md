# Testing

Three layers, all run by `cargo test`:

| Layer | Where | Needs a toolchain? |
|---|---|---|
| Unit | `src/**/tests.rs` and embedded `mod tests` | no |
| End-to-end | `crates/compact-lsp/tests/*.rs` (spawn the real binary over stdio) | mostly no |
| Compiler-backed | `compiler_fixtures.rs`, `compiler_diagnostics.rs`, `compiler_edit_safety.rs`, `compiler_version_gating.rs`, analyzer `probe` tests | yes |

Fixture projects live in `crates/compact-lsp/tests/fixtures/` and are
calibrated against toolchains 0.31.1 / 0.34.0 / 0.35.0; the expected outcomes
are recorded in `fixtures/expected.toml`.

## Running locally

```bash
cargo test                      # everything (compiler tests skip without a toolchain)
cargo test -p compact-lsp --test lsp_lifecycle
```

Compiler-backed tests detect a compiler the same way the server does
(`COMPACT_COMPILER`, devtools, `~/compactc`, `PATH`). To run them against a
specific toolchain:

```bash
COMPACT_COMPILER=~/.compact/versions/0.35.0/<target>/compactc.bin \
COMPACT_FORMATTER=~/.compact/versions/0.35.0/<target>/format-compact \
cargo test --all-features
```

CI sets `COMPACT_LSP_REQUIRE_COMPILER=1` and `COMPACT_LSP_REQUIRE_FORMATTER=1`
in the compiler-probe matrix, which turns a missing toolchain into a failure
instead of a silent skip.

## Snapshots

`lsp_snapshots.rs` uses [insta](https://insta.rs). Review intentional output
changes with `cargo insta review` (or `INSTA_UPDATE=always cargo test -p
compact-lsp --test lsp_snapshots`). Compiler diagnostics are not snapshotted
(their wording varies per toolchain); they are asserted structurally.

## Known issues

Failing behaviour is pinned as `#[ignore]`d acceptance tests so it cannot be
forgotten and so the fix has a ready-made target. Run them with
`cargo test -- --ignored` (they are expected to fail until fixed).

| Issue | Pinned by |
|---|---|
| **UTF-16 ↔ byte positions.** tree-sitter reports byte columns while LSP positions are UTF-16 code units, so any line where non-ASCII precedes code gets wrong hover/definition/reference/rename/semantic-token ranges, and `didChange` after an astral character is off by one character. | `crates/compact-lsp/tests/unicode_positions.rs` (4 ignored tests) + README Known Limitations |
| **Renaming a type whose uses are member bases.** Renaming an enum type does not update `EnumName.variant` expressions, leaving the project uncompilable. | `compiler_edit_safety.rs::test_rename_enum_type_updates_variant_bases` (ignored) |
| **Hidden `.compact` files are indexed.** `find_compact_files` skips hidden directories but not dot-prefixed files, so a leftover `.compact-lsp-temp-*.compact` can be scanned as a source. | `workspace/tests.rs::test_find_compact_files_skips_hidden_files` (ignored) |
