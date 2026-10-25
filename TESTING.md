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

The end-to-end suites share a harness (`tests/common/mod.rs`): spawn the
binary, `open`/`open_and_wait` (waits for the first `publishDiagnostics` so
requests never race the notification), `change`/`change_range`/`change_full`,
`save`, `close`, `wait_for_diagnostics`, `assert_no_diagnostics_for`,
`apply_text_edit(s)`, `apply_workspace_edit_to_disk`, `wait_for_exit`, and an
environment-controlled `start_with_env`.

## CI

`ci.yml` runs three jobs on `main` and `dev`:

- **Check** — fmt, clippy `-D warnings`, release build, full test suite
  (compiler-backed tests skip; the hermetic formatter test still runs).
- **Compiler probe** (`0.31.1`, `0.34.0`, `0.35.0`) — installs the toolchain,
  runs the analyzer registry probes, then the full suite with
  `COMPACT_LSP_REQUIRE_COMPILER=1` and `COMPACT_LSP_REQUIRE_FORMATTER=1`, so
  those legs cannot pass without exercising the real binaries.
- **Coverage** — `cargo llvm-cov` (lcov artifact, report-only for now).

## Property tests and fuzzing

- [proptest](https://proptest-rs.github.io/proptest/) properties cover the
  formatting line diff (lossless roundtrip, ordered/disjoint edits, selection
  invariants), the text-based base parsers, and semantic-token encoding
  (never panics, sorted roundtrip).
- `parser/fuzz_tests.rs` runs a deterministic seeded mutation fuzz over a
  Compact corpus (structural characters, non-ASCII, truncation, insertion)
  through every `ParserEngine` entry point: the assertion is that nothing
  panics, so failures are always reproducible.

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

There are currently no ignored tests: the UTF-16 position encoding and
enum-type rename issues have been fixed. Add a row here if a future known
limitation is pinned that way.
