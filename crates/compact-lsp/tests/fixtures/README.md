# Test fixtures

Real, compiler-validated Compact projects used by the end-to-end suites.
Unlike the inline raw strings in `ccc_integration.rs`, these are written to
disk so the LSP, `compactc`, and `format-compact` all operate on the same
files.

All fixtures are written in the language subset accepted by every supported
toolchain (0.31.1 → 0.35.0) unless a manifest marks them as version-gated.

## Layout

| Directory | Purpose |
|---|---|
| `portable/` | Compiles cleanly on 0.31.1, 0.34.0 and 0.35.0. Exercises modules, plain/prefix/selective imports, enums, structs, ledgers, witnesses, generics, and deliberately messy formatting. |
| `crossfile/` | Cross-contract calls (`ref.transfer(...)`). requires language ≥ 0.26 (toolchain 0.34+); does not compile on the mainnet 0.31.1 pin. |
| `broken/` | One file per error class (parse error, type error, unbound identifier, bad import). Every file must fail to compile. |
| `unicode/` | Valid source with non-ASCII comments/text before symbols, for position-encoding tests. |
| `toolchain/` | Symbols that exist only on newer language versions, for registry-gating tests. |

## Verified behaviour (toolchain → language)

| Fixture | 0.31.1 (0.23.0) | 0.34.0 (0.26.0) | 0.35.0 (0.27.0) |
|---|---|---|---|
| `portable/token.compact` | ok | ok | ok |
| `portable/lib.compact` | ok | ok | ok |
| `portable/imports_plain.compact` | ok | ok | ok |
| `portable/imports_prefix.compact` | ok | ok | ok |
| `portable/imports_selective.compact` | ok | ok | ok |
| `portable/messy.compact` | ok | ok | ok |
| `unicode/comments.compact` | ok | ok | ok |
| `crossfile/token.compact` | error | ok | ok |
| `crossfile/caller.compact` | error | ok | ok |
| `broken/parse_error.compact` | error | error | error |
| `broken/type_error.compact` | error | error | error |
| `broken/unbound_id.compact` | error | error | error |
| `broken/bad_import.compact` | error | error | error |
| `toolchain/newer_symbols.compact` | error (unbound `sha512`) | error (unbound `sha512`) | ok |

`sha512` is added in language 0.26.103, so neither 0.26.0 (0.34.0) nor
0.23.0 (0.31.1) has it. Toolchains older than 0.35.105 also need
`--feature-zkir-v3` for it; `DiagnosticEngine` detects and passes that flag
from the compiler's `--help` output.

Language facts learned while calibrating (keep fixtures in this subset):

- A relative `import "./file"` only works when the imported file contains
  exactly one `module` definition; top-level-only files are rejected with
  `does not contain a (single) module defintion`.
- `new type X = Y` is nominal (returning it as `Y` is a type error); plain
  `type X = Y` is a transparent alias.
- `Counter.increment`/`decrement` take `Uint<16>`; `Map.lookup` and ledger
  writes require `disclose` on circuit parameters.
- A type-parameterized circuit cannot be exported at the top level; export it
  from a module instead.
- `Uint<8> + Uint<8>` widens to `Uint<0..511>`, so declare a wider return type.
- No fixture needs a `pragma language_version` line; the compiler infers it.

## Calibration

`crates/compact-lsp/tests/compiler_fixtures.rs` compiles every fixture with
the detected compiler and asserts the manifest in `expected.toml`. When the
fixtures change, re-run the compiler by hand and update the manifest:

```bash
~/.compact/versions/<ver>/<target>/compactc.bin --vscode --skip-zk <file> <outdir>
```

The tests skip when no compiler is detected unless
`COMPACT_LSP_REQUIRE_COMPILER=1`.
