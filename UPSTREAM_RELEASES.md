# Upstream Compact releases — tracked

How `compact-lsp` follows the upstream Compact toolchain. The goal is to know, at a
glance, **which toolchain is the public-network (mainnet) pin** and **what the LSP must do**
when a new one ships.

- Release notes: <https://docs.midnight.network/relnotes/compact> (append `.md` for raw Markdown)
- Compatibility/support matrix: <https://docs.midnight.network/relnotes/support-matrix.md>
- Public releases: <https://github.com/midnightntwrk/compact/releases>
- Compiler source + authoritative changelog: <https://github.com/LFDT-Minokawa/compact>
  (`CHANGELOG.md`)

## Current target

| | |
|---|---|
| Public-network toolchain | **0.31.1** |
| Language | **0.23.0** |
| Compact runtime | **0.16.0** |
| Ledger | **ledger-8.0.2** |
| LSP registry | tracks 0.23.x source, gated with `min_language_version` |

## Tracked releases

| Toolchain | Date | Language | Runtime | Ledger | Public networks | LSP action |
|---|---|---|---|---|---|---|
| **0.31.1** | 2026-06-25 | **0.23.0** | 0.16.0 | 8 | ✅ Mainnet / Preprod / Preview | **Current target.** `keccak256` gated at 0.23.103 |
| 0.31.0 | 2026-04-29 | 0.23.0 | 0.16.0 | 8 | ✅ (prior pin) | cast-codegen patch in 0.31.1 |
| 0.34.0 | 2026-08-25 | 0.26.0 | 0.19.0 | 9 | ❌ not deployable | `kernel.caller`, `PublicAddress`; needs new grammar |
| 0.35.0 | 2026-09-29 | 0.27.0 | 0.20.0 | 9 | ❌ not deployable | P256/Curve25519/Ed25519/SHA-512; needs new grammar |

> Toolchains 0.32.0/0.33.0 (languages 0.24.0/0.25.0) exist in the compiler source line but
> are not required for the migration; add a row here if they become relevant.

## Process when a new toolchain appears

The scheduled [`upstream-watch`](./.github/workflows/upstream-watch.yml) workflow opens an issue
when a newer `compactc-*` release is found. To resolve it:

1. Read the release notes / changelog; check the **support matrix** for the new public-network pin.
2. Add or update a row in the table above, and bump `.github/upstream-known.txt` to the reviewed
   latest toolchain.
3. If the language version moved, update the registries (`stdlib.toml`, `builtins.toml`) and any
   `min_language_version` gates; check keywords/built-in types in `validation.rs` / `parser/mod.rs`.
4. If the syntax moved, the tree-sitter grammar must be updated first (watch
   `midnightntwrk/compact-tree-sitter`; the current pin is `feat/grammar-v0.30.0`).
5. Run the probe tests against the new toolchain (`compiler-probe` CI job, or locally):
   `COMPACT_COMPILER=... cargo test -p compact-analyzer probe`.
6. Close the issue with a PR.

## Local check

```bash
scripts/check-upstream.sh          # human-readable report
scripts/check-upstream.sh --ci     # also files an issue when newer (used by the workflow)
```
