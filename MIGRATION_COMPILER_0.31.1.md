# Migration plan — target Compact toolchain 0.31.1 (language 0.23.0)

Status: **in progress** · Base branch: **`dev`** (20 commits ahead of stale `main`)
Owner: compact-lsp maintainers · Source of truth: [`CLAUDE.md`](./CLAUDE.md)

## 1. Objective

Make `compact-lsp` officially target **Compact toolchain 0.31.1** — the compiler pinned
by Mainnet, Preprod, and Preview. That means:

- **Language 0.23.0**, Compact runtime **0.16.0**, **ledger 8**.
- No language or grammar migration is needed (the `dev` branch already adopted the
  0.23-capable grammar). The real work is **compiler discovery** and **reconciling the
  symbol registry with the released mainnet compiler**.

Everything lands on `dev`, which is the active integration line; `main` predates the 0.23
work and must not be used as the base.

## 2. Version matrix (why 0.31.1, and what comes later)

| Toolchain | Language | Runtime | Ledger | Public networks | Notes |
|---|---|---|---|---|---|
| **0.31.0 / 0.31.1** | **0.23.0** | **0.16.0** | **8** | **Mainnet / Preprod / Preview** | 0.31.1 is a cast-codegen patch; language unchanged |
| 0.32.0 | 0.24.0 | 0.17.0 | 8 | — | |
| 0.33.0 | 0.25.0 | 0.18.0 | 8 | — | foreign-field arithmetic, EcdsaRecover in runtime |
| 0.34.0 | 0.26.0 | 0.19.0 | 9 | not deployable | `kernel.caller`, `PublicAddress` |
| 0.35.0 | 0.27.0 | 0.20.0 | 9 | not deployable | P256/Curve25519/Ed25519/SHA-512, dynamic cross-contract calls |

Sources: `docs.midnight.network/relnotes/support-matrix.md`,
`LFDT-Minokawa/compact` `CHANGELOG.md`.

> Compiler crate versions such as `0.31.103` (language `0.23.103`) are **internal build
> versions, not released toolchains**. The registry currently tracks that source line,
> which is why it is ahead of the installed compiler.

## 3. Current state and gaps

1. **Compiler discovery misses the version manager.** `DiagnosticEngine::find_compiler()`
   (`crates/compact-analyzer/src/diagnostics.rs`) checks only `COMPACT_COMPILER`,
   `~/compactc/compactc.bin`, and `compactc.bin` on `PATH`. The devtools install at
   `~/.compact/versions/<ver>/<triple>/compactc.bin` (active symlink
   `~/.compact/bin/compactc`) is never found. The `compact` CLI delegates to the active
   compiler via `compact compile …`.
2. **Registry is ahead of the mainnet compiler.** `dev` targets language **0.23.103**;
   0.31.1 is language **0.23.0**. Symbols added after 0.23.0 are suggested but rejected:
   - `keccak256` (0.23.103) — **probe-confirmed rejected by 0.31.1**: `unbound identifier keccak256`
   - reserved keywords `event`, `log` (0.23.101)
   - reserved keywords `eval`, `arguments` (0.23.102)
   - (`jubjubSchnorrVerify`, `ecNeg` are 0.23.104/105 and are *not* yet in the registries — no action needed.)
3. **No version-aware gating.** Completion/hover/definition present registry symbols
   unconditionally, so a 0.31.1 user sees symbols their compiler rejects.
4. **Grammar pin is a temporary necessity.** `tree-sitter-compact` `feat/grammar-v0.30.0`
   is the only 0.23-capable grammar and is unmerged upstream. Keep the pin and its revert
   note (see `CLAUDE.md`).
5. **`main` is stale.** All 0.23 work lives on `dev`.

**No syntax/AST changes are required for 0.31.1** — grammar and parser already cover
language 0.23.0.

## 4. Plan

### Step 1 — Document the sources of truth
- [x] Fold release-notes URLs, repo list, version matrix, and pins into `CLAUDE.md`. *(done)*
- [x] Create this plan document. *(done)*
- [x] Cross-linked from `NEXT_STEPS.md`.

### Step 2 — Branch ✅ done
- [x] Created `migrate/0.31.1` off `dev`.
- [x] `dev` is the integration base going forward; `main` is stale until Step 9.

### Step 3 — Compiler discovery incl. the version manager ✅ done
- [x] Extend `find_compiler()` search order: `COMPACT_COMPILER` → Compact devtools active
      toolchain `$COMPACT_DIRECTORY/bin/compactc` → `~/compactc/compactc.bin` → `PATH`.
- [x] **Resolve the devtools wrapper correctly.** `~/.compact/bin/compactc` is a *wrapper
      script* that execs `compactc.bin` relative to its own real dir; invoked through the
      `bin/` symlink it looks for a nonexistent `bin/compactc.bin` and fails. `devtools_compiler()`
      canonicalizes the symlink to the versioned toolchain dir and uses the real `compactc.bin`.
- [x] Apply the same resolution to `format-compact` (`FormatterEngine`).
- [x] Surface compiler, language, ledger, and runtime versions in the startup message
      (`server/mod.rs` `initialized`, via `version_info()` / `ledger_version()` / `runtime_version()`).

> Not pursued: shelling out through the `compact` CLI. Resolving `compactc.bin` is enough and
> avoids a second wrapper layer.

### Step 4 — Align the registry with language 0.23.0 ✅ done (version-aware gating)
- [x] Added `LanguageVersion` (`compact-analyzer/src/version.rs`) with lenient parsing
      (`0.35.0 (hash date)` → `0.35.0`), ordering, and `at_least`.
- [x] `DiagnosticEngine::language_version()` returns the parsed version; `server` caches it and
      **disables gating when no compiler is found** (show everything rather than hide blindly).
- [x] Added optional `min_language_version` to `stdlib.toml` / `builtins.toml` entries and
      `*_for(language)` lookup variants used at presentation time (completion, hover, definition,
      signature help).
- [x] Gated `keccak256` at `0.23.103` — the mainnet 0.23.0 compiler rejects it (probe-confirmed).
      `event`/`log`/`eval`/`arguments` were never in the registry, so nothing else needed gating.
- [x] Kept `builtins.toml` / `stdlib.toml` and the duplicated `is_builtin_type` list in lockstep.

The fallback (remove `keccak256` outright) was not needed.

### Step 5 — Verify grammar/parser coverage ✅ done
- [x] The pinned `feat/grammar-v0.30.0` grammar covers 0.23.0 syntax; the probe tests (Step 6)
      confirm the parser and compiler agree on the symbol set. No new fixtures were required.

### Step 6 — Diagnostics compatibility + probe test ✅ done
- [x] `compactc.bin --vscode --skip-zk <file> <tmp>` stderr format is unchanged on 0.31.1.
- [x] Probe tests in `diagnostics.rs` (skip when no compiler is found):
      - `test_probe_compile_stable_stdlib_symbols` — curated registry coverage (stdlib circuits,
        ledger `Counter` methods, kernel `self`) compiles without parse errors or unbound symbols;
      - `test_probe_keccak256_age_gate_consistency` — asserts `keccak256` is rejected below
        language 0.23.103 and accepted at/above it, tying the gate to compiler reality;
      - `test_engine_detection_does_not_panic` — guards detection and asserts the resolved path
        is a real file (not the broken wrapper);
      - `test_resolve_devtools_binary_uses_real_binary` — regression test for the wrapper-vs-symlink
        bug (builds a fake devtools tree with a `bin/compactc` symlink and asserts the real
        `compactc.bin` is chosen).
- [x] Gating unit tests: `test_is_available_gating_logic` (empty/unparseable/older/equal/newer/unknown)
      and `test_stdlib_completion_items_gate_keccak` (the completion handler's actual stdlib-item
      builder omits `keccak256` on 0.23.0 and includes it when no compiler is detected).
- [x] Noted new compiler flags (`--ledger-version`, `--runtime-version`, `--compact-path`,
      `--trace-search`); the startup message now reads ledger/runtime via the first two.

### Step 7 — Docs and configuration alignment ✅ done
- [x] Updated `README.md`, `CLAUDE.md`, `NEXT_STEPS.md`: target = toolchain 0.31.1 /
      language 0.23.0; 0.34/0.35 marked future/not deployable.
- [x] Documented automatic devtools detection; manual `COMPACT_COMPILER` must point at the real
      `.../versions/<ver>/<triple>/compactc.bin`, **not** the `~/.compact/bin/compactc` wrapper.

### Step 8 — CI and gates ✅ done
- [x] `cargo fmt --all --check`, `cargo clippy --all-targets --all-features -- -D warnings`,
      `cargo build`, `cargo test` all pass (126 tests).
- [x] Added a `compiler-probe` CI job that installs devtools + toolchain 0.31.1 and runs the probe
tests (`cargo test -p compact-analyzer probe`). The main `check` job stays compiler-free and fast;
the probe tests skip there when no compiler is present.

### Step 9 — Land
- [ ] Merge `migrate/0.31.1` → `dev`; update `CHANGELOG.md`.
- [ ] Decide whether to promote `dev` → `main` so the released line reflects 0.31.1.
      (Recommended: yes — `main` currently misrepresents the project's language target.)

### Step 10 — Future track (documented, not implemented yet)
- [ ] When public networks move to ledger 9, add language 0.26/0.27 support:
      `sha512`, `Secp256r1`/`Curve25519` point+field types, `secp256r1EcdsaVerify`,
      `ed25519Verify`, `PublicAddress`, `kernel.caller()`.
- [ ] Handle breaking stdlib changes: points no longer nominal aliases;
      `JubjubScalar`/`Secp256k1Base`/`Secp256k1Scalar` must be imported;
      `secp256k1EcdsaRecover` and field `add`/`mul` removed; ZKIR v3 default.
- [ ] Requires an updated `compact-tree-sitter` grammar (none published past
      `feat/grammar-v0.30.0` yet) — track upstream for a new branch.

## 5. Functionality changes we want to support

**For 0.31.1 (now):** none. Language 0.23.0 matches the current grammar/parser; only the
registry must stop advertising symbols the mainnet compiler does not have.

**For 0.34/0.35 (future):**

| Area | Change | Action |
|---|---|---|
| Stdlib additions | `sha512`, P256/Curve25519 point+field types, `secp256r1EcdsaVerify`, `ed25519Verify` | Add to `stdlib.toml`, gate by version |
| Ledger/kernel | `kernel.caller()`, `PublicAddress` | Add to `builtins.toml` |
| Breaking stdlib | points no longer nominal aliases; scalar/base types must be imported; `secp256k1EcdsaRecover` and field `add`/`mul` removed | Update existing entries |
| Reserved words | `event`/`log` (0.23.101), `eval`/`arguments` (0.23.102) | Gate by version |
| Compiler output | `contract-manifest.json`; `contract` subdir recreated; ZKIR v3 default | Informational; no LSP change unless used |
| Version string | now includes commit + date (e.g. `0.35.0-rc.0 (0e7a4b677 2026-09-28)`) | May need parsing robustness in `version_info()` |
| Syntax | dynamic cross-contract calls are a **runtime/TS** change, not Compact syntax | No grammar work for this item |

## 6. Repos to follow

| Repo / source | What to watch |
|---|---|
| `LFDT-Minokawa/compact` | `CHANGELOG.md`, `compiler/standard-library.compact`, `doc/compact-reference.mdx`, `doc/release-notes/toolchain-*.md` |
| `midnightntwrk/compact` | GitHub Releases (binary zips + notes), `prerelease/` |
| `midnightntwrk/compact-tree-sitter` | `feat/grammar-v0.30.0` branch; watch for a new grammar line |
| `docs.midnight.network/relnotes/compact` | Release-notes overview (`.md` suffix) |
| `docs.midnight.network/relnotes/support-matrix.md` | Which toolchain the public networks pin |

## 7. Verification

```bash
# Compiler versions
compact update 0.31.1
~/.compact/versions/0.31.1/aarch64-darwin/compactc.bin --version --language-version
# → 0.31.1 / 0.23.0

# The LSP auto-detects the devtools toolchain. To set it manually, point at the
# real binary (the ~/.compact/bin/compactc wrapper is broken when invoked via its symlink):
COMPACT_COMPILER="$HOME/.compact/versions/0.31.1/aarch64-darwin/compactc.bin" cargo test

# Probe: keccak256 must fail on 0.31.1 (gating sanity check)
printf 'import CompactStandardLibrary;\nexport circuit p(): Bytes<32> { return keccak256(serialize<32>(1)); }\n' > /tmp/probe.compact
~/.compact/versions/0.31.1/aarch64-darwin/compactc.bin --vscode --skip-zk /tmp/probe.compact /tmp/probe_out
# → Exception: ... unbound identifier keccak256

# Full gates
cargo fmt --all --check
cargo clippy --all-targets --all-features -- -D warnings
cargo build
cargo test
```
