//! Unit tests for the diagnostics module.

use super::*;

#[test]
fn test_engine_detection_does_not_panic() {
    // Detection depends on the host; this simply guards the search logic.
    let engine = DiagnosticEngine::new();
    let _ = engine.is_available();
    let _ = engine.version_info();
    let _ = engine.language_version();
    let _ = engine.ledger_version();
    let _ = engine.runtime_version();

    // A detected devtools compiler must be an executable file. This guards
    // against regressing to the broken `~/.compact/bin/compactc` wrapper.
    if let Some(path) = engine.compiler_path.as_deref() {
        assert!(
            std::path::Path::new(path).is_file(),
            "detected compiler path is not a file: {path}"
        );
    }
}

/// Run the detected compiler over `source` and return its combined stderr.
///
/// Returns `None` when no compiler is available, so callers skip.
fn compile_probe(engine: &DiagnosticEngine, source: &str) -> Option<String> {
    let compiler = engine.compiler_path.as_ref()?;
    let dir = tempfile::tempdir().ok()?;
    let source_path = dir.path().join("probe.compact");
    std::fs::write(&source_path, source).ok()?;

    let output = std::process::Command::new(compiler)
        .arg("--vscode")
        .arg("--skip-zk")
        .args(engine.feature_flags())
        .arg(&source_path)
        .arg(dir.path())
        .output()
        .ok()?;

    Some(String::from_utf8_lossy(&output.stderr).to_string())
}

/// Probe-compile stable stdlib symbols; they must not be reported as unbound
/// on any supported compiler. Skips when no compiler is available.
#[test]
fn test_probe_compile_stable_stdlib_symbols() {
    let engine = DiagnosticEngine::new();
    if !engine.is_available() {
        eprintln!("skipping probe: no compactc detected");
        return;
    }

    // Curated coverage of the registry: stdlib circuits (incl. a generic
    // with a helper), ledger ADT methods, and a kernel method. Compiles
    // cleanly on 0.23.0, so a `parse error` here means name resolution
    // never ran and the assertions below would be meaningless.
    let source = r#"
import CompactStandardLibrary;

ledger counter: Counter;

export circuit probeHash(x: Field): Bytes<32> {
    return persistentHash<Field>(x);
}

export circuit probeCommit(x: Field): Bytes<32> {
    return persistentCommit<Field>(x, pad(32, "probe"));
}

export circuit probeSome(x: Field): Maybe<Field> {
    return some<Field>(x);
}

export circuit probeEither(x: Field): Either<Field, Field> {
    return left<Field, Field>(x);
}

export circuit probeCounter(): Uint<64> {
    counter.increment(1);
    return counter.read();
}

export circuit probeKernel(): ContractAddress {
    return kernel.self();
}
"#;

    let stderr = match compile_probe(&engine, source) {
        Some(s) => s,
        None => return,
    };

    assert!(
        !stderr.contains("parse error"),
        "probe did not parse; name-resolution assertions are not meaningful:\n{stderr}"
    );
    for symbol in [
        "persistentHash",
        "persistentCommit",
        "some",
        "left",
        "Counter",
        "kernel",
    ] {
        assert!(
            !stderr.contains(&format!("unbound identifier {symbol}")),
            "stable stdlib symbol `{symbol}` reported unbound:\n{stderr}"
        );
    }
}

/// The reason `keccak256` is version-gated: on a compiler older than language
/// 0.23.103 (the mainnet 0.31.1 / 0.23.0 toolchain) it is an unbound identifier.
#[test]
fn test_probe_keccak256_age_gate_consistency() {
    let engine = DiagnosticEngine::new();
    if !engine.is_available() {
        eprintln!("skipping probe: no compactc detected");
        return;
    }
    let Some(language) = engine.language_version() else {
        return;
    };

    let source = r#"
import CompactStandardLibrary;

export circuit probe(x: Field): Bytes<32> {
    return keccak256<Field>(x);
}
"#;

    let stderr = match compile_probe(&engine, source) {
        Some(s) => s,
        None => return,
    };
    let rejected = stderr.contains("unbound identifier keccak256");

    if language < LanguageVersion::new(0, 23, 103) {
        assert!(
            rejected,
            "language {language} should reject keccak256 (gated in the registry):\n{stderr}"
        );
    } else {
        assert!(
            !rejected,
            "language {language} should accept keccak256 (not gated):\n{stderr}"
        );
    }
}

/// Newer stdlib symbols must be accepted by compilers at or above the
/// language version where the registry exposes them. The probe uses the
/// detected feature flags (e.g. `--feature-zkir-v3`), which the foreign
/// curve types require on toolchains 0.34.100–0.35.104.
#[test]
fn test_probe_version_gated_new_symbols() {
    let engine = DiagnosticEngine::new();
    if !engine.is_available() {
        eprintln!("skipping probe: no compactc detected");
        return;
    }
    let Some(language) = engine.language_version() else {
        return;
    };

    // (symbol, source, first language version that has it)
    let cases: &[(&str, &str, LanguageVersion)] = &[
        (
            "jubjubSchnorrVerify",
            r#"
import CompactStandardLibrary;

// Message length 1 is a placeholder; the point is name resolution.
export circuit probe(msg: Vector<1, Field>, sig: JubjubSchnorrSignature, pk: JubjubPoint): Boolean {
    return jubjubSchnorrVerify<1>(msg, sig, pk);
}
"#,
            LanguageVersion::new(0, 23, 104),
        ),
        (
            "sha512",
            r#"
import CompactStandardLibrary;

export circuit probe(x: Field): Bytes<64> {
    return sha512<Field>(x);
}
"#,
            LanguageVersion::new(0, 26, 103),
        ),
        (
            "kernel.caller",
            r#"
import CompactStandardLibrary;

export circuit probe(): Maybe<PublicAddress> {
    return kernel.caller();
}
"#,
            LanguageVersion::new(0, 26, 105),
        ),
        (
            "secp256r1EcdsaVerify",
            r#"
import CompactStandardLibrary;

export circuit probe(msg: Bytes<32>, sig: Secp256r1EcdsaSignature, pk: Secp256r1Point): Boolean {
    return secp256r1EcdsaVerify(msg, sig, pk);
}
"#,
            LanguageVersion::new(0, 26, 106),
        ),
        (
            "ed25519Verify",
            r#"
import CompactStandardLibrary;

export circuit probe(msg: Bytes<3>, sig: Ed25519Signature, pk: Curve25519Point): Boolean {
    return ed25519Verify<3>(msg, sig, pk);
}
"#,
            LanguageVersion::new(0, 26, 106),
        ),
    ];

    for (symbol, source, min) in cases {
        if language < *min {
            continue;
        }
        let stderr = match compile_probe(&engine, source) {
            Some(s) => s,
            None => return,
        };
        assert!(
            !stderr.contains("parse error"),
            "probe for {symbol} did not parse on language {language}:\n{stderr}"
        );
        assert!(
            !stderr.contains("unbound identifier"),
            "language {language} (>= {min}) should accept `{symbol}`:\n{stderr}"
        );
    }
}

#[cfg(unix)]
#[test]
fn test_resolve_devtools_binary_uses_real_binary() {
    use std::os::unix::fs::symlink;

    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    let version_dir = root.join("versions").join("0.31.1").join("triple");
    std::fs::create_dir_all(&version_dir).expect("version dir");

    // The version dir holds the real binary plus the wrapper script.
    let real_bin = version_dir.join("compactc.bin");
    std::fs::write(&real_bin, b"real").expect("real bin");
    std::fs::write(version_dir.join("compactc"), b"#!/bin/sh\n").expect("wrapper");

    let bin = root.join("bin");
    std::fs::create_dir_all(&bin).expect("bin dir");
    symlink(version_dir.join("compactc"), bin.join("compactc")).expect("symlink");

    // Must resolve past the wrapper symlink to the real binary, not the
    // broken `bin/compactc` path. `tempdir` paths are themselves symlinked
    // on macOS, so compare against the canonical path.
    let expected_bin = std::fs::canonicalize(&real_bin)
        .expect("canonical real bin")
        .to_string_lossy()
        .into_owned();
    assert_eq!(
        resolve_devtools_binary(root, "compactc", "compactc.bin"),
        Some(expected_bin)
    );

    // A missing entry resolves to None.
    assert_eq!(resolve_devtools_binary(root, "nope", "nope.bin"), None);

    // When no `<real_name>` exists, the resolved target is used as-is
    // (format-compact is symlinked straight to the real binary).
    let real_format = version_dir.join("format-compact");
    std::fs::write(&real_format, b"real").expect("format bin");
    symlink(
        version_dir.join("format-compact"),
        bin.join("format-compact"),
    )
    .expect("format symlink");
    let expected_format = std::fs::canonicalize(&real_format)
        .expect("canonical real format")
        .to_string_lossy()
        .into_owned();
    assert_eq!(
        resolve_devtools_binary(root, "format-compact", "format-compact"),
        Some(expected_format)
    );
}

#[test]
fn test_parse_error_line() {
    let line = "Exception: broken.compact line 1 char 1: parse error: found \"this\" looking for a program element or end of file";
    let diag = parse_error_line(line).expect("Should parse error line");

    assert_eq!(diag.range.start.line, 0); // 1-indexed -> 0-indexed
    assert_eq!(diag.range.start.character, 0);
    assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
    assert!(diag.message.contains("parse error"));
    assert_eq!(diag.source, Some("compactc".to_string()));
}

#[test]
fn test_parse_type_error() {
    let line = "Exception: broken3.compact line 4 char 3: mismatch between actual return type Bytes<12> and declared return type Uint<64> of circuit test";
    let diag = parse_error_line(line).expect("Should parse type error");

    assert_eq!(diag.range.start.line, 3); // line 4 -> index 3
    assert_eq!(diag.range.start.character, 2); // char 3 -> index 2
    assert!(diag.message.contains("mismatch"));
}

#[test]
fn test_parse_invalid_line() {
    let line = "This is not an error line";
    assert!(parse_error_line(line).is_none());
}
