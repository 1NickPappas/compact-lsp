//! Compiler-backed conformance tests for the fixture corpus.
//!
//! Copies `tests/fixtures/` to a temp directory, compiles every fixture with
//! the detected compiler exactly as the server does (same `DiagnosticEngine`,
//! same feature flags), and asserts the expectations recorded in
//! `fixtures/expected.toml`.
//!
//! Skips when no compiler is available; with `COMPACT_LSP_REQUIRE_COMPILER=1`
//! a missing compiler fails instead.

mod common;

use common::{copy_fixtures_to, load_fixture_manifest, read_fixture};
use compact_analyzer::{DiagnosticEngine, LanguageVersion};

pub fn assert_compiles(diagnostics: &[lsp_types::Diagnostic], relative: &str) {
    assert!(
        diagnostics.is_empty(),
        "{relative} should compile cleanly, got: {diagnostics:#?}"
    );
}

pub fn assert_fails(
    diagnostics: &[lsp_types::Diagnostic],
    relative: &str,
    line: Option<u32>,
    character: Option<u32>,
    message_contains: Option<&str>,
) {
    assert!(
        !diagnostics.is_empty(),
        "{relative} should fail to compile, but produced no diagnostics"
    );
    let matches = diagnostics.iter().any(|d| {
        line.is_none_or(|l| d.range.start.line == l)
            && character.is_none_or(|c| d.range.start.character == c)
            && message_contains.is_none_or(|m| d.message.contains(m))
    });
    assert!(
        matches,
        "{relative} expected error (line={line:?}, char={character:?}, message~{message_contains:?}), got: {diagnostics:#?}"
    );
}

/// The manifest is the machine-readable version of the README's matrix.
#[tokio::test]
async fn test_fixture_corpus_matches_engine() {
    if !common::require_compiler() {
        return;
    }

    let manifest = load_fixture_manifest();
    let engine = DiagnosticEngine::new();
    let language = engine
        .language_version()
        .expect("detected compiler must report a language version");

    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());

    for relative in &manifest.compiles {
        let (uri, content) = read_fixture(&root, relative);
        let diagnostics = engine.diagnose_content(&uri, &content).await;
        assert_compiles(&diagnostics, relative);
    }

    for failure in &manifest.fails {
        let (uri, content) = read_fixture(&root, &failure.file);
        let diagnostics = engine.diagnose_content(&uri, &content).await;
        assert_fails(
            &diagnostics,
            &failure.file,
            Some(failure.line),
            Some(failure.character),
            Some(&failure.message_contains),
        );
    }

    for gated in &manifest.gated {
        let min = LanguageVersion::parse(&gated.min_language_version)
            .unwrap_or_else(|| panic!("bad min_language_version {}", gated.min_language_version));
        let (uri, content) = read_fixture(&root, &gated.file);
        let diagnostics = engine.diagnose_content(&uri, &content).await;
        let expected = if language >= min {
            &gated.above
        } else {
            &gated.below
        };
        match expected.as_str() {
            "compiles" => assert_compiles(&diagnostics, &gated.file),
            "fails" => assert_fails(
                &diagnostics,
                &gated.file,
                None,
                None,
                gated.below_message_contains.as_deref(),
            ),
            other => panic!("unknown expected outcome `{other}`"),
        }
        eprintln!(
            "{}: language {language} vs min {min} -> {expected}",
            gated.file
        );
    }
}
