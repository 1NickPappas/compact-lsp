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

use std::path::{Path, PathBuf};

use compact_analyzer::{DiagnosticEngine, LanguageVersion};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Manifest {
    compiles: Vec<String>,
    #[serde(default)]
    fails: Vec<Failure>,
    #[serde(default)]
    gated: Vec<Gated>,
}

#[derive(Debug, Deserialize)]
struct Failure {
    file: String,
    line: u32,
    character: u32,
    message_contains: String,
}

#[derive(Debug, Deserialize)]
struct Gated {
    file: String,
    min_language_version: String,
    above: String,
    below: String,
    #[serde(default)]
    below_message_contains: Option<String>,
}

fn fixtures_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

fn copy_dir(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).expect("create fixture temp dir");
    for entry in std::fs::read_dir(from).expect("read fixtures") {
        let entry = entry.expect("fixture entry");
        let target = to.join(entry.file_name());
        if entry.file_type().expect("file type").is_dir() {
            copy_dir(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), target).expect("copy fixture");
        }
    }
}

fn read_fixture(root: &Path, relative: &str) -> (String, String) {
    let path = root.join(relative);
    let content = std::fs::read_to_string(&path).expect("read fixture");
    let uri = format!("file://{}", path.display());
    (uri, content)
}

fn assert_compiles(diagnostics: &[lsp_types::Diagnostic], relative: &str) {
    assert!(
        diagnostics.is_empty(),
        "{relative} should compile cleanly, got: {diagnostics:#?}"
    );
}

fn assert_fails(
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

    let manifest =
        std::fs::read_to_string(fixtures_dir().join("expected.toml")).expect("read expected.toml");
    let manifest: Manifest = toml::from_str(&manifest).expect("parse expected.toml");

    let engine = DiagnosticEngine::new();
    let language = engine
        .language_version()
        .expect("detected compiler must report a language version");

    let temp = tempfile::tempdir().expect("tempdir");
    let root = temp.path().join("fixtures");
    copy_dir(&fixtures_dir(), &root);

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
