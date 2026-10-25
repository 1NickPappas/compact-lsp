//! Parity between the diagnostics the LSP publishes and the diagnostics the
//! compiler itself produces for the same fixture.
//!
//! For every fixture in `expected.toml`, open it through the real LSP server,
//! capture `textDocument/publishDiagnostics`, and compare the
//! `compactc`-sourced entries with `DiagnosticEngine::diagnose_content` on the
//! same content. The manifest expectations are asserted on the LSP output too.
//!
//! Skips when no compiler is available; with `COMPACT_LSP_REQUIRE_COMPILER=1`
//! a missing compiler fails instead.

mod common;

use common::{
    LspClient, copy_fixtures_to, initialize, load_fixture_manifest, manifest_files, open,
    read_fixture, require_compiler,
};
use compact_analyzer::{DiagnosticEngine, LanguageVersion};
use serde_json::Value;

/// `(line, character, message)` for the `compactc`-sourced diagnostics, sorted.
fn lsp_entries(diagnostics: &[Value]) -> Vec<(u32, u32, String)> {
    let mut entries: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.get("source").and_then(Value::as_str) == Some("compactc"))
        .map(|d| {
            let line = d
                .pointer("/range/start/line")
                .and_then(Value::as_u64)
                .unwrap_or(0) as u32;
            let character = d
                .pointer("/range/start/character")
                .and_then(Value::as_u64)
                .unwrap_or(0) as u32;
            let message = d
                .get("message")
                .and_then(Value::as_str)
                .unwrap_or("")
                .to_string();
            (line, character, message)
        })
        .collect();
    entries.sort();
    entries
}

/// `(line, character, message)` for engine diagnostics, sorted.
fn engine_entries(diagnostics: &[lsp_types::Diagnostic]) -> Vec<(u32, u32, String)> {
    let mut entries: Vec<_> = diagnostics
        .iter()
        .map(|d| {
            (
                d.range.start.line,
                d.range.start.character,
                d.message.clone(),
            )
        })
        .collect();
    entries.sort();
    entries
}

#[tokio::test]
async fn test_lsp_diagnostics_match_engine() {
    if !require_compiler() {
        return;
    }

    let manifest = load_fixture_manifest();
    let engine = DiagnosticEngine::new();
    let language = engine
        .language_version()
        .expect("detected compiler must report a language version");

    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let root_uri = format!("file://{}", root.display());

    let mut client = LspClient::start();
    initialize(&mut client, &root_uri);

    for relative in manifest_files(&manifest) {
        let (uri, content) = read_fixture(&root, &relative);
        open(&mut client, &uri, &content);
        let published = client.wait_for_diagnostics(&uri);
        let published_entries = lsp_entries(&published);

        let engine_diagnostics = engine.diagnose_content(&uri, &content).await;
        let expected_entries = engine_entries(&engine_diagnostics);

        assert_eq!(
            published_entries, expected_entries,
            "LSP/compiler diagnostics parity failed for {relative}"
        );

        // The manifest expectation, asserted on the LSP output.
        if manifest.compiles.contains(&relative) {
            assert!(
                published_entries.is_empty(),
                "{relative} should publish no compiler diagnostics: {published:#?}"
            );
        }
        if let Some(failure) = manifest.fails.iter().find(|f| f.file == relative) {
            assert!(
                published_entries.iter().any(|(line, character, message)| {
                    *line == failure.line
                        && *character == failure.character
                        && message.contains(&failure.message_contains)
                }),
                "{relative} should publish {failure:?}: {published:#?}"
            );
        }
        if let Some(gated) = manifest.gated.iter().find(|g| g.file == relative) {
            let min = LanguageVersion::parse(&gated.min_language_version).unwrap_or_else(|| {
                panic!("bad min_language_version {}", gated.min_language_version)
            });
            let expected = if language >= min {
                &gated.above
            } else {
                &gated.below
            };
            match expected.as_str() {
                "compiles" => assert!(
                    published_entries.is_empty(),
                    "{relative} should publish no compiler diagnostics: {published:#?}"
                ),
                "fails" => assert!(
                    !published_entries.is_empty(),
                    "{relative} should publish a compiler diagnostic"
                ),
                other => panic!("unknown expected outcome `{other}`"),
            }
        }

        eprintln!(
            "{relative}: {} compiler diagnostic(s)",
            published_entries.len()
        );
    }

    client.shutdown();
}

/// `didOpen` must diagnose the in-memory buffer, not the file on disk: an
/// editor can open a dirty buffer whose saved copy is valid.
#[tokio::test]
async fn test_open_diagnostics_use_buffer_not_disk() {
    if !require_compiler() {
        return;
    }

    let temp = tempfile::tempdir().expect("tempdir");
    let path = temp.path().join("dirty.compact");
    let valid =
        "import CompactStandardLibrary;\n\nexport circuit ok(): Uint<8> {\n  return 1;\n}\n";
    let invalid = "import CompactStandardLibrary;\n\nexport circuit ok(): Uint<8> {\n  return noSuchSymbol;\n}\n";
    std::fs::write(&path, valid).expect("write valid disk copy");

    let uri = format!("file://{}", path.display());
    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", temp.path().display()));
    open(&mut client, &uri, invalid);

    let published = client.wait_for_diagnostics(&uri);
    assert!(
        lsp_entries(&published)
            .iter()
            .any(|(_, _, message)| message.contains("unbound identifier noSuchSymbol")),
        "diagnostics should come from the buffer, not the valid disk copy: {published:#?}"
    );

    client.shutdown();
}
