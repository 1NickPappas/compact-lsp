//! Edit-safety guarantee: nothing the LSP edits may break compilable code.
//!
//! For each scenario: compile the fixture with the real compiler (before),
//! drive rename/formatting through the LSP, apply the returned edits to the
//! on-disk project, then recompile (after) and require success. Range
//! formatting additionally asserts that everything outside the selected range
//! is byte-identical.
//!
//! Each test works on a fresh copy of the corpus and its own server, so edits
//! from one scenario never invalidate another scenario's cached buffers (a real
//! client re-syncs via didSave/didChange; here isolation is simpler).
//!
//! Rename tests need a compiler; formatting tests need `format-compact`. Both
//! skip when unavailable unless the matching `COMPACT_LSP_REQUIRE_*` env var
//! is set.

mod common;

use std::path::Path;

use common::{
    LspClient, apply_text_edits, apply_workspace_edit_to_disk, copy_fixtures_to, initialize,
    open_and_wait, position_of, read_fixture, require_compiler, require_formatter,
};
use compact_analyzer::{DiagnosticEngine, FormatterEngine};
use serde_json::{Value, json};

/// Boot the server on a fresh copy of the corpus, after the workspace scan.
fn start_client(root: &Path) -> LspClient {
    let root_uri = format!("file://{}", root.display());
    let mut client = LspClient::start();
    initialize(&mut client, &root_uri);
    client.wait_for_log_message("server ready");
    client
}

async fn assert_compiles(engine: &DiagnosticEngine, root: &Path, relative: &str) {
    let (uri, content) = read_fixture(root, relative);
    let diagnostics = engine.diagnose_content(&uri, &content).await;
    assert!(
        diagnostics.is_empty(),
        "{relative} no longer compiles after the LSP edit: {diagnostics:#?}"
    );
}

fn rename_request(uri: &str, line: u32, character: u32, new_name: &str) -> Value {
    json!({
        "textDocument": { "uri": uri },
        "position": { "line": line, "character": character },
        "newName": new_name
    })
}

#[tokio::test]
async fn test_rename_struct_keeps_project_compiling() {
    if !require_compiler() {
        return;
    }
    let engine = DiagnosticEngine::new();
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());

    // Before: the untouched fixtures compile.
    assert_compiles(&engine, &root, "portable/token.compact").await;
    assert_compiles(&engine, &root, "portable/lib.compact").await;

    let mut client = start_client(&root);

    // 1. Same-file rename of a struct name used in a type position.
    let (token_uri, token_src) = read_fixture(&root, "portable/token.compact");
    open_and_wait(&mut client, &token_uri, &token_src);
    let (line, column) = position_of(&token_src, "export struct TokenInfo");
    let response = client.request(
        "textDocument/rename",
        rename_request(
            &token_uri,
            line,
            column + "export struct ".len() as u32,
            "TokenData",
        ),
    );
    let edit = response.get("result").cloned().unwrap_or(Value::Null);
    assert!(!edit.is_null(), "rename returned no edit: {response}");
    let changed = apply_workspace_edit_to_disk(&edit);
    assert!(!changed.is_empty(), "rename produced no file changes");
    assert_eq!(
        changed.len(),
        1,
        "rename emitted the same physical file more than once: {changed:#?}"
    );
    assert_compiles(&engine, &root, "portable/token.compact").await;

    // 2. Cross-file rename of a struct exported from a module and imported by
    //    plain, prefixed, and selective imports.
    let (lib_uri, lib_src) = read_fixture(&root, "portable/lib.compact");
    open_and_wait(&mut client, &lib_uri, &lib_src);
    let (line, column) = position_of(&lib_src, "export struct Order");
    let response = client.request(
        "textDocument/rename",
        rename_request(
            &lib_uri,
            line,
            column + "export struct ".len() as u32,
            "Purchase",
        ),
    );
    let edit = response.get("result").cloned().unwrap_or(Value::Null);
    assert!(!edit.is_null(), "rename returned no edit: {response}");
    let changed = apply_workspace_edit_to_disk(&edit);
    assert_eq!(
        changed.len(),
        4,
        "rename should touch lib + the three importers: {changed:#?}"
    );

    for relative in [
        "portable/lib.compact",
        "portable/imports_plain.compact",
        "portable/imports_prefix.compact",
        "portable/imports_selective.compact",
    ] {
        assert_compiles(&engine, &root, relative).await;
    }

    client.shutdown();
}

/// Renaming a selective-import element rewrites only the element; the `as`
/// alias and its usages must stay untouched.
#[tokio::test]
async fn test_rename_selective_element_preserves_alias() {
    if !require_compiler() {
        return;
    }
    let engine = DiagnosticEngine::new();
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    assert_compiles(&engine, &root, "portable/lib.compact").await;

    let mut client = start_client(&root);
    let (lib_uri, lib_src) = read_fixture(&root, "portable/lib.compact");
    open_and_wait(&mut client, &lib_uri, &lib_src);

    let (line, column) = position_of(&lib_src, "export circuit total");
    let response = client.request(
        "textDocument/rename",
        rename_request(
            &lib_uri,
            line,
            column + "export circuit ".len() as u32,
            "sum",
        ),
    );
    let edit = response.get("result").cloned().unwrap_or(Value::Null);
    assert!(!edit.is_null(), "rename returned no edit: {response}");
    let changed = apply_workspace_edit_to_disk(&edit);
    assert_eq!(
        changed.len(),
        4,
        "rename should touch lib + the three importers: {changed:#?}"
    );

    let (_, selective) = read_fixture(&root, "portable/imports_selective.compact");
    assert!(
        selective.contains("import { Order, sum as totalOf } from \"./lib\";"),
        "selective element not renamed or alias disturbed: {selective}"
    );
    assert!(
        selective.contains("return totalOf(order);"),
        "alias usage must stay untouched: {selective}"
    );

    for relative in [
        "portable/lib.compact",
        "portable/imports_plain.compact",
        "portable/imports_prefix.compact",
        "portable/imports_selective.compact",
    ] {
        assert_compiles(&engine, &root, relative).await;
    }

    client.shutdown();
}

/// Renaming a type whose uses are member-access bases (`EnumName.variant`)
/// currently leaves those bases untouched, so the renamed project stops
/// compiling. This is the acceptance test for closing that gap.
#[tokio::test]
#[ignore = "renaming an enum type does not update `EnumName.variant` bases"]
async fn test_rename_enum_type_updates_variant_bases() {
    if !require_compiler() {
        return;
    }
    let engine = DiagnosticEngine::new();
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    assert_compiles(&engine, &root, "portable/token.compact").await;

    let mut client = start_client(&root);
    let (token_uri, token_src) = read_fixture(&root, "portable/token.compact");
    open_and_wait(&mut client, &token_uri, &token_src);
    let (line, column) = position_of(&token_src, "export enum TokenStatus");
    let response = client.request(
        "textDocument/rename",
        rename_request(
            &token_uri,
            line,
            column + "export enum ".len() as u32,
            "TokenPhase",
        ),
    );
    let edit = response.get("result").cloned().unwrap_or(Value::Null);
    assert!(!edit.is_null(), "rename returned no edit: {response}");
    apply_workspace_edit_to_disk(&edit);
    assert_compiles(&engine, &root, "portable/token.compact").await;

    client.shutdown();
}

#[tokio::test]
async fn test_whole_document_formatting_keeps_project_compiling() {
    if !(require_compiler() && require_formatter()) {
        return;
    }
    let engine = DiagnosticEngine::new();
    let formatter = FormatterEngine::new();
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    assert_compiles(&engine, &root, "portable/messy.compact").await;

    let mut client = start_client(&root);
    let (uri, source) = read_fixture(&root, "portable/messy.compact");
    open_and_wait(&mut client, &uri, &source);

    let response = client.request(
        "textDocument/formatting",
        json!({
            "textDocument": { "uri": uri },
            "options": { "tabSize": 2, "insertSpaces": true }
        }),
    );
    let edits: Vec<Value> = response
        .pointer("/result")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    assert!(
        !edits.is_empty(),
        "formatting returned no edits: {response}"
    );

    let applied = apply_text_edits(&source, &edits);
    let expected = formatter.format(&source).await.expect("format-compact");
    assert_eq!(
        applied, expected,
        "applied LSP formatting edits differ from format-compact output"
    );

    let path = uri.strip_prefix("file://").expect("file uri");
    std::fs::write(path, &applied).expect("write formatted fixture");
    assert_compiles(&engine, &root, "portable/messy.compact").await;

    client.shutdown();
}

#[tokio::test]
async fn test_range_formatting_touches_only_selection() {
    if !(require_compiler() && require_formatter()) {
        return;
    }
    let engine = DiagnosticEngine::new();
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    assert_compiles(&engine, &root, "portable/messy.compact").await;

    let mut client = start_client(&root);
    let (uri, source) = read_fixture(&root, "portable/messy.compact");
    open_and_wait(&mut client, &uri, &source);

    let lines: Vec<&str> = source.lines().collect();
    let start = lines
        .iter()
        .position(|l| l.contains("export circuit  sum"))
        .expect("sum circuit");
    let end = lines
        .iter()
        .skip(start)
        .position(|l| l.trim() == "}")
        .expect("closing brace")
        + start;

    let response = client.request(
        "textDocument/rangeFormatting",
        json!({
            "textDocument": { "uri": uri },
            "range": {
                "start": { "line": start, "character": 0 },
                "end": { "line": end + 1, "character": 0 }
            },
            "options": { "tabSize": 2, "insertSpaces": true }
        }),
    );
    let edits: Vec<Value> = response
        .pointer("/result")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    assert!(
        !edits.is_empty(),
        "range formatting returned no edits: {response}"
    );

    let applied = apply_text_edits(&source, &edits);
    assert_ne!(applied, source, "range formatting changed nothing");

    // Everything before and after the selected `sum` circuit must be untouched.
    let prefix_end = source
        .find("export circuit  sum")
        .expect("sum circuit offset");
    assert_eq!(
        &applied[..prefix_end],
        &source[..prefix_end],
        "range formatting changed text before the selection"
    );
    let suffix_start = source
        .find("export circuit pick")
        .expect("pick circuit offset");
    let applied_suffix_start = applied
        .find("export circuit pick")
        .expect("pick circuit offset in formatted text");
    assert_eq!(
        &applied[applied_suffix_start..],
        &source[suffix_start..],
        "range formatting changed text after the selection"
    );

    let path = uri.strip_prefix("file://").expect("file uri");
    std::fs::write(path, &applied).expect("write range-formatted fixture");
    assert_compiles(&engine, &root, "portable/messy.compact").await;

    client.shutdown();
}
