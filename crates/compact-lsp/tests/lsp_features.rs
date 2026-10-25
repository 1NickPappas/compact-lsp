//! End-to-end coverage for the remaining request handlers.
//!
//! `documentSymbol`, `workspace/symbol` (scan cache and freshly opened
//! buffers), `foldingRange`, `prepareRename`, rename validation, and a
//! semantic-token decoder that maps every token back onto the source text.
//! No compiler is required.

mod common;

use common::{LspClient, copy_fixtures_to, initialize, offset_of, open_and_wait, position_of};
use serde_json::{Value, json};

const SOURCE: &str =
    "import CompactStandardLibrary;\n\nexport circuit alpha(): Uint<64> {\n  return 1;\n}\n";

fn start(client_root_uri: &str) -> LspClient {
    let mut client = LspClient::start();
    initialize(&mut client, client_root_uri);
    client
}

fn virtual_uri(temp: &std::path::Path, name: &str) -> (String, String) {
    let root_uri = format!("file://{}", temp.display());
    (root_uri, format!("file://{}/{name}", temp.display()))
}

fn document_symbols(client: &mut LspClient, uri: &str) -> Value {
    client.request(
        "textDocument/documentSymbol",
        json!({ "textDocument": { "uri": uri } }),
    )
}

fn workspace_symbols(client: &mut LspClient, query: &str) -> Vec<Value> {
    client
        .request("workspace/symbol", json!({ "query": query }))
        .pointer("/result")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default()
}

#[test]
fn test_document_symbols_on_unsaved_document() {
    let temp = tempfile::tempdir().expect("tempdir");
    let (root_uri, uri) = virtual_uri(temp.path(), "unsaved.compact");
    let mut client = start(&root_uri);
    open_and_wait(&mut client, &uri, SOURCE);

    // The file was never written to disk.
    assert!(!std::path::Path::new(uri.strip_prefix("file://").unwrap()).exists());
    let response = document_symbols(&mut client, &uri);
    let text = response.to_string();
    assert!(text.contains("\"alpha\""), "{text}");
    assert!(
        text.contains("\"kind\":12"),
        "circuit kind expected: {text}"
    );

    client.shutdown();
}

#[test]
fn test_workspace_symbols_from_scan_and_open_buffer() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let root_uri = format!("file://{}", root.display());
    let mut client = start(&root_uri);
    client.wait_for_log_message("server ready");

    // From the workspace scan (lib.compact is never opened here).
    let lib_path = std::fs::canonicalize(root.join("portable/lib.compact")).expect("lib path");
    let results = workspace_symbols(&mut client, "Order");
    let text = serde_json::to_string(&results).unwrap();
    assert!(text.contains("\"Order\""), "{text}");
    assert!(
        text.contains(lib_path.to_string_lossy().as_ref()),
        "scan results should point at the canonical lib path: {text}"
    );

    // Case-insensitive substring filtering.
    let results = workspace_symbols(&mut client, "ord");
    assert!(
        results
            .iter()
            .any(|s| s.get("name").and_then(Value::as_str) == Some("Order")),
        "query `ord` should match Order: {results:#?}"
    );
    assert!(
        workspace_symbols(&mut client, "no-such-symbol-anywhere").is_empty(),
        "non-matching query should return nothing"
    );

    // A freshly opened (unsaved) buffer is indexed immediately.
    let (_, unsaved_uri) = virtual_uri(temp.path(), "fresh.compact");
    open_and_wait(
        &mut client,
        &unsaved_uri,
        "export circuit freshlyOpened(): Uint<64> {\n  return 1;\n}\n",
    );
    let results = workspace_symbols(&mut client, "freshlyOpened");
    assert!(
        results
            .iter()
            .any(|s| s.get("name").and_then(Value::as_str) == Some("freshlyOpened")),
        "opened buffer should be indexed: {results:#?}"
    );

    client.shutdown();
}

#[test]
fn test_folding_ranges_on_nested_constructs() {
    let temp = tempfile::tempdir().expect("tempdir");
    let (root_uri, uri) = virtual_uri(temp.path(), "nested.compact");
    let mut client = start(&root_uri);

    let source = "module M {\n  struct S {\n    a: Uint<8>,\n    b: Uint<8>,\n  }\n\n  circuit c(n: Uint<8>): Uint<8> {\n    if (n > 0) {\n      for (const i of 0..n) {\n        n = n;\n      }\n    }\n    return n;\n  }\n}\n";
    open_and_wait(&mut client, &uri, source);

    let response = client.request(
        "textDocument/foldingRange",
        json!({ "textDocument": { "uri": uri } }),
    );
    let ranges = response
        .pointer("/result")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    assert!(
        ranges.len() >= 3,
        "expected several foldable regions: {response}"
    );

    let module_start = position_of(source, "module M").0;
    assert!(
        ranges
            .iter()
            .any(|r| r.get("startLine").and_then(Value::as_u64) == Some(module_start as u64)),
        "module body should fold: {response}"
    );

    client.shutdown();
}

#[test]
fn test_prepare_rename_valid_and_invalid() {
    let temp = tempfile::tempdir().expect("tempdir");
    let (root_uri, uri) = virtual_uri(temp.path(), "rename.compact");
    let mut client = start(&root_uri);
    open_and_wait(&mut client, &uri, SOURCE);

    // On the symbol: a range covering exactly the identifier.
    let (line, column) = position_of(SOURCE, "export circuit alpha");
    let name = column + "export circuit ".len() as u32;
    let response = client.request(
        "textDocument/prepareRename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": name + 1 }
        }),
    );
    let range = response.pointer("/result").cloned().unwrap_or(Value::Null);
    assert_eq!(
        range.pointer("/start/character").and_then(Value::as_u64),
        Some(name as u64),
        "{response}"
    );
    assert_eq!(
        range.pointer("/end/character").and_then(Value::as_u64),
        Some(name as u64 + "alpha".len() as u64),
        "{response}"
    );

    // On whitespace: nothing to rename.
    let response = client.request(
        "textDocument/prepareRename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 1, "character": 0 }
        }),
    );
    assert!(
        response.get("result").is_none_or(Value::is_null),
        "expected null for whitespace: {response}"
    );

    // On a keyword: nothing to rename.
    let (line, column) = position_of(SOURCE, "export circuit");
    let response = client.request(
        "textDocument/prepareRename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": column + 1 }
        }),
    );
    assert!(
        response.get("result").is_none_or(Value::is_null),
        "expected null for a keyword: {response}"
    );

    client.shutdown();
}

#[test]
fn test_rename_rejects_invalid_new_names() {
    let temp = tempfile::tempdir().expect("tempdir");
    let (root_uri, uri) = virtual_uri(temp.path(), "rename.compact");
    let mut client = start(&root_uri);
    open_and_wait(&mut client, &uri, SOURCE);

    let (line, column) = position_of(SOURCE, "export circuit alpha");
    let name = column + "export circuit ".len() as u32;

    for bad in ["1leading", "has space", "circuit", "Uint"] {
        let response = client.request(
            "textDocument/rename",
            json!({
                "textDocument": { "uri": uri },
                "position": { "line": line, "character": name },
                "newName": bad
            }),
        );
        assert_eq!(
            response.pointer("/error/code").and_then(Value::as_i64),
            Some(-32602),
            "`{bad}` should be rejected with InvalidParams: {response}"
        );
    }

    // A valid rename still works.
    let response = client.request(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": name },
            "newName": "beta"
        }),
    );
    assert!(
        response.get("result").is_some_and(|r| !r.is_null()),
        "{response}"
    );

    // Renaming from whitespace is a no-op, not an error.
    let response = client.request(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": 1, "character": 0 },
            "newName": "whatever"
        }),
    );
    assert!(
        response.get("result").is_none_or(Value::is_null),
        "{response}"
    );

    client.shutdown();
}

/// Without `format-compact` both formatting requests degrade to a null
/// result (and a `window/showMessage`), never an error or a crash.
/// With a formatter present the positive path is covered by
/// `compiler_edit_safety.rs`.
#[test]
fn test_formatting_is_null_without_formatter() {
    let temp = tempfile::tempdir().expect("tempdir");
    let (root_uri, uri) = virtual_uri(temp.path(), "format.compact");

    // Hermetic server: no devtools (empty HOME/COMPACT_DIRECTORY), no
    // formatter override, no PATH to find one on.
    let fake_home = temp.path().join("home");
    std::fs::create_dir_all(&fake_home).expect("fake home");
    let fake_home = fake_home.to_string_lossy().into_owned();
    let mut client = LspClient::start_with_env(&[
        ("HOME", &fake_home),
        ("COMPACT_DIRECTORY", ""),
        ("COMPACT_FORMATTER", ""),
        ("COMPACT_COMPILER", ""),
        ("PATH", "/nonexistent"),
    ]);
    initialize(&mut client, &root_uri);
    open_and_wait(&mut client, &uri, SOURCE);

    let formatting = client.request(
        "textDocument/formatting",
        json!({
            "textDocument": { "uri": uri },
            "options": { "tabSize": 2, "insertSpaces": true }
        }),
    );
    assert!(
        formatting.pointer("/result").is_none_or(Value::is_null),
        "formatting should degrade to null: {formatting}"
    );

    let range = client.request(
        "textDocument/rangeFormatting",
        json!({
            "textDocument": { "uri": uri },
            "range": {
                "start": { "line": 0, "character": 0 },
                "end": { "line": 4, "character": 1 }
            },
            "options": { "tabSize": 2, "insertSpaces": true }
        }),
    );
    assert!(
        range.pointer("/result").is_none_or(Value::is_null),
        "range formatting should degrade to null: {range}"
    );

    // The server keeps serving requests.
    let response = client.request("workspace/symbol", json!({ "query": "" }));
    assert!(response.get("result").is_some(), "{response}");

    client.shutdown();
}

#[test]
fn test_semantic_tokens_map_back_to_source() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let root_uri = format!("file://{}", root.display());
    let mut client = start(&root_uri);

    let (uri, source) = common::read_fixture(&root, "portable/token.compact");
    open_and_wait(&mut client, &uri, &source);

    let response = client.request(
        "textDocument/semanticTokens/full",
        json!({ "textDocument": { "uri": uri } }),
    );
    let data = response
        .pointer("/result/data")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    assert!(!data.is_empty(), "no semantic tokens: {response}");

    // Decode deltas into absolute positions.
    let mut decoded = Vec::new();
    let mut line = 0u32;
    let mut character = 0u32;
    let (records, remainder) = data.as_chunks::<5>();
    assert!(remainder.is_empty(), "token data must be 5-field records");
    for fields in records {
        let delta_line = fields[0].as_u64().unwrap_or(0) as u32;
        let delta_start = fields[1].as_u64().unwrap_or(0) as u32;
        let length = fields[2].as_u64().unwrap_or(0) as u32;
        let token_type = fields[3].as_u64().unwrap_or(0);
        let modifiers = fields[4].as_u64().unwrap_or(0);
        line += delta_line;
        character = if delta_line == 0 {
            character + delta_start
        } else {
            delta_start
        };
        decoded.push((line, character, length, token_type, modifiers));
    }

    // Positions must be strictly increasing: this is exactly what the delta
    // encoding (and the server's unsigned subtraction) relies on.
    for pair in decoded.windows(2) {
        assert!(
            (pair[1].0, pair[1].1) > (pair[0].0, pair[0].1),
            "tokens out of order: {:?} then {:?}",
            pair[0],
            pair[1]
        );
    }

    // Every token must slice out of the source with exactly its length.
    for &(line, character, length, _, _) in &decoded {
        let start = offset_of(&source, line, character);
        let end = offset_of(&source, line, character + length);
        let text = &source[start..end];
        assert!(
            !text.is_empty() && !text.contains('\n'),
            "token at {line}:{character} len {length} does not map to source: {text:?}"
        );
    }

    // A known declaration maps to the expected token text.
    let status_start = position_of(&source, "export enum TokenStatus");
    let status_char = status_start.1 + "export enum ".len() as u32;
    let token = decoded
        .iter()
        .find(|t| t.0 == status_start.0 && t.1 == status_char)
        .expect("TokenStatus should be tokenized");
    let start = offset_of(&source, token.0, token.1);
    assert_eq!(&source[start..start + token.2 as usize], "TokenStatus");

    client.shutdown();
}
