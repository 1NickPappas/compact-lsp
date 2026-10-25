//! Golden-file snapshots for version-stable LSP output.
//!
//! These lock in the shape of parser-derived responses (document symbols,
//! folding ranges, decoded semantic tokens) and of hover/member completion on
//! user-declared symbols. Compiler diagnostics are deliberately NOT
//! snapshotted (their wording differs per toolchain; those are asserted
//! structurally in `compiler_diagnostics.rs`), and neither are general
//! completion lists (they include registry symbols gated by the detected
//! language version).
//!
//! Review with `cargo insta review` after intentional output changes.

mod common;

use common::{LspClient, copy_fixtures_to, initialize, open_and_wait, position_of, read_fixture};
use serde_json::{Value, json};

fn start_server(root: &std::path::Path) -> LspClient {
    let mut client = LspClient::start();
    initialize(&mut client, &format!("file://{}", root.display()));
    client
}

fn request_at(client: &mut LspClient, method: &str, uri: &str, line: u32, character: u32) -> Value {
    client.request(
        method,
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": character }
        }),
    )
}

/// Decode LSP semantic-token deltas into absolute token positions.
fn decode_semantic_tokens(response: &Value) -> Vec<Value> {
    let data = response
        .pointer("/result/data")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();

    let mut line = 0u64;
    let mut character = 0u64;
    let mut tokens = Vec::new();
    // `data` is a flat array of 5-field records: deltaLine, deltaStart,
    // length, tokenType, tokenModifiers.
    let (records, _remainder) = data.as_chunks::<5>();
    for fields in records {
        let delta_line = fields[0].as_u64().unwrap_or(0);
        let delta_start = fields[1].as_u64().unwrap_or(0);
        let length = fields[2].as_u64().unwrap_or(0);
        let token_type = fields[3].as_u64().unwrap_or(0);
        let modifiers = fields[4].as_u64().unwrap_or(0);

        line += delta_line;
        character = if delta_line == 0 {
            character + delta_start
        } else {
            delta_start
        };

        tokens.push(json!({
            "line": line,
            "character": character,
            "length": length,
            "tokenType": token_type,
            "modifiers": modifiers,
        }));
    }
    tokens
}

fn labels_of(response: &Value) -> Vec<String> {
    let items = response
        .pointer("/result/items")
        .and_then(Value::as_array)
        .or_else(|| response.pointer("/result").and_then(Value::as_array))
        .cloned()
        .unwrap_or_default();
    items
        .iter()
        .filter_map(|item| {
            item.get("label")
                .and_then(Value::as_str)
                .map(str::to_string)
        })
        .collect()
}

#[test]
fn test_snapshot_document_symbols() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let mut client = start_server(&root);

    // A rich top-level file.
    let (token_uri, token_src) = read_fixture(&root, "portable/token.compact");
    open_and_wait(&mut client, &token_uri, &token_src);
    let response = request_at(&mut client, "textDocument/documentSymbol", &token_uri, 0, 0);
    insta::assert_json_snapshot!("document_symbols_token", response.get("result"));

    // A module file: symbols must nest under the `lib` module.
    let (lib_uri, lib_src) = read_fixture(&root, "portable/lib.compact");
    open_and_wait(&mut client, &lib_uri, &lib_src);
    let response = request_at(&mut client, "textDocument/documentSymbol", &lib_uri, 0, 0);
    insta::assert_json_snapshot!("document_symbols_lib", response.get("result"));

    client.shutdown();
}

#[test]
fn test_snapshot_folding_ranges() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let mut client = start_server(&root);

    let (uri, src) = read_fixture(&root, "portable/token.compact");
    open_and_wait(&mut client, &uri, &src);
    let response = request_at(&mut client, "textDocument/foldingRange", &uri, 0, 0);
    insta::assert_json_snapshot!("folding_ranges_token", response.get("result"));

    client.shutdown();
}

#[test]
fn test_snapshot_semantic_tokens() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let mut client = start_server(&root);

    let (uri, src) = read_fixture(&root, "portable/token.compact");
    open_and_wait(&mut client, &uri, &src);
    let response = client.request(
        "textDocument/semanticTokens/full",
        json!({ "textDocument": { "uri": uri } }),
    );
    insta::assert_json_snapshot!("semantic_tokens_token", decode_semantic_tokens(&response));

    client.shutdown();
}

#[test]
fn test_snapshot_hover() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let mut client = start_server(&root);

    let (uri, src) = read_fixture(&root, "portable/token.compact");
    open_and_wait(&mut client, &uri, &src);

    for (name, needle, offset) in [
        (
            "mintTokens",
            "export circuit mintTokens",
            "export circuit ".len(),
        ),
        (
            "TokenInfo",
            "export struct TokenInfo",
            "export struct ".len(),
        ),
        (
            "TokenStatus",
            "export enum TokenStatus",
            "export enum ".len(),
        ),
        ("identity", "circuit identity", "circuit ".len()),
    ] {
        let (line, column) = position_of(&src, needle);
        let response = request_at(
            &mut client,
            "textDocument/hover",
            &uri,
            line,
            column + offset as u32,
        );
        insta::assert_json_snapshot!(format!("hover_{name}"), response.get("result"));
    }

    client.shutdown();
}

#[test]
fn test_snapshot_member_completion() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let mut client = start_server(&root);

    // `balances.` resolves to the ledger `Map`, so the items are exactly the
    // Map methods (stable across language versions).
    let (uri, src) = read_fixture(&root, "portable/token.compact");
    open_and_wait(&mut client, &uri, &src);
    let (line, column) = position_of(&src, "balances.lookup");
    let dot = column + "balances.".len() as u32;
    let response = client.request(
        "textDocument/completion",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": dot },
            "context": { "triggerKind": 2, "triggerCharacter": "." }
        }),
    );
    let mut labels = labels_of(&response);
    labels.sort();
    insta::assert_json_snapshot!("member_completion_map", labels);

    client.shutdown();
}
