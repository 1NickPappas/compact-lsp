//! Position-encoding tests: non-ASCII text on the same line as code.
//!
//! LSP positions use UTF-16 code units by default, while tree-sitter reports
//! byte columns (`Point.column`). The server currently passes one for the
//! other, so every range that follows a non-ASCII character on the same line
//! is wrong, and `didChange` positions after an astral character land one
//! character early.
//!
//! The failing cases are pinned as `#[ignore]`d acceptance tests that encode
//! the correct behaviour: they must pass once byte columns are converted to
//! UTF-16 on output and UTF-16 positions to byte offsets on input. The
//! ASCII-only case at the bottom passes today and scopes the bug to lines
//! that actually contain non-ASCII before the symbol.
//!
//! See TESTING.md#known-issues.

mod common;

use common::{
    LspClient, apply_workspace_edit_to_disk, change_range, copy_fixtures_to, initialize,
    open_and_wait, read_fixture, require_compiler,
};
use serde_json::{Value, json};

/// The UTF-16 position of the first occurrence of `needle`, which is what an
/// LSP client sends.
fn utf16_position_of(source: &str, needle: &str) -> (u32, u32) {
    for (line, text) in source.lines().enumerate() {
        if let Some(byte) = text.find(needle) {
            let character = text[..byte].chars().map(char::len_utf16).sum::<usize>() as u32;
            return (line as u32, character);
        }
    }
    panic!("needle `{needle}` not found")
}

fn hover(client: &mut LspClient, uri: &str, line: u32, character: u32) -> Value {
    client.request(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": character }
        }),
    )
}

fn definition(client: &mut LspClient, uri: &str, line: u32, character: u32) -> Value {
    client.request(
        "textDocument/definition",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": character }
        }),
    )
}

fn ascii_result(response: &Value) -> Value {
    response.pointer("/result").cloned().unwrap_or(Value::Null)
}

/// Features on ASCII-only lines keep working even when non-ASCII comments
/// appear elsewhere in the file.
#[test]
fn test_ascii_lines_around_unicode_comments_work() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let root_uri = format!("file://{}", root.display());
    let mut client = LspClient::start();
    initialize(&mut client, &root_uri);

    let (uri, source) = read_fixture(&root, "unicode/comments.compact");
    open_and_wait(&mut client, &uri, &source);

    let (line, column) = utf16_position_of(&source, "export circuit greet");
    let response = hover(
        &mut client,
        &uri,
        line,
        column + "export circuit ".len() as u32,
    );
    let result = ascii_result(&response);
    assert!(!result.is_null(), "hover on `greet` failed: {response}");
    assert!(result.to_string().contains("greet"), "{response}");

    let (line, column) = utf16_position_of(&source, "export enum Mood");
    let response = hover(
        &mut client,
        &uri,
        line,
        column + "export enum ".len() as u32,
    );
    let result = ascii_result(&response);
    assert!(!result.is_null(), "hover on `Mood` failed: {response}");
    assert!(result.to_string().contains("Mood"), "{response}");

    client.shutdown();
}

/// The hover range must start at the UTF-16 column of the declaration, not
/// its byte column.
#[test]
#[ignore = "byte columns are reported as UTF-16 positions (see TESTING.md#known-issues)"]
fn test_hover_range_on_non_ascii_line() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let root_uri = format!("file://{}", root.display());
    let mut client = LspClient::start();
    initialize(&mut client, &root_uri);

    let (uri, source) = read_fixture(&root, "unicode/comments.compact");
    open_and_wait(&mut client, &uri, &source);

    let (line, column) = utf16_position_of(&source, "export circuit emojiCircuit");
    let response = hover(
        &mut client,
        &uri,
        line,
        column + "export circuit ".len() as u32,
    );
    let result = ascii_result(&response);
    assert!(!result.is_null(), "hover failed: {response}");
    assert_eq!(
        result
            .pointer("/range/start/character")
            .and_then(Value::as_u64),
        Some(column as u64),
        "hover range must use the UTF-16 column: {response}"
    );

    client.shutdown();
}

/// A definition target on a non-ASCII line must be reported at its UTF-16
/// column so the editor lands on the identifier.
#[test]
#[ignore = "byte columns are reported as UTF-16 positions (see TESTING.md#known-issues)"]
fn test_definition_targets_utf16_column() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let root_uri = format!("file://{}", root.display());
    let mut client = LspClient::start();
    initialize(&mut client, &root_uri);

    let (uri, source) = read_fixture(&root, "unicode/comments.compact");
    open_and_wait(&mut client, &uri, &source);

    let (line, column) = utf16_position_of(&source, "emojiCircuit");
    let response = definition(&mut client, &uri, line, column + 1);
    let result = ascii_result(&response);
    assert!(!result.is_null(), "definition failed: {response}");
    assert_eq!(
        result
            .pointer("/range/start/character")
            .and_then(Value::as_u64),
        Some(column as u64),
        "definition range must use the UTF-16 column: {response}"
    );

    client.shutdown();
}

/// Rename edits are applied by clients in UTF-16 coordinates, so byte-column
/// ranges corrupt the file.
#[tokio::test]
#[ignore = "byte columns are reported as UTF-16 positions (see TESTING.md#known-issues)"]
async fn test_rename_edit_uses_utf16_columns() {
    if !require_compiler() {
        return;
    }
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let root_uri = format!("file://{}", root.display());
    let mut client = LspClient::start();
    initialize(&mut client, &root_uri);
    client.wait_for_log_message("server ready");

    let (uri, source) = read_fixture(&root, "unicode/comments.compact");
    open_and_wait(&mut client, &uri, &source);

    let (line, column) = utf16_position_of(&source, "emojiCircuit");
    let response = client.request(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": column },
            "newName": "renamedCircuit"
        }),
    );
    let edit = ascii_result(&response);
    assert!(!edit.is_null(), "rename failed: {response}");
    apply_workspace_edit_to_disk(&edit);

    let (_, renamed) = read_fixture(&root, "unicode/comments.compact");
    assert!(
        renamed.contains("/* 😀 */ export circuit renamedCircuit(): Uint<8> {"),
        "rename corrupted the non-ASCII line: {renamed}"
    );

    client.shutdown();
}

/// An incremental change whose position follows an astral character must land
/// where the client asked; a character-index/UTF-16 mix-up inserts one
/// character early and breaks the code.
#[test]
#[ignore = "didChange positions after astral characters are off by one (see TESTING.md#known-issues)"]
fn test_didchange_position_after_astral_char() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root_uri = format!("file://{}", temp.path().display());
    let uri = format!("{root_uri}/astral.compact");

    let source = "export circuit a(): Uint<8> {\n  /* 😀 */ return 1;\n}\n";
    let mut client = LspClient::start();
    initialize(&mut client, &root_uri);
    open_and_wait(&mut client, &uri, source);

    // Insert a complete statement before `return`, addressed by its UTF-16
    // column (one past the emoji's two code units).
    let line = 1;
    let (_, return_column) = utf16_position_of(source, "return");
    change_range(
        &mut client,
        &uri,
        2,
        line,
        return_column,
        line,
        return_column,
        "const x: Uint<8> = 0;\n  ",
    );

    let diagnostics = client.wait_for_diagnostics(&uri);
    let syntax: Vec<&Value> = diagnostics
        .iter()
        .filter(|d| d.get("source").and_then(Value::as_str) == Some("compact-syntax"))
        .collect();
    assert!(
        syntax.is_empty(),
        "inserted at the wrong position, producing syntax errors: {diagnostics:#?}"
    );

    client.shutdown();
}
