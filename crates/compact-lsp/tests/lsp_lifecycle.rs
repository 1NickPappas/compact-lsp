//! Document-lifecycle and protocol-robustness end-to-end tests.
//!
//! Covers `didChange` (incremental and full), `didSave`, `didClose`,
//! `shutdown`, unknown methods, and malformed requests. These tests need no
//! compiler: document state is observed through `documentSymbol`, and
//! diagnostics notifications prove a notification was processed.

mod common;

use std::time::Duration;

use common::{
    LspClient, apply_text_edits, change, change_full, close, copy_fixtures_to, initialize,
    open_and_wait, position_of, read_fixture, save,
};
use serde_json::{Value, json};

const BASE: &str =
    "import CompactStandardLibrary;\n\nexport circuit alpha(): Uint<64> {\n  return 1;\n}\n";

fn start_client(root_uri: &str) -> LspClient {
    let mut client = LspClient::start();
    initialize(&mut client, root_uri);
    client
}

fn symbols_of(client: &mut LspClient, uri: &str) -> String {
    client
        .request(
            "textDocument/documentSymbol",
            json!({ "textDocument": { "uri": uri } }),
        )
        .to_string()
}

/// Apply changes to the local mirror the same way the server does: in order,
/// each range relative to the result of the previous change.
fn apply_in_order(buffer: &mut String, changes: &[Value]) {
    for change in changes {
        *buffer = apply_text_edits(buffer, std::slice::from_ref(change));
    }
}

#[test]
fn test_incremental_change_insert_delete_replace() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root_uri = format!("file://{}", temp.path().display());
    let uri = format!("{root_uri}/main.compact");
    let mut client = start_client(&root_uri);
    open_and_wait(&mut client, &uri, BASE);

    let symbols = symbols_of(&mut client, &uri);
    assert!(symbols.contains("\"alpha\""), "{symbols}");

    // Insert a circuit at end of file.
    let mut buffer = BASE.to_string();
    let insert = json!({
        "range": {
            "start": { "line": 5, "character": 0 },
            "end": { "line": 5, "character": 0 }
        },
        "text": "\nexport circuit beta(): Uint<64> {\n  return 2;\n}\n"
    });
    apply_in_order(&mut buffer, std::slice::from_ref(&insert));
    change(&mut client, &uri, 2, vec![insert.clone()]);
    client.wait_for_diagnostics(&uri);

    let symbols = symbols_of(&mut client, &uri);
    assert!(
        symbols.contains("\"alpha\"") && symbols.contains("\"beta\""),
        "{symbols}"
    );

    // Delete the `alpha` circuit (three lines plus their terminator).
    let (alpha_line, _) = position_of(&buffer, "export circuit alpha");
    let delete = json!({
        "range": {
            "start": { "line": alpha_line, "character": 0 },
            "end": { "line": alpha_line + 3, "character": 0 }
        },
        "text": ""
    });
    apply_in_order(&mut buffer, std::slice::from_ref(&delete));
    change(&mut client, &uri, 3, vec![delete]);
    client.wait_for_diagnostics(&uri);

    let symbols = symbols_of(&mut client, &uri);
    assert!(
        !symbols.contains("\"alpha\""),
        "alpha should be gone: {symbols}"
    );
    assert!(symbols.contains("\"beta\""), "{symbols}");

    // Replace the `beta` identifier in place.
    let (line, column) = position_of(&buffer, "export circuit beta");
    let name_start = column + "export circuit ".len() as u32;
    let replace = json!({
        "range": {
            "start": { "line": line, "character": name_start },
            "end": { "line": line, "character": name_start + "beta".len() as u32 }
        },
        "text": "gamma"
    });
    apply_in_order(&mut buffer, std::slice::from_ref(&replace));
    change(&mut client, &uri, 4, vec![replace]);
    client.wait_for_diagnostics(&uri);

    let symbols = symbols_of(&mut client, &uri);
    assert!(symbols.contains("\"gamma\""), "{symbols}");
    assert!(
        !symbols.contains("\"beta\""),
        "beta should be renamed: {symbols}"
    );

    client.shutdown();
}

#[test]
fn test_multi_change_and_full_resync() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root_uri = format!("file://{}", temp.path().display());
    let uri = format!("{root_uri}/main.compact");
    let mut client = start_client(&root_uri);
    open_and_wait(&mut client, &uri, BASE);

    // Two changes in one notification: rename alpha, then append beta.
    let mut buffer = BASE.to_string();
    let (line, column) = position_of(&buffer, "export circuit alpha");
    let name_start = column + "export circuit ".len() as u32;
    let rename = json!({
        "range": {
            "start": { "line": line, "character": name_start },
            "end": { "line": line, "character": name_start + "alpha".len() as u32 }
        },
        "text": "alphaTwo"
    });
    apply_in_order(&mut buffer, std::slice::from_ref(&rename));

    let (eof_line, _) = position_of(&buffer, "}");
    let append = json!({
        "range": {
            "start": { "line": eof_line + 1, "character": 0 },
            "end": { "line": eof_line + 1, "character": 0 }
        },
        "text": "\nexport circuit beta(): Uint<64> {\n  return 2;\n}\n"
    });
    apply_in_order(&mut buffer, std::slice::from_ref(&append));

    change(&mut client, &uri, 2, vec![rename, append]);
    client.wait_for_diagnostics(&uri);

    let symbols = symbols_of(&mut client, &uri);
    assert!(symbols.contains("\"alphaTwo\""), "{symbols}");
    assert!(symbols.contains("\"beta\""), "{symbols}");
    assert!(!symbols.contains("\"alpha\""), "{symbols}");

    // Full-document resync replaces the whole buffer.
    let resync =
        "import CompactStandardLibrary;\n\nexport circuit omega(): Uint<64> {\n  return 9;\n}\n";
    change_full(&mut client, &uri, 3, resync);
    client.wait_for_diagnostics(&uri);

    let symbols = symbols_of(&mut client, &uri);
    assert!(symbols.contains("\"omega\""), "{symbols}");
    assert!(!symbols.contains("\"alphaTwo\""), "{symbols}");

    client.shutdown();
}

#[test]
fn test_did_save_reindexes_and_republishes_dependents() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = copy_fixtures_to(temp.path());
    let root_uri = format!("file://{}", root.display());
    let mut client = start_client(&root_uri);
    client.wait_for_log_message("server ready");

    let (lib_uri, lib_src) = read_fixture(&root, "portable/lib.compact");
    open_and_wait(&mut client, &lib_uri, &lib_src);

    // Add a circuit to the buffer (not to disk) and save.
    let (line, _) = position_of(&lib_src, "export circuit total");
    let insert = json!({
        "range": {
            "start": { "line": line, "character": 0 },
            "end": { "line": line, "character": 0 }
        },
        "text": "  export circuit extra(): Uint<64> {\n    return 0;\n  }\n\n"
    });
    change(&mut client, &lib_uri, 2, vec![insert]);
    client.wait_for_diagnostics(&lib_uri);

    save(&mut client, &lib_uri);
    // Save republishes for the saved file...
    client.wait_for_diagnostics(&lib_uri);

    // ...and for every file that imports it. Dependents are recorded and
    // republished under their canonical paths (the scan canonicalizes).
    let plain_path = std::fs::canonicalize(root.join("portable/imports_plain.compact"))
        .expect("canonical dependent path");
    let plain_uri = format!("file://{}", plain_path.display());
    client.wait_for_diagnostics(&plain_uri);

    // The save re-indexed the buffer, so workspace symbols see the new circuit.
    let response = client.request("workspace/symbol", json!({ "query": "extra" }));
    assert!(
        response.to_string().contains("\"extra\""),
        "saved buffer was not re-indexed: {response}"
    );

    client.shutdown();
}

#[test]
fn test_did_close_clears_diagnostics_and_cancels_pending() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root_uri = format!("file://{}", temp.path().display());
    let uri = format!("{root_uri}/main.compact");
    let mut client = start_client(&root_uri);
    open_and_wait(&mut client, &uri, BASE);

    // A change schedules the debounced compiler pass...
    change_full(&mut client, &uri, 2, BASE);
    client.wait_for_diagnostics(&uri);

    // ...but closing must abort it and clear diagnostics.
    close(&mut client, &uri);
    let cleared = client.wait_for_diagnostics(&uri);
    assert!(
        cleared.is_empty(),
        "close should clear diagnostics: {cleared:?}"
    );

    client.assert_no_diagnostics_for(&uri, Duration::from_millis(900));
    client.shutdown();
}

#[test]
fn test_shutdown_and_exit() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root_uri = format!("file://{}", temp.path().display());
    let mut client = start_client(&root_uri);

    let response = client.request("shutdown", Value::Null);
    assert!(
        response.get("error").is_none(),
        "shutdown failed: {response}"
    );

    // `shutdown` consumes the client, sends `exit`, and waits for the process.
    client.shutdown();
}

#[test]
fn test_unknown_method_returns_method_not_found() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root_uri = format!("file://{}", temp.path().display());
    let mut client = start_client(&root_uri);

    let response = client.request("textDocument/nonsense", json!({}));
    assert_eq!(
        response.pointer("/error/code").and_then(Value::as_i64),
        Some(-32601),
        "expected MethodNotFound: {response}"
    );

    // The server survives.
    let response = client.request("workspace/symbol", json!({ "query": "" }));
    assert!(response.get("result").is_some(), "{response}");

    client.shutdown();
}

/// Structurally invalid params produce a JSON-RPC error and the server keeps
/// serving requests.
#[test]
fn test_invalid_params_keep_server_alive() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root_uri = format!("file://{}", temp.path().display());
    let mut client = start_client(&root_uri);

    let response = client.request("textDocument/hover", json!({}));
    assert!(
        response.get("error").is_some(),
        "expected an error for missing params: {response}"
    );

    let response = client.request("workspace/symbol", json!({ "query": "" }));
    assert!(
        response.get("result").is_some(),
        "server died after bad params: {response}"
    );

    client.shutdown();
}

/// A frame that is not valid JSON is a fatal transport error for tower-lsp:
/// the server answers with a parse error and closes the connection, then exits
/// cleanly. What matters is that it does not hang or panic.
#[test]
fn test_malformed_json_closes_cleanly() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root_uri = format!("file://{}", temp.path().display());
    let mut client = start_client(&root_uri);

    client.send_raw("{ this is not json");
    // A parse-error response with a null id may arrive first.
    let _ = client.try_recv(Duration::from_millis(500));

    let status = client.wait_for_exit(Duration::from_secs(5));
    let status = status.expect("server did not terminate after a protocol error");
    assert!(status.success(), "server exited abnormally: {status}");
}
